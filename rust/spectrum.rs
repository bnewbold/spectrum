/*
 * A partial Scheme/LISP implementation in Rust. Build and run with:
 *
 *  rustc spectrum.rs -o spectrum
 *  ./spectrum
 *
 * Intentended to work with Rust 1.8 (stable from Spring 2016)
 */

use std::io;
use std::io::Write;
use std::collections::HashMap;

//////////// Types and Constants

// TODO: how to avoid the '34' here?
const SCHEME_BUILTINS: [&'static str; 34] = [
    "lambda", "quote", "cond", "else", "display",
    "define", "set!",
    "cons", "car", "cdr",
    "boolean?", "symbol?", "procedure?", "pair?", "number?", "string?", "null?", "atom?", "zero?",
    "eq?",
    "=", ">", ">=", "<", "<=",
    "+", "-", "*", "/",
    "exp", "log", "sin", "cos", "tan",
    ];

// The SchemeExpr type is basically the complete AST.
// There doesn't seem to be a symbol or quote type in Rust, so i'm using typed strings.
#[derive(Clone, PartialEq)]
enum SchemeExpr {
    SchemeNull,
    SchemeTrue,
    SchemeFalse,
    SchemeNum(f64),
    SchemeBuiltin(String),
    SchemeSymbol(String),
    SchemeIdentifier(String),
    SchemeStr(String),
    SchemeProcedure(
        Vec<String>,
        Vec<SchemeExpr>,
        HashMap<String, SchemeExpr>),
    SchemeList(Vec<SchemeExpr>),
    SchemeQuote(Vec<SchemeExpr>),
}

//////////// Lexing, Parsing, and Printing

fn is_scheme_whitespace(c: char) -> bool{
    " \r\n".find(c) != None
}

fn is_scheme_sep(c: char) -> bool {
    "()".find(c) != None
}

fn is_valid_identifier(s: &str) -> bool {
    // TODO: this could be an 'any' or 'filter' call?
    if s.len() == 0 {
        return false;
    }
    if s.starts_with("-") || s.ends_with("-") {
        return false;
    }
    for c in s.chars() {
        if !(c.is_alphabetic() || c == '-') {
            return false;
        }
    }
    return true;
}

/*
 * This function takes a raw string and splits it up into a flat sequence of string tokens.
 * It should handle basic quotes (double quotes only) and comments (';' character to end-of-line).
 */
fn scheme_tokenize<'a>(raw_str: &'a str) -> Result<Vec<&'a str>, String> {
    let mut ret = Vec::<&str>::new();
    let mut food: usize = 0;    // "how many chars of current token have we read?"
    let mut quoted: bool = false;
    let mut commented: bool = false;
    for (i, c) in raw_str.chars().enumerate() {
        if quoted {
            // Safe to look-back a character here because quoted can't be true for first char
            if c == '"' && raw_str.chars().collect::<Vec<char>>()[i-1] != '\\' {
                ret.push(&raw_str[i-food-1..i+1]);
                quoted = false;
                food = 0;
            } else if raw_str.len() == i+1 {
                return Err(format!("unmatched quote char"));
            } else {
                food += 1;
            }
        } else if commented {
            if c == '\n' {
                commented = false;
            }
        } else if c == ';' {
            if food > 0 {
                ret.push(&raw_str[i-food..i]);
            }
            commented = true;
            food = 0;
        } else if c == '"' {
            if food > 0 {
                return Err(format!("unexpected quote char"));
            }
            quoted = true;
        } else if is_scheme_whitespace(c) || is_scheme_sep(c) {
            if food > 0 {
                ret.push(&raw_str[i-food..i]);
            }
            if is_scheme_sep(c) {
                ret.push(&raw_str[i..i+1]);
            }
            food = 0;
        } else if raw_str.len() == i+1 {
            // end of input
            ret.push(&raw_str[i-food..]);
        } else {
            food += 1;
        }
    }
    if quoted {
        return Err(format!("unmatched (trailing) quote char"));
    }
    return Ok(ret);
}

/*
 * This function takes a token (still a string) and parses it into a single SchemeExpression
 */
fn scheme_parse_token(token: &str) -> Result<SchemeExpr, String> {

    // Is it a constant?
    match token {
        "#t" => return Ok(SchemeExpr::SchemeTrue),
        "#f" => return Ok(SchemeExpr::SchemeFalse),
        _ => ()
    }

    // Is it a builtin?
    if SCHEME_BUILTINS.contains(&token) {
        return Ok(SchemeExpr::SchemeBuiltin(token.to_string()));
    }

    // Try to parse as a number
    match token.parse::<f64>() {
        Ok(x) => return Ok(SchemeExpr::SchemeNum(x)),
        Err(_) => ()
    }

    // Is it a string?
    if token.starts_with("\"") && token.ends_with("\"") {
        return Ok(SchemeExpr::SchemeStr(token.to_string()));
    }

    // Is it a symbol?
    if token.starts_with("'") && is_valid_identifier(&token[1..]) {
        return Ok(SchemeExpr::SchemeSymbol(token.to_string()));
    }

    // Else, we'll treat it as an identifier
    if is_valid_identifier(token) {
        return Ok(SchemeExpr::SchemeIdentifier(token.to_string()));
    }

    return Err(format!("unparsable token: \"{}\"", token));
}

/*
 * This function takes a flat sequence of string tokens (as output by scheme_tokenize) and parses
 * into a SchemeExpression (eg, a nested list of expressions).
 */
fn scheme_parse(tokens: &Vec<&str>, depth: u32) -> Result<(Vec<SchemeExpr>, usize), String> {
    let mut i: usize = 0;
    if tokens.len() == 0  {
        return Ok((vec![SchemeExpr::SchemeNull], 0));
    } else if tokens.len() == 1 {
        let expr = try!(scheme_parse_token(tokens[0]));
        return Ok((vec![expr], 1));
    }
    let mut parsed: usize = 0;
    let mut ret = Vec::<SchemeExpr>::new();
    while i < tokens.len() {
        parsed += 1;
        match tokens[i] {
            "(" => {
                // "Read ahead" to check for empty tuple
                if i+1 < tokens.len() && tokens[i+1] == ")" {
                    ret.push(SchemeExpr::SchemeNull);
                    i += 1;
                    parsed += 1;
                } else {
                    let (expr_list, skip) = try!(scheme_parse(&tokens[i+1..].to_vec(), depth+1));
                    i += skip;
                    parsed += skip;
                    ret.push(SchemeExpr::SchemeList(expr_list));
                }
            },
            ")" => {
                if depth == 0 {
                    return Err(format!("missing an open bracket"));
                }
                return Ok((ret, parsed));
            },
            token => {
                let expr = try!(scheme_parse_token(token));
                ret.push(expr);
            }
        }
        i += 1;
    }
    if depth > 0 {
        return Err(format!("missing a close bracket"));
    }
    return Ok((ret, parsed));
}

/*
 * This function takes an arbitary SchemeExpression and returns a string representation.
 * It's basically the inverse of scheme_tokenize and scheme_parse; the output representation is
 * just plain old LISP/Scheme s-expr syntax.
 */
fn scheme_repr(ast: &SchemeExpr) -> Result<String, String> {
    return match ast {
        &SchemeExpr::SchemeTrue => Ok("#t".to_string()),
        &SchemeExpr::SchemeFalse => Ok("#f".to_string()),
        &SchemeExpr::SchemeNull => Ok("'()".to_string()),
        &SchemeExpr::SchemeNum(num) => Ok(format!("{}", num).to_string()),
        &SchemeExpr::SchemeBuiltin(ref b)=> Ok(b.clone()),
        &SchemeExpr::SchemeStr(ref s)=> Ok(s.clone()),
        &SchemeExpr::SchemeSymbol(ref s)=> Ok(s.clone()),
        &SchemeExpr::SchemeIdentifier(ref s)=> Ok(s.to_string()),
        &SchemeExpr::SchemeProcedure(ref binds, ref body, _) => {
            let mut ret = "(lambda (".to_string();
            for bind in binds {
                ret = ret + &bind + " ";
            }
            ret = ret + ") ";
            for expr in body {
                ret = ret + &try!(scheme_repr(&expr));
            }
            ret = ret + ")";
            Ok(ret)
        },
        &SchemeExpr::SchemeList(ref list) => {
            let mut ret: String =
                list.iter().fold("(".to_string(),
                                 |acc, ref el| acc + " " + &scheme_repr(&el).unwrap());
            ret.push_str(" )");
            Ok(ret)
        },
        &SchemeExpr::SchemeQuote(ref list) => {
            let mut ret: String =
                list.iter().fold("(quote ".to_string(),
                                 |acc, ref el| acc + " " + &scheme_repr(&el).unwrap());
            ret.push_str(" )");
            Ok(ret)
        },
    }
}

//////////// Expression Evaluation

fn quote_action(list: &Vec<SchemeExpr>) -> Result<SchemeExpr, String> {
    // XXX: why can't I '.map()' here? (try .iter().skip(1)...)
    let mut body = Vec::<SchemeExpr>::new();
    for el in list[1..].to_vec() {
        body.push(el.clone());
    }
    Ok(SchemeExpr::SchemeQuote(body))
}

fn cond_action(list: &Vec<SchemeExpr>,
               ctx: HashMap<String, SchemeExpr>,
               env: &mut HashMap<String, SchemeExpr>) -> Result<SchemeExpr, String> {
    for line in list.iter().skip(1) {
        match line {
            &SchemeExpr::SchemeList(ref inner) => {
                if inner.len() != 2 {
                    return Err(format!("cond must contain tuples of (predicate, value) (len !=2) at: {}",
                                       scheme_repr(line).unwrap()));
                }
                let pred = &inner[0];
                let val = &inner[1];
                let m = try!(scheme_meaning(&pred, ctx.clone(), env));
                if m != SchemeExpr::SchemeFalse && m != SchemeExpr::SchemeNull {
                    return scheme_meaning(&val, ctx, env);
                } },
            _ => {
                return Err(format!("cond must contain tuples of (predicate, value); got: {}",
                                   scheme_repr(line).unwrap())); },
        }
    }
    // "undefined", return empty tuple
    Ok(SchemeExpr::SchemeNull)
}

fn lambda_action(list: &Vec<SchemeExpr>,
                 ctx: HashMap<String, SchemeExpr>) -> Result<SchemeExpr, String> {
    if list.len() < 3 {
        return Err(format!("lambda must have a bind and at least one body expr"));
    }
    let mut binds = Vec::<String>::new();
    let bind_list = match &list[1] {
        &SchemeExpr::SchemeList(ref bl) => bl,
        _ => { return Err(format!("second arg to lambda must be a list of binds; got: {}",
                                  scheme_repr(&list[1]).unwrap())); },
    };
    for bind in bind_list {
        match bind {
            &SchemeExpr::SchemeIdentifier(ref name) =>
                binds.push(name.clone()),
            _ => return Err(format!("lambda binds must all be non-builtin symbols; got: {}",
                                    scheme_repr(bind).unwrap()))
        }
    }
    let body = list.iter().skip(2).map(|x| x.clone()).collect();
    Ok(SchemeExpr::SchemeProcedure(binds, body, ctx.clone()))
}

fn apply_math_cmp(action: &str, args: Vec<SchemeExpr>) -> Result<SchemeExpr, String> {
    if args.len() != 2 {
        return Err(format!("math comparisons take 2 args (at {})", action));
    }
    let mut vals = Vec::<f64>::new();
    for arg in args {
        match arg {
            SchemeExpr::SchemeNum(x) => { vals.push(x) },
            _ => { return Err(format!("math builtins take only numerical types (got {})",
                                      scheme_repr(&arg).unwrap())) },
        }
    }

    let ret: bool = match action {
        "="     => vals[0] == vals[1],
        ">"     => vals[0] > vals[1],
        ">="    => vals[0] >= vals[1],
        "<"     => vals[0] < vals[1],
        "<="    => vals[0] <= vals[1],
        _ => { return Err(format!("unexpected math builting: {}", action)); },
    };
    return Ok( if ret {SchemeExpr::SchemeTrue} else { SchemeExpr::SchemeFalse } );
}

fn apply_math_unary(action: &str, args: Vec<SchemeExpr>) -> Result<SchemeExpr, String> {
    if args.len() != 1 {
        return Err(format!("math unary builtins only take one argument (at {})", action));
    }
    let val = match args[0] {
        SchemeExpr::SchemeNum(x) => x,
        _ => { return Err(format!("math builtins take only numerical types (got {})",
                                  scheme_repr(&args[0]).unwrap()))
        },
    };

    let ret: f64 = match action {
        "exp"     => val.exp(),
        "log"     => val.ln(),
        "sin"     => val.sin(),
        "cos"     => val.cos(),
        "tan"     => val.tan(),
        _ => { return Err(format!("unimplemented math operation: {}", action)); },
    };
    Ok(SchemeExpr::SchemeNum(ret))
}

fn apply_math_op(action: &str, args: Vec<SchemeExpr>) -> Result<SchemeExpr, String> {
    if args.len() < 2 {
        return Err(format!("math builtins take two or more args (at {})", action));
    }
    let mut vals = Vec::<f64>::new();
    for arg in args {
        match arg {
            SchemeExpr::SchemeNum(x) => { vals.push(x) },
            _ => { return Err(format!("math builtins take only numerical types (got {})",
                                      scheme_repr(&arg).unwrap())) },
        }
    }

    let ret: f64 = match action {
        "+"     => vals.iter().fold(0., |a, &b| a+b),
        "*"     => vals.iter().fold(1., |a, &b| a * b),
        "-"     => vals[1..].iter().fold(vals[0], |a, &b| a - b),
        "/"     => vals[1..].iter().fold(vals[0], |a, &b| a / b),
        _ => { return Err(format!("unimplemented math operation: {}", action)); },
    };
    Ok(SchemeExpr::SchemeNum(ret))
}

fn apply_typecheck(action: &str, args: Vec<SchemeExpr>) -> Result<SchemeExpr, String> {
    if args.len() != 1 {
        return Err(format!("typecheck builtins take a single argument (for {})", action));
    }
    let arg: &SchemeExpr = &args[0];
    let ret: bool = match action {
        "boolean?"   => *arg == SchemeExpr::SchemeTrue || *arg == SchemeExpr::SchemeFalse,
        "symbol?"   => match *arg {
            SchemeExpr::SchemeSymbol(_) => true,
            _ => false,
        },
        "procedure?"   => match *arg {
            SchemeExpr::SchemeProcedure(_, _, _) => true,
            _ => false,
        },
        "pair?"   => match *arg {
            SchemeExpr::SchemeList(_) => true,
            _ => false,
        },
        "number?" => match *arg {
            SchemeExpr::SchemeNum(_) => true,
            _ => false
        },
        "string?"   => match *arg {
            SchemeExpr::SchemeStr(_) => true,
            _ => false,
        },
        "null?"   => *arg == SchemeExpr::SchemeNull,
        "atom?"   => match *arg {
            SchemeExpr::SchemeNull |
                SchemeExpr::SchemeTrue |
                SchemeExpr::SchemeFalse |
                SchemeExpr::SchemeNum(_) => true,
            _ => false
        },
        "zero?"   => *arg == SchemeExpr::SchemeNum(0.0),
        _ => { return Err(format!("unimplemented typecheck builtin: {}", action)); },
    };
    if ret {
        Ok(SchemeExpr::SchemeTrue)
    } else {
        Ok(SchemeExpr::SchemeFalse)
    }
}

/*
 * This function is sort of the heart the program: it takes a non-builtin SchemeProcedure (aka, a
 * parsed lambda expression) and applies it to arguments.
 */
fn apply_action(list: &Vec<SchemeExpr>,
                ctx: HashMap<String, SchemeExpr>,
                env: &mut HashMap<String, SchemeExpr>) -> Result<SchemeExpr, String> {
    if list.len() == 0 {
        // TODO: is this correct?
        return Ok(SchemeExpr::SchemeNull);
    }
    let action = &list[0];
    let mut procedure = SchemeExpr::SchemeNull;

    /*
     * First pass:
     *  - execute special case built-ins (eg, those which take non-symbol identifiers as args)
     *  - lambdas and identifiers which need to be expanded
     */
    match action {
        &SchemeExpr::SchemeBuiltin(ref builtin) => {
            match builtin.as_str() {
                "define" | "set!" => {
                    if list.len() != 3 {
                        return Err(format!("define takes two arguments (lambda syntax not supported)"));
                    }
                    match &list[1] {
                        &SchemeExpr::SchemeIdentifier(ref sym) => {
                            if builtin == "set!" && !env.contains_key(sym) {
                                return Err(format!("tried to `set!` an undefined identifier: {}", sym));
                            }
                            let val = try!(scheme_meaning(&list[2], ctx.clone(), env));
                            env.insert(sym.clone(), val);
                            return Ok(SchemeExpr::SchemeNull);
                        },
                        _ => { return Err(format!("define requires an identifier")); },
                    }
                },
                _ => (), // fall through to second pass
            }
        },
        &SchemeExpr::SchemeProcedure(_, _, _) => (), // fall through to second pass
        _ => {
            // expand, eg, lambdas and non-builtin identifiers in the head position of the apply
            procedure = try!(scheme_meaning(action, ctx.clone(), env));
        },
    }

    let action = if procedure != SchemeExpr::SchemeNull { &procedure } else { action };

    // Second pass:
    //  -  evaluate all arguments
    //  - dispatch to builtin handlers or procedure handler
    // TODO: make this arg meaning .map() a single line?
    let arg_meanings: Result<Vec<_>, _> = list.iter().skip(1).map(|x| scheme_meaning(x, ctx.clone(), env)).collect();
    let args: Vec<SchemeExpr> = try!(arg_meanings);
    match action {
        &SchemeExpr::SchemeBuiltin(ref builtin) => {
            return match builtin.as_str() {
                "+" | "-" | "*" | "/" => apply_math_op(builtin, args),
                "=" | ">" | ">=" | "<" | "<=" => apply_math_cmp(builtin, args),
                "exp"| "log" | "sin" | "cos" | "tan" => apply_math_unary(builtin, args),
                "boolean?" | "symbol?" | "procedure?" | "pair?" | "number?" | "string?" |
                    "null?" | "atom?" | "zero?" => apply_typecheck(builtin, args),
                "eq?" => {
                    if args.len() != 2 {
                        return Err(format!("eq? takes only two arguments"));
                    }
                    if args[0] == args[1] {
                        return Ok(SchemeExpr::SchemeTrue)
                    } else {
                        return Ok(SchemeExpr::SchemeFalse)
                    }
                },
                "car" => {
                    if args.len() != 1 {
                        return Err(format!("car takes a single list argument"));
                    }
                    match &args[0] {
                        &SchemeExpr::SchemeList(ref list) => {
                            Ok(list[0].clone())
                        },
                        _ => Err(format!("cdr only takes lists"))
                    }
                },
                "cdr" => {
                    if args.len() != 1 {
                        return Err(format!("cdr takes a single list argument"));
                    }
                    match &args[0] {
                        &SchemeExpr::SchemeList(ref list) => {
                            Ok(SchemeExpr::SchemeList(list[1..].to_vec()))
                        },
                        _ => Err(format!("car only takes lists"))
                    }
                },
                "cons" => {
                    if args.len() != 2 {
                        return Err(format!("cons takes two arguments"));
                    }
                    match &args[1] {
                        &SchemeExpr::SchemeList(ref list) => {
                            let mut ret = vec![args[0].clone()];
                            ret.extend_from_slice(list);
                            Ok(SchemeExpr::SchemeList(ret))
                        },
                        _ => Err(format!("cdr takes only lists"))
                    }
                },
                _ => Err(format!("unimplemented builtin: {}", builtin)),
            };
        },
        &SchemeExpr::SchemeProcedure(ref binds, ref body, ref proc_ctx) => {
            // This block of code implements procedure (lambda) application
            if body.len() != 1 {
                return Err(format!("prodedure must have single-expression body"));
            }
            if binds.len() != args.len() {
                return Err(format!("wrong number of args to procedure"));
            }
            let mut closure = proc_ctx.clone();
            for (name, arg) in binds.iter().zip(args) {
                closure.insert(name.clone(), arg.clone());
            }
            return scheme_meaning(&body[0], closure, env);
        },
        _ => { return Err(format!("non-procedure at head of expression: {}",
                                  scheme_repr(&action).unwrap()));
        },
    }
}

/*
 * This is the main entry point for eval: it recursively evaluates an AST and returns the result.
 */
fn scheme_meaning(ast: &SchemeExpr,
                  ctx: HashMap<String, SchemeExpr>,
                  env: &mut HashMap<String, SchemeExpr>) -> Result<SchemeExpr, String> {

    return match ast {
            // "identity actions"
        &SchemeExpr::SchemeTrue         => Ok(ast.clone()),
        &SchemeExpr::SchemeFalse        => Ok(ast.clone()),
        &SchemeExpr::SchemeNull         => Ok(ast.clone()),
        &SchemeExpr::SchemeStr(_)       => Ok(ast.clone()),
        &SchemeExpr::SchemeSymbol(_)    => Ok(ast.clone()),
        &SchemeExpr::SchemeNum(_)       => Ok(ast.clone()),
        &SchemeExpr::SchemeBuiltin(_)   => Ok(ast.clone()),
        &SchemeExpr::SchemeProcedure(_, _, _) => Ok(ast.clone()),
        &SchemeExpr::SchemeQuote(ref list)
                                        => Ok(SchemeExpr::SchemeList(list.clone())),
        &SchemeExpr::SchemeIdentifier(ref sym)  => {
            match ctx.get(sym) {
                // the "lookup action"
                Some(val) => Ok(val.clone()),
                None => {
                    match env.get(sym) {
                        // fall through to env...
                        Some(val) => Ok(val.clone()),
                        None => Err(format!("identifier not defined: {}", sym)),
                    }
                }
            }
        },
        &SchemeExpr::SchemeList(ref list) => {
            if list.len() == 0 {
                return Ok(SchemeExpr::SchemeNull);
            }
            match list[0] {
                SchemeExpr::SchemeBuiltin(ref b) if b == "quote" =>
                    quote_action(&list),
                SchemeExpr::SchemeBuiltin(ref b) if b == "cond" =>
                    cond_action(&list, ctx, env),
                SchemeExpr::SchemeBuiltin(ref b) if b == "lambda" =>
                    lambda_action(&list, ctx),
                SchemeExpr::SchemeBuiltin(_) =>
                    apply_action(&list, ctx, env),
                SchemeExpr::SchemeProcedure(_, _, _) =>
                    apply_action(&list, ctx, env),
                SchemeExpr::SchemeList(_) =>
                    apply_action(&list, ctx, env),
                SchemeExpr::SchemeIdentifier(_) =>
                    apply_action(&list, ctx, env),
                _ => {
                    Err(format!("unexpected head of expression: {}",
                                scheme_repr(&list[0]).unwrap()))
                }
            }
        },
    }
}

fn scheme_eval(ast: &SchemeExpr,
               env: &mut HashMap<String, SchemeExpr>) -> Result<SchemeExpr, String> {
    let ctx = HashMap::<String, SchemeExpr>::new();
    Ok(try!(scheme_meaning(ast, ctx, env)))
}

//////////// Top-Level Program

fn repl<'b>(verbose: bool, top_env: &mut HashMap<String, SchemeExpr>) {

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        let raw_input = &mut String::new();
        stdout.write(b"\nspectrum> ").unwrap();
        stdout.flush().unwrap();
        stdin.read_line(raw_input).unwrap();
        let raw_input = raw_input; // mutable to immutable reference
        if raw_input.len() == 0 {
            // end-of-line, aka Ctrl-D. Blank line will still have newline char
            stdout.write(b"\nCiao!\n").unwrap();
            return;
        }
        // TODO: use debug or something instead of "verbose"?
        let tokens = match scheme_tokenize(&raw_input) {
            Ok(tokens) => {
                if verbose { println!("Tokens: {}", tokens.join(", ")); };
                tokens
            },
            Err(e) => {
                println!("couldn't tokenize: {}", e);
                continue;
            }
        };
        let ast = match scheme_parse(&tokens, 0) {
            Ok((mut ast_list, _)) => {
                if verbose {
                    for ast in &ast_list {
                        println!("AST: {}", scheme_repr(ast).unwrap());
                    };
                };
                // We're a REPL, so only one expression at a time
                if ast_list.len() > 1 {
                    println!("one expression at a time please!");
                    continue;
                } else if ast_list.len() == 0 {
                    SchemeExpr::SchemeNull
                } else {
                    let ast = ast_list.pop().unwrap();
                    ast
                }
            },
            Err(e) => {
                println!("couldn't parse: {}", e);
                continue;
            }
        };
        let resp = match scheme_eval(&ast, top_env) {
            Ok(x) => x,
            Err(e) => {
                println!("couldn't eval: {}", e);
                continue;
            }
        };
        println!("{}", scheme_repr(&resp).unwrap());
    }
}

fn main() {

    let mut top_env = HashMap::<String, SchemeExpr>::new();

    // For now only REPL mode is implemented
    repl(true, &mut top_env);
}

