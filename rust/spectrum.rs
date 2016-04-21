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

// There doesn't seem to be a symbol or quote type in Rust, so i'm going to use strings and vectors

// XXX: how to avoid the '16' here?
const SCHEME_BUILTINS: [&'static str; 16] = ["lambda", "quote", "cond", "else", "cons", "car", "cdr",
    "null?", "eq?", "atom?", "zero?", "number?", "+", "-", "*", "/"];

#[derive(Clone, PartialEq)]
enum SchemeExpr<'a> {
    SchemeNull,
    SchemeTrue,
    SchemeFalse,
    SchemeNum(f64),
    SchemeBuiltin(&'a str),
    SchemeSymbol(&'a str),
    SchemeStr(&'a str),
    SchemeProcedure(
        Vec<&'a str>,
        Vec<SchemeExpr<'a>>,
        HashMap<&'a str, SchemeExpr<'a>>),
    SchemeList(Vec<SchemeExpr<'a>>),
    SchemeQuote(Vec<SchemeExpr<'a>>),
}

//////////// Lexing, Parsing, and Printing

fn is_scheme_whitespace(c: char) -> bool{
    " \r\n".find(c) != None
}
fn is_scheme_sep(c: char) -> bool {
    "()".find(c) != None
}

fn is_valid_symbol(s: &str) -> bool {
    // TODO: this could be an 'any' or 'filter' call?
    if s.len() == 0 {
        return false;
    }
    for c in s.chars() {
        if !c.is_alphabetic() && c != '-' {
            return false;
        }
    }
    return true;
}

// TODO: need to expand prefix notation stuff like `(1 2 3) to (quote 1 2 3) here?
fn scheme_tokenize<'a>(raw_str: &'a str) -> Result<Vec<&'a str>, &'static str> {
    let mut ret = Vec::<&str>::new();
    let mut food: usize = 0;
    let mut quoted: bool = false;
    for (i, c) in raw_str.chars().enumerate() {
        if quoted {
            if c == '"' && raw_str.chars().collect::<Vec<char>>()[i-1] != '\\' {
                ret.push(&raw_str[i-food-1..i+1]);
                quoted = false;
                food = 0;
            } else if raw_str.len() == i+1 {
                return Err("unmatched quote char");
            } else {
                food += 1;
            }
        } else if c == '"' {
            if food > 0 {
                return Err("unexpected quote char");
            }
            if raw_str.len() == i+1 {
                return Err("unmatched (trailing) quote char");
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
            ret.push(&raw_str[i-food..]);
        } else {
            food += 1;
        }
    }
    return Ok(ret);
}

fn scheme_parse_token(token: &str) -> Result<SchemeExpr, &'static str> {

    // First match on easy stuff
    match token {
        "#t" => return Ok(SchemeExpr::SchemeTrue),
        "#f" => return Ok(SchemeExpr::SchemeFalse),
        ")"  => return Ok(SchemeExpr::SchemeNull),
        _ => ()
    }

    // Is it a builtin?
    if SCHEME_BUILTINS.contains(&token) {
        return Ok(SchemeExpr::SchemeBuiltin(token));
    }

    // Try to parse as a number
    match token.parse::<f64>() {
        Ok(x) => return Ok(SchemeExpr::SchemeNum(x)),
        Err(_) => ()
    }

    // Is it a string?
    if token.starts_with("\"") && token.ends_with("\"") {
        return Ok(SchemeExpr::SchemeStr(token));
    }

    // If it's all alphas, must be a symbol
    if is_valid_symbol(token) {
        return Ok(SchemeExpr::SchemeSymbol(token));
    }

    return Err("unparsable token");
}

fn scheme_parse<'a>(tokens: &Vec<&'a str>, depth: u32) -> Result<(SchemeExpr<'a>, usize), &'static str> {
    let mut ret = Vec::<SchemeExpr>::new();
    let mut i: usize = 0;
    if tokens.len() == 0  {
        return Ok((SchemeExpr::SchemeNull, 0));
    } else if tokens.len() == 1 {
        let expr = try!(scheme_parse_token(tokens[0]));
        return Ok((expr, 1));
    }
    while i < tokens.len() {
        match tokens[i] {
            "(" => {
                let (expr, skip) = try!(scheme_parse(&tokens[i+1..].to_vec(), depth+1));
                ret.push(expr);
                i += skip;},
            ")" => {
                if depth == 0 {
                    return Err("missing an open bracket");
                }
                return Ok((SchemeExpr::SchemeList(ret), i+1));},
            token => {
                let expr = try!(scheme_parse_token(token));
                ret.push(expr);
            }
        }
        i += 1;
    }
    if depth > 0 {
        return Err("missing a close bracket");
    }
    let rlen = ret.len();
    if depth == 0 && rlen == 1 {
        return Ok((ret.pop().unwrap(), rlen));
    } else {
        return Ok((SchemeExpr::SchemeList(ret), rlen));
    }
}

fn scheme_repr(ast: &SchemeExpr) -> Result<String, &'static str> {
    return match ast {
        &SchemeExpr::SchemeTrue => Ok("#t".to_string()),
        &SchemeExpr::SchemeFalse => Ok("#f".to_string()),
        &SchemeExpr::SchemeNull => Ok("'()".to_string()),
        &SchemeExpr::SchemeNum(num) => Ok(format!("{}", num).to_string()),
        &SchemeExpr::SchemeBuiltin(b)=> Ok(b.to_string()),
        &SchemeExpr::SchemeStr(s)=> Ok(s.to_string()),
        &SchemeExpr::SchemeSymbol(s)=> Ok(s.to_string()),
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

#[allow(unused_variables)]
fn quote_action<'a>(list: &Vec<SchemeExpr<'a>>, ctx: HashMap<&str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
    // XXX: why can't I '.map()' here? (try .iter().skip(1)...)
    let mut body = Vec::<SchemeExpr>::new();
    for el in list[1..].to_vec() {
        body.push(el.clone());
    }
    Ok(SchemeExpr::SchemeQuote(body))
}

fn cond_action<'a>(list: &Vec<SchemeExpr<'a>>, ctx: HashMap<&'a str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
    for line in list.iter().skip(1) {
        match line {
            &SchemeExpr::SchemeList(ref inner) => {
                if inner.len() != 2 {
                    return Err("cond must contain tuples of (predicate, value) (len !=2)");
                }
                let pred = &inner[0];
                let val = &inner[1];
                let m = try!(scheme_meaning(&pred, ctx.clone()));
                if m != SchemeExpr::SchemeFalse && m != SchemeExpr::SchemeNull {
                    return scheme_meaning(&val, ctx);
                } },
            _ => {
                return Err("cond must contain tuples of (predicate, value)"); },
        }
    }
    // "undefined", return empty tuple
    Ok(SchemeExpr::SchemeNull)
}

fn lambda_action<'a>(list: &Vec<SchemeExpr<'a>>, ctx: HashMap<&'a str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
    if list.len() < 3 {
        return Err("lambda must have a bind and at least one body expr");
    }
    let mut binds = Vec::<&str>::new();
    let bind_list = match &list[1] {
        &SchemeExpr::SchemeList(ref bl) => bl,
        _ => { return Err("second arg to lambda must be a list of binds") },
    };
    for bind in bind_list {
        match bind {
            &SchemeExpr::SchemeSymbol(name) =>
                binds.push(name),
            _ => return Err("lambda binds must all be non-builtin symbols")
        }
    }
    let body = list.iter().skip(2).map(|x| x.clone()).collect();
    Ok(SchemeExpr::SchemeProcedure(binds, body, ctx.clone()))
}

fn apply_math_op<'a>(action: &'a str, args: Vec<SchemeExpr>) -> Result<SchemeExpr<'a>, &'static str> {
    if args.len() < 2 {
        return Err("math builtins take two or more args");
    }
    let mut vals = Vec::<f64>::new();
    for arg in args {
        match arg {
            SchemeExpr::SchemeNum(x) => { vals.push(x) },
            _ => { return Err("math builtins take only numerical types") },
        }
    }

    let ret: f64 = match action {
        "+"     => vals.iter().fold(0., |a, &b| a+b),
        "*"     => vals.iter().fold(1., |a, &b| a * b),
        "-"     => vals[1..].iter().fold(vals[0], |a, &b| a - b),
        "/"     => vals[1..].iter().fold(vals[0], |a, &b| a / b),
        _ => { return Err("unimplemented math operation"); },
    };
    Ok(SchemeExpr::SchemeNum(ret))
}

fn apply_typecheck<'a>(action: &'a str, args: Vec<SchemeExpr>) -> Result<SchemeExpr<'a>, &'static str> {
    if args.len() != 1 {
        return Err("typecheck builtins take a single argument");
    }
    let arg: &SchemeExpr = &args[0];
    let ret: bool = match action {
        "null?"   => *arg == SchemeExpr::SchemeNull,
        "zero?"   => *arg == SchemeExpr::SchemeNum(0.0),
        "number?" => match *arg {
            SchemeExpr::SchemeNum(_) => true,
            _ => false},
        "atom?"   => match *arg {
            SchemeExpr::SchemeNull |
                SchemeExpr::SchemeTrue |
                SchemeExpr::SchemeFalse |
                SchemeExpr::SchemeNum(_) => true,
            _ => false},
        _ => { return Err("unimplemented typecheck builtin"); },
    };
    if ret {
        Ok(SchemeExpr::SchemeTrue)
    } else {
        Ok(SchemeExpr::SchemeFalse)
    }
}

fn apply_action<'a>(list: &Vec<SchemeExpr<'a>>, ctx: HashMap<&'a str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
    if list.len() == 0 {
        // TODO: is this correct?
        return Ok(SchemeExpr::SchemeNull);
    }
    let action = &list[0];
    let args: Vec<SchemeExpr> = list.iter().skip(1).map(|x| scheme_meaning(x, ctx.clone()).unwrap()).collect();
    match action {
        &SchemeExpr::SchemeBuiltin(builtin) => {
            return match builtin {
                "+" | "-" | "*" | "/" => apply_math_op(builtin, args),
                "null?" | "number?" | "zero?" | "atom?" => apply_typecheck(builtin, args),
                "eq?" => {
                    if args.len() != 2 {
                        return Err("eq? takes only two arguments");
                    }
                    if args[0] == args[1] {
                        return Ok(SchemeExpr::SchemeTrue)
                    } else {
                        return Ok(SchemeExpr::SchemeFalse)
                    }
                },
                "car" => {
                    if args.len() != 1 {
                        return Err("car takes a single list argument");
                    }
                    match &args[0] {
                        &SchemeExpr::SchemeList(ref list) => {
                            Ok(list[0].clone())
                        },
                        _ => Err("cdr takes only lists")
                    }
                },
                "cdr" => {
                    if args.len() != 1 {
                        return Err("cdr takes a single list argument");
                    }
                    match &args[0] {
                        &SchemeExpr::SchemeList(ref list) => {
                            Ok(SchemeExpr::SchemeList(list[1..].to_vec()))
                        },
                        _ => Err("car takes only lists")
                    }
                },
                "cons" => {
                    if args.len() != 2 {
                        return Err("cons takes two arguments");
                    }
                    match &args[1] {
                        &SchemeExpr::SchemeList(ref list) => {
                            let mut ret = vec![args[0].clone()];
                            ret.extend_from_slice(list);
                            Ok(SchemeExpr::SchemeList(ret))
                        },
                        _ => Err("cdr takes only lists")
                    }
                },
                _ => Err("unimplemented builtin"),
            }; },
        &SchemeExpr::SchemeList(_) => {
            let procedure: SchemeExpr = try!(scheme_meaning(&action, ctx.clone()));
            match procedure {
                SchemeExpr::SchemeProcedure(binds, body, proc_ctx) => {
                    // This block of code implements procedure (lambda) application
                    if body.len() != 1 {
                        return Err("prodedure must have single-expression body");
                    }
                    if binds.len() != args.len() {
                        return Err("wrong number of args to procedure");
                    }
                    let mut closure = proc_ctx.clone();
                    for (name, arg) in binds.iter().zip(args) {
                        closure.insert(name, arg.clone());
                    }
                    return scheme_meaning(&body[0], closure);
                },
                _ => { return Err("non-procedure at head of expression"); },
                } },
        _ => { return Err("apply called with something non-applicable"); },
    }
}

fn scheme_meaning<'a>(ast: &SchemeExpr<'a>, ctx: HashMap<&'a str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
    return match ast {
            // "identity actions"
        &SchemeExpr::SchemeTrue         => Ok(ast.clone()),
        &SchemeExpr::SchemeFalse        => Ok(ast.clone()),
        &SchemeExpr::SchemeNull         => Ok(ast.clone()),
        &SchemeExpr::SchemeStr(_)       => Ok(ast.clone()),
        &SchemeExpr::SchemeNum(_)       => Ok(ast.clone()),
        &SchemeExpr::SchemeBuiltin(_)   => Ok(ast.clone()),
        &SchemeExpr::SchemeProcedure(_, _, _) => Ok(ast.clone()),
        &SchemeExpr::SchemeQuote(ref list)
                                        => Ok(SchemeExpr::SchemeList(list.clone())),
        &SchemeExpr::SchemeSymbol(sym)  => match ctx.get(sym) {
            // the "lookup action"
            Some(val) => Ok(val.clone()),
            None => Err("symbol not defined"),
        },
        &SchemeExpr::SchemeList(ref list) => {
            if list.len() == 0 {
                return Ok(SchemeExpr::SchemeNull);
            }
            match list[0] {
                SchemeExpr::SchemeBuiltin("quote") =>
                    quote_action(&list, ctx),
                SchemeExpr::SchemeBuiltin("cond") =>
                    cond_action(&list, ctx),
                SchemeExpr::SchemeBuiltin("lambda") =>
                    lambda_action(&list, ctx),
                SchemeExpr::SchemeBuiltin(_) =>
                    apply_action(&list, ctx),
                SchemeExpr::SchemeProcedure(_, _, _) =>
                    apply_action(&list, ctx),
                SchemeExpr::SchemeList(_) =>
                    apply_action(&list, ctx),
                _ => Ok(SchemeExpr::SchemeNull)
            }
        },
    }
}

fn scheme_eval<'a>(ast: &'a SchemeExpr) -> Result<SchemeExpr<'a>, &'static str> {
    let ctx = HashMap::<&str, SchemeExpr>::new();
    Ok(try!(scheme_meaning(ast, ctx)))
}

//////////// Top-Level Program

fn main() {

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        let raw_input = &mut String::new();
        stdout.write(b"\nspectrum> ").unwrap();
        stdout.flush().unwrap();
        stdin.read_line(raw_input).unwrap();
        let raw_input = raw_input;  // UGH
        if raw_input.len() == 0 {
            stdout.write(b"\nCiao!\n").unwrap();
            return;
        }
        let tokens = match scheme_tokenize(&raw_input) {
            Ok(tokens) => {
                println!("Tokens: {}", tokens.join(", ")); // debug
                tokens},
            Err(e) => {
                println!("couldn't tokenize: {}", e);
                continue}};
        let ast = match scheme_parse(&tokens, 0) {
            Ok((ast, _)) => {
                println!("AST: {}", scheme_repr(&ast).unwrap());
                ast},
            Err(e) => {
                println!("couldn't parse: {}", e);
                continue}};
        let resp = match scheme_eval(&ast) {
            Ok(x) => x,
            Err(e) => {
                println!("couldn't eval: {}", e);
                continue}};
        println!("{}", scheme_repr(&resp).unwrap());
    }
}

