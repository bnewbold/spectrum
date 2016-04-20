
// A partial Scheme implementation in Rust
// Build with: rustc minimal.rs -o minimal-rust

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
        Vec<SchemeExpr<'a>>,
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
            } else if (raw_str.len() == i+1) {
                return Err("unmatched quote char");
            } else {
                food += 1;
            }
        } else if c == '"' {
            if food > 0 {
                return Err("unexpected quote char");
            }
            if (raw_str.len() == i+1) {
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
        } else if (raw_str.len() == i+1) {
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

fn scheme_repr<'a>(ast: &SchemeExpr) -> Result<String, &'static str> {
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
                ret = ret + &try!(scheme_repr(&bind)) + " ";
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

fn quote_action<'a>(list: &'a Vec<SchemeExpr>, ctx: HashMap<&str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
    // XXX: why can't I '.map()' here? (try .iter().skip(1)...)
    let mut body = Vec::<SchemeExpr>::new();
    for el in list[1..].to_vec() {
        body.push(el.clone());
    }
    Ok(SchemeExpr::SchemeList(body))
}

fn cond_action<'a>(list: &'a Vec<SchemeExpr>, ctx: HashMap<&'a str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
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

fn lambda_action<'a>(list: &'a Vec<SchemeExpr>, ctx: HashMap<&'a str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
    if list.len() < 3 {
        return Err("lambda must have a bind and at least one body expr");
    }
    let mut binds = Vec::<SchemeExpr>::new();
    let bind_list = match &list[1] {
        &SchemeExpr::SchemeList(ref bl) => bl,
        _ => { return Err("second arg to lambda must be a list of binds") },
    };
    for bind in bind_list {
        match bind {
            &SchemeExpr::SchemeSymbol(_) =>
                binds.push(bind.clone()),
            _ => return Err("lambda binds must all be non-builtin symbols")
        }
    }
    let mut body = list.iter().skip(2).map(|x| x.clone()).collect();
    Ok(SchemeExpr::SchemeProcedure(binds, body, ctx.clone()))
}

fn apply_action<'a>(list: &'a Vec<SchemeExpr>, ctx: HashMap<&'a str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
    Ok(SchemeExpr::SchemeNull)
}

fn scheme_meaning<'a>(ast: &'a SchemeExpr, ctx: HashMap<&'a str, SchemeExpr<'a>>) -> Result<SchemeExpr<'a>, &'static str> {
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
            match list[0] {
                SchemeExpr::SchemeBuiltin("quote") =>
                    quote_action(list, ctx),
                SchemeExpr::SchemeBuiltin("cond") =>
                    cond_action(list, ctx),
                SchemeExpr::SchemeBuiltin("lambda") =>
                    lambda_action(list, ctx),
                SchemeExpr::SchemeBuiltin(_) =>
                    apply_action(list, ctx),
                SchemeExpr::SchemeProcedure(_, _, _) =>
                    apply_action(list, ctx),
                SchemeExpr::SchemeList(_) =>
                    apply_action(list, ctx),
                _ => Ok(SchemeExpr::SchemeNull)
            }},
    }
}

fn scheme_eval<'a>(ast: &'a SchemeExpr) -> Result<SchemeExpr<'a>, &'static str> {
    let mut ctx = HashMap::<&str, SchemeExpr>::new();
    Ok(try!(scheme_meaning(ast, ctx)))
}

//////////// Top-Level Program

fn main() {

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        let raw_input = &mut String::new();
        stdout.write(b"\nminimal-rust> ").unwrap();
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

