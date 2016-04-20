
// A partial Scheme implementation in Rust
// Build with: rustc minimal.rs -o minimal-rust

use std::io;
use std::io::Write;

// There doesn't seem to be a symbole or quote type in Rust, so i'm going to use strings and
// vectors

// "list of tokens" implemented as a vector of String.
// AST implemented as nested tuples (immutable).

//let const SCHEME_BUILTINS = ("lambda", "quote", "cond", "else", "cons", "car", "cdr", "null?", "eq?", "atom?", "zero?", "number?", "+", "-", "*", "/");

#[allow(dead_code)]
enum SchemeExpr<'a> {
    SchemeNull,
    SchemeTrue,
    SchemeFalse,
    SchemeNum(f64),
    SchemeBuiltin(&'a str),
    SchemeStr(&'a str),
    SchemeList(Vec<SchemeExpr<'a>>),
}

///////////////////////////////////

fn is_zero(n: f64) -> bool {
    return n == 0.;
}

///////////////////////////////////

fn is_scheme_whitespace(c: char) -> bool{
    " \r\n".find(c) != None
}
fn is_scheme_sep(c: char) -> bool {
    "()".find(c) != None
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

fn scheme_parse_num(s: &String) -> Result<f64, &'static str> {
    let num = 0.;
    return Ok(num);
}

fn scheme_parse_sexpr<'a>(sexpr: &Vec<&'a str>) -> Result<SchemeExpr<'a>, &'static str> {
    let ret = sexpr.into_iter().map(|el| SchemeExpr::SchemeStr(el)).collect();
    return Ok(SchemeExpr::SchemeList(ret))
}

fn scheme_parse<'a>(tokens: &SchemeExpr) -> Result<SchemeExpr<'a>, &'static str> {
    return Ok(SchemeExpr::SchemeNull);
}

fn scheme_eval<'a>(ast: &SchemeExpr) -> Result<SchemeExpr<'a>, &'static str> {
    return Ok(SchemeExpr::SchemeNull);
}

fn scheme_repr<'a>(ast: &SchemeExpr) -> Result<String, &'static str> {
    return match ast {
        &SchemeExpr::SchemeTrue => Ok("#t".to_string()),
        &SchemeExpr::SchemeFalse => Ok("#t".to_string()),
        &SchemeExpr::SchemeNull => Ok("'()".to_string()),
        &SchemeExpr::SchemeList(ref list) => {
            let mut ret: String =
                list.iter().fold("(".to_string(),
                                 |acc, ref el| acc + &scheme_repr(&el).unwrap());
            ret.push_str(")");
            Ok(ret)
        },
        _ => Err("don't know how to repr something"),
    }
}

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
        let tokens = scheme_tokenize(&raw_input).unwrap();
        println!("Tokens: {}", tokens.join(", "));
        let sexpr = scheme_parse_sexpr(&tokens).unwrap();
        let ast = scheme_parse(&sexpr).unwrap();
        let resp = scheme_eval(&ast).unwrap();
        println!("{}", scheme_repr(&resp).unwrap());
    }
}

