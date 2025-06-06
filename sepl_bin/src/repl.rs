use linefeed::{Interface, ReadResult, Terminal};

use anyhow::Result;
use sepl_lib::{
    eval::{EnvTable, Expr, SymbolTable}, lex::{Lex, Token}, parse::{Error as ParseError, Parser},
    stringify::Stringify,
};

pub fn repl() -> Result<()> {
    let mut symbol_table = SymbolTable::new();
    let mut env_table = EnvTable::with_builtins(&mut symbol_table);
    let env_global = env_table.env_global();

    let reader_interface = Interface::new("sepl")?;

    print_header();
    loop {
        let parsed_exprs = match read_parse(&reader_interface, &mut symbol_table)? {
            Some(expr) => expr,
            None => break,
        };

        for parsed_expr in parsed_exprs {
            let evald_expr = parsed_expr.eval(&mut env_table, env_global);
            match evald_expr {
                Ok(expr) => print!("{} ", expr.stringify(&symbol_table)),
                Err(err) => println!("{}", err.stringify(&symbol_table)),
            }
        }
        println!();
    }

    Ok(())
}

fn print_header() {
    println!("Welcome to {} v{} interactive REPL.\n", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
}

fn read_parse<T: Terminal>(reader_interface: &Interface<T>, symbol_table: &mut SymbolTable) -> Result<Option<Vec<Expr>>> {
    let mut line_buf = String::new();
    let mut exprs = Vec::new();
    loop {
        if line_buf.len() == 0 {
            reader_interface.set_prompt("> ")?;
        } else {
            line_buf.push(' ');
            reader_interface.set_prompt("\t.. ")?;
        }

        match reader_interface.read_line()? {
            ReadResult::Input(string) => {
                line_buf.push_str(&string);
            },
            ReadResult::Signal(_) => 
                break Ok(None),
            ReadResult::Eof => 
                break Ok(None),
        }

        let line_cloned = line_buf.clone();
        let parser = Parser::new(Token::lexer(&line_cloned), symbol_table);

        for parse_result in parser {
            let parsed_expr = match parse_result {
                Ok(expr) => {
                    expr
                },
                Err(ParseError::UnexpectedEOF) => {
                    continue;
                }
                Err(err) => {
                    println!("{err}");
                    line_buf.clear();
                    continue;
                },
            };
            exprs.push(parsed_expr);
        }
        
        if !line_buf.trim().is_empty() {
            reader_interface.add_history_unique(line_buf.clone());
        }

        break Ok(Some(exprs))
    }
}
