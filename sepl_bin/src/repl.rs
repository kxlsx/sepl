use linefeed::{Interface, ReadResult, Terminal};

use anyhow::Result;
use sepl_lib::{
    eval::{EnvTable, Expr, SymbolTable},
    lex::{Lex, Token},
    parse::{Error as ParseError, Parser},
    stringify::Stringify,
};

// TODO: this file is a bit sad, but I'm currently short on time

enum ReplCommand {
    Eval(Box<[Expr]>),
    Quit,
    PrintDefined,
    Help,
}

fn print_header() {
    // TODO: make this more fun
    println!(
        "Welcome to {} v{} interactive REPL.\n",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION")
    );
    println!("type ':h' for help.")
}

fn print_help() {
    // FIXME: this needs to be checked with ReplCommands
    println!(":q | :quit    => Exit the program.");
    println!(":d | :defined => Print symbol definitions");
    println!(":h | :help    => Print this message.");
}

pub fn repl() -> Result<()> {
    let mut symbol_table = SymbolTable::new();
    let mut env_table = EnvTable::with_builtins(&mut symbol_table);

    let reader_interface = Interface::new("sepl")?;

    print_header();
    loop {
        match read_repl_command(&reader_interface, &mut symbol_table)? {
            ReplCommand::Eval(parsed_exprs) => {
                for parsed_expr in parsed_exprs {
                    let evald_expr = parsed_expr.eval_global(&mut env_table);
                    match evald_expr {
                        Ok(expr) => print!("{} ", expr.stringify(&symbol_table)),
                        Err(err) => println!("{}", err.stringify(&symbol_table)),
                    }
                }
                println!();
            }
            ReplCommand::PrintDefined => {
                let symbol_defs = env_table.symbols_global();
                if let Some(defs) = symbol_defs {
                    for (symbol, expr) in defs {
                        println!(
                            "'{}' => {}",
                            symbol_table.resolve(*symbol),
                            expr.stringify(&symbol_table)
                        );
                    }
                }
            }
            ReplCommand::Help => print_help(),
            ReplCommand::Quit => break,
        };
    }

    Ok(())
}

fn read_repl_command<T: Terminal>(
    reader_interface: &Interface<T>,
    symbol_table: &mut SymbolTable,
) -> Result<ReplCommand> {
    let mut line_buf = String::new();
    let mut exprs = Vec::new();
    'readmore: loop {
        if line_buf.is_empty() {
            reader_interface.set_prompt("> ")?;
        } else {
            line_buf.push(' ');
            reader_interface.set_prompt("\t.. ")?;
        }

        match reader_interface.read_line()? {
            ReadResult::Input(string) => {
                line_buf.push_str(&string);
            }
            ReadResult::Signal(_) => break Ok(ReplCommand::Quit),
            ReadResult::Eof => break Ok(ReplCommand::Quit),
        }

        if line_buf.trim().starts_with(":") {
            if let Some(command) = parse_repl_command(&line_buf.trim()[1..]) {
                return Ok(command);
            } else {
                println!("Invalid command '{}'.", &line_buf.trim()[1..]);
                line_buf.clear();
                continue 'readmore;
            }
        }

        let line_cloned = line_buf.clone();
        let parser = Parser::new(Token::lexer(&line_cloned), symbol_table);

        for parse_result in parser {
            let parsed_expr = match parse_result {
                Ok(expr) => expr,
                Err(ParseError::UnexpectedEOF) => {
                    continue 'readmore;
                }
                Err(err) => {
                    println!("{err}");
                    line_buf.clear();
                    continue 'readmore;
                }
            };
            exprs.push(parsed_expr);
        }

        if !line_buf.trim().is_empty() {
            reader_interface.add_history_unique(line_buf.clone());
        }

        break Ok(ReplCommand::Eval(exprs.into_boxed_slice()));
    }
}

fn parse_repl_command(command_str: &str) -> Option<ReplCommand> {
    match command_str {
        "q" | "quit" => Some(ReplCommand::Quit),
        "h" | "help" => Some(ReplCommand::Help),
        "d" | "defined" => Some(ReplCommand::PrintDefined),
        _ => None,
    }
}
