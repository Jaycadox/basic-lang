mod jit;
mod library;
mod shared;
mod interpreter;
mod parser;

use chumsky::prelude::*;
use shared::*;

fn main() {
    let args = shared::Args::get();
    let ast_out = parser::parser().parse(std::fs::read_to_string(&args.input).unwrap());
    match ast_out {
        Ok(ast) => {
            println!("Returned: {:?}", if args.jit { run_jit(ast, args.verbose, args.evaluate_constants, args.meta_jit_evaluate_constants).unwrap() } else { run_interpreted(ast) });
        }
        Err(e) => {
            e.into_iter().for_each(|e| println!("Parser error: {e}"));
        } 
    };
}