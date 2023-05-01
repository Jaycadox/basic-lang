mod jit;
mod library;
mod shared;
mod interpreter;
mod parser;

use chumsky::prelude::*;
use iced_x86::{Formatter, Decoder, DecoderOptions, NasmFormatter, Instruction};
use shared::*;

fn main() {
    let args = shared::Args::get();
    let ast_out = parser::parser().parse(std::fs::read_to_string(&args.input).unwrap());
    match ast_out {
        Ok(ast) => {
            if args.jit {
                run_jit(ast, args.verbose);
            } else {
                run_interpreted(ast)
            }
        }
        Err(e) => {
            e.into_iter().for_each(|e| println!("Parser error: {e}"));
        } 
    };
}

fn run_jit(ast: Expr, verbose: bool) {
    if verbose {
        println!("Using mode: JIT");
    }
    if let Expr::Global(gbl) = ast {
        let mut generator = jit::Generator::new(verbose);
        let mut main_func: Option<fn() -> f64> = None;

        for func in &gbl {
            if let Expr::Fn { name, .. } = func {
                if verbose {
                    println!("> {name} ...");
                }
                let (out, size) = generator.compile(func).unwrap();
                if verbose {
                    println!("> {name} 0x{:X} | {} bytes", out as usize, size);
                }

                if verbose {
                    println!("> Disassembly (given x86-64):");
                    unsafe {
                        let data = std::slice::from_raw_parts(out.clone(), size as usize);
                        let mut decoder = Decoder::with_ip(64, data, out.clone() as u64, DecoderOptions::NONE);

                        let mut formatter = NasmFormatter::new();
                        let mut output = String::new();
                        
                        let mut instruction = Instruction::new();
                        while decoder.can_decode() {
                            output.clear();
                            decoder.decode_out(&mut instruction);
                            formatter.format(&instruction, &mut output);
                            println!(">>    {}", output);
                        }
                    }
                }

                if name == "main" {
                    unsafe {
                        let code_fn = std::mem::transmute::<_, fn() -> f64>(out);
                        main_func = Some(code_fn);
                    }
                }
            }
        }
        if let Some(mfunc) = main_func {
            let ret = mfunc();
            println!("Returned: {ret}");
        } else {
            panic!("No main function found")
        }
    }
    return;
}

fn run_interpreted(ast: Expr) {
    let mut vars = Vec::<_>::new();
    match interpreter::eval(&ast, &mut vars, &mut Vec::<_>::new(), &mut None) {
            Ok(out) => println!("Returned: {out:?}"),
            Err(err) => println!("Runtime error: {err}"),
    };
}