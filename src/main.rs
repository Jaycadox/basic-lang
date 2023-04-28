mod jit;
mod library;
mod shared;

use chumsky::prelude::*;
use shared::*;
use std::io::Write;
use std::sync::{Arc, Mutex};

fn generate_vars() -> Vec<Expr> {
    vec![Expr::Identifier(library::generate_library())]
}

fn main() {
    let args = shared::Args::get();
    let ast_out = if args.deserialize {
        Ok(bincode::deserialize(&std::fs::read(&args.input).unwrap()).unwrap())
    } else {
        parser().parse(std::fs::read_to_string(&args.input).unwrap())
    };

    if args.serialize {
        if let Ok(expr) = ast_out {
            let bin = bincode::serialize(&expr).unwrap();
            let file_path = format!("{}.compiled", args.input);
            std::fs::File::create(&file_path)
                .unwrap()
                .write(&bin)
                .unwrap();
            println!("Compiled to: {file_path}");
            return;
        } else {
            println!("Parsing failed.");
            return;
        }
    }
    if args.jit {
        if args.verbose {
            println!("Using mode: JIT");
        }
        if let Expr::Global(gbl) = ast_out.unwrap() {
            let mut generator = jit::Generator::new(args.verbose);
            let mut main_func: Option<fn() -> f64> = None;
            for func in &gbl {
                if let Expr::Fn { name, .. } = func {
                    if args.verbose {
                        println!("> {name} ...");
                    }
                    let out = generator.compile(func).unwrap();
                    if args.verbose {
                        println!("> {name} 0x{:X}", out as usize);
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

    let mut vars = Vec::<_>::new();
    match ast_out {
        Ok(ast) => match eval(&ast, &mut vars, &mut Vec::<_>::new(), &mut None) {
            Ok(out) => println!("Returned: {out:?}"),
            Err(err) => println!("Runtime error: {err}"),
        },
        Err(error) => error
            .into_iter()
            .for_each(|e| println!("Parser error: {e}")),
    }
}

fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident().padded();

    let expr = recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Identifier(Value::Number(s.parse().unwrap())))
            .padded()
            .or(text::keyword("true").map(|_| Expr::Identifier(Value::Boolean(true))))
            .or(text::keyword("false").map(|_| Expr::Identifier(Value::Boolean(false))))
            .or(just('"')
                .ignore_then(none_of('"').repeated())
                .then_ignore(just('"'))
                .map(|s| Expr::Identifier(Value::String(s.into_iter().collect()))))
            .padded();
        let ident = ident
            .clone()
            .then(just('.').ignore_then(ident).repeated())
            .map(|(x, y)| {
                Expr::Property(
                    Box::new(Expr::Identifier(Value::String(x))),
                    y.iter()
                        .map(|p| Expr::Identifier(Value::String(p.to_owned())))
                        .collect(),
                )
            })
            .or(ident.clone().map(|i| Expr::Identifier(Value::String(i))));
        let call = ident
            .then(
                expr.clone()
                    .separated_by(just(','))
                    .allow_trailing()
                    .delimited_by(just('(').padded(), just(')').padded()),
            )
            .map(|(name, args)| Expr::Call(Box::new(name), args));

        let op = |c| just(c).padded();
        let op_str = |c| just(c).padded();

        let atom = int
            .or(expr.clone().delimited_by(op('('), op(')')))
            .or(op('!')
                .ignore_then(expr.clone())
                .map(|val| Expr::Not(Box::new(val))))
            .or(expr
                .clone()
                .separated_by(op(','))
                .allow_trailing()
                .delimited_by(op('['), op(']'))
                .map(|items| Expr::List(items)))
            .or(expr
                .clone()
                .then(expr.clone().delimited_by(op('['), op(']')))
                .delimited_by(op('('), op(')'))
                .map(|(expr, i)| Expr::Index(Box::new(expr), Box::new(i))))
            .or(expr
                .clone()
                .then_ignore(op_str("&&").padded())
                .then(expr.clone())
                .delimited_by(op('('), op(')'))
                .map(|(lhs, rhs)| Expr::And(Box::new(lhs), Box::new(rhs))))
            .or(expr
                .clone()
                .then_ignore(op_str("||").padded())
                .then(expr.clone())
                .delimited_by(op('('), op(')'))
                .map(|(lhs, rhs)| Expr::Or(Box::new(lhs), Box::new(rhs))))
            .or(expr
                .clone()
                .then_ignore(op_str("==").padded())
                .then(expr.clone())
                .delimited_by(op('('), op(')'))
                .map(|(lhs, rhs)| Expr::Eq(Box::new(lhs), Box::new(rhs))))
            .or(expr
                .clone()
                .then_ignore(op_str("<").padded())
                .then(expr.clone())
                .delimited_by(op('('), op(')'))
                .map(|(lhs, rhs)| Expr::LessThan(Box::new(lhs), Box::new(rhs))))
            .or(expr
                .clone()
                .then_ignore(op_str("!=").padded())
                .then(expr.clone())
                .delimited_by(op('('), op(')'))
                .map(|(lhs, rhs)| Expr::NotEq(Box::new(lhs), Box::new(rhs))))
            .or(call)
            .or(ident);

        let unary = op('-')
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                op('*')
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(op('/').to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                op('+')
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
        sum
    });
    let decl = recursive(|decl| {
        let let_ = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(decl.clone())
            .map(|(name, rhs)| Expr::Let {
                name,
                rhs: Box::new(rhs),
            });

        let expression = let_
            .or(expr.clone())
            .or(decl
                .clone()
                .then_ignore(just(';'))
                .repeated()
                .then(decl.clone())
                .delimited_by(just('{'), just('}'))
                .map(|(list, decl)| Expr::ExpressionList(list, Box::new(decl))))
            .padded();

        let if_ = text::keyword("if")
            .ignore_then(decl.clone())
            .then(decl.clone())
            .then(text::keyword("else").ignore_then(decl.clone()).or_not())
            .map(|((condition, body), else_)| Expr::If {
                condition: Box::new(condition),
                body: Box::new(body),
                else_: if else_.is_some() {
                    Some(Box::new(else_.unwrap()))
                } else {
                    None
                },
            });

        let while_ = text::keyword("while")
            .ignore_then(decl.clone())
            .then(decl.clone())
            .map(|(condition, body)| Expr::While {
                condition: Box::new(condition),
                body: Box::new(body),
            });

        let expression = if_.or(while_).or(expression).padded();
        let return_ = text::keyword("return")
            .ignore_then(decl.clone())
            .map(|value| Expr::Return(Box::new(value)));

        return_.or(expression).padded()
    });
    let func = text::keyword("func")
        .or_not()
        .ignore_then(ident)
        .then(
            ident
                .separated_by(just(','))
                .allow_trailing()
                .delimited_by(just('(').or_not(), just(')').or_not())
                .or_not(),
        )
        .then_ignore(just('=').or_not().padded())
        .then(decl.clone())
        .then_ignore(just(';').or_not())
        .map(|((name, args), body)| Expr::Fn {
            name: name,
            args: args.unwrap_or(Vec::new()),
            body: Box::new(body),
        });

    let globals = func
        .clone()
        .or(decl.then_ignore(just(';').padded()))
        .repeated()
        .map(|funcs| Expr::Global(funcs));

    globals.then_ignore(end())
}

fn eval<'a>(
    expr: &'a Expr,
    vars: &mut Vec<(String, Value)>,
    funcs: &mut Vec<&'a Expr>,
    ret: &mut Option<Value>,
) -> Result<Value, String> {
    //println!("{vars:#?}");
    match expr {
        Expr::Global(exprs) => {
            for expr in exprs {
                eval(expr, vars, funcs, ret)?;
            }
            let call = Expr::Call(
                Box::new(Expr::Property(
                    Box::new(Expr::Identifier(Value::String("main".to_owned()))),
                    vec![],
                )),
                generate_vars(),
            );
            eval(&call, &mut vars.clone(), &mut funcs.clone(), ret)
        }
        Expr::Fn { .. } => {
            funcs.push(expr);
            Ok(Value::String("function".to_owned()))
        }
        Expr::Return(expr) => {
            let ret_val = eval(expr, vars, funcs, ret)?;
            *ret = Some(ret_val.clone());
            return Ok(ret_val);
        }
        Expr::List(items) => {
            let item_values = items
                .iter()
                .map(|i| eval(i, vars, funcs, ret).unwrap().clone());
            Ok(Value::List(Arc::new(Mutex::new(item_values.collect()))))
        }
        Expr::Index(val, i) => eval(val, vars, funcs, ret)?
            .index(&eval(i, vars, funcs, ret)?)
            .ok_or("tried to index non indexable item".to_owned()),
        Expr::Eq(lhs, rhs) => {
            return if eval(lhs, vars, funcs, ret)?.string() == eval(rhs, vars, funcs, ret)?.string()
            {
                Ok(Value::Number(1.0))
            } else {
                Ok(Value::Number(0.0))
            }
        }
        Expr::LessThan(lhs, rhs) => {
            return if eval(lhs, vars, funcs, ret)?.string() < eval(rhs, vars, funcs, ret)?.string()
            {
                Ok(Value::Number(1.0))
            } else {
                Ok(Value::Number(0.0))
            }
        }
        Expr::Not(val) => Ok(Value::Boolean(!eval(val, vars, funcs, ret)?.is_true())),
        Expr::And(lhs, rhs) => Ok(Value::Boolean(
            eval(lhs, vars, funcs, ret)?.is_true() && eval(rhs, vars, funcs, ret)?.is_true(),
        )),
        Expr::Or(lhs, rhs) => Ok(Value::Boolean(
            eval(lhs, vars, funcs, ret)?.is_true() || eval(rhs, vars, funcs, ret)?.is_true(),
        )),
        Expr::NotEq(lhs, rhs) => {
            return if eval(lhs, vars, funcs, ret)?.string() != eval(rhs, vars, funcs, ret)?.string()
            {
                Ok(Value::Number(1.0))
            } else {
                Ok(Value::Number(0.0))
            }
        }
        Expr::Identifier(name) => Ok(name.to_owned()),
        Expr::Property(obj, property) => {
            let obj_name = eval(obj, vars, funcs, ret)?.string();

            for func in funcs.iter() {
                match func {
                    Expr::Fn { name, .. } => {
                        if name == &obj_name {
                            return Ok(Value::Function(Box::new(func.to_owned().clone())));
                        }
                    }
                    _ => {}
                }
            }
            for (name, val) in vars.iter().rev() {
                if *name == obj_name {
                    if property.len() != 0 {
                        if !matches!(val, Value::ExternalFunctionLibrary(_)) {
                            return Err(format!(
                                "Attempted to get property `{:?}` from `{:?}`",
                                property, obj
                            ));
                        }
                    } else {
                        return Ok(val.to_owned());
                    }
                }
            }

            for (name, val) in vars.iter() {
                if *name == obj_name {
                    if let Value::ExternalFunctionLibrary(lib) = val.clone() {
                        let key = property
                            .iter()
                            .map(|e| eval(e, vars, funcs, ret).unwrap().string())
                            .collect::<Vec<String>>();
                        if lib.contains_key(&key) {
                            let func = lib[&key].clone();
                            return Ok(*func);
                        } else {
                            return Err(format!("Unknown property of function-library"));
                        }
                    } else {
                        return Err(format!("Tried to get property of non-function-library"));
                    }
                }
            }
            Err(format!(
                "Can't resolve property `{:?}` of `{:?}`",
                property, obj
            ))
        }
        Expr::Call(name, args) => {
            let mut old_vars = vars.clone();
            let fn_name = name;
            let fn_args_len = args.len();
            let fn_args = args;

            let func_result = eval(fn_name, vars, funcs, ret)?;
            match func_result {
                Value::ExternalFunction(efunc) => {
                    return efunc(
                        args.iter()
                            .map(|e| eval(e, vars, funcs, ret).unwrap())
                            .collect(),
                    );
                }
                Value::Function(func) => match *func {
                    Expr::Fn { name, .. } => {
                        let nfunc_name = name;
                        for func in funcs.clone() {
                            match func {
                                Expr::Fn { name, args, body } => {
                                    if *name != nfunc_name {
                                        continue;
                                    }

                                    for (arg_name, arg_expr) in std::iter::zip(args, fn_args) {
                                        vars.push((
                                            arg_name.to_owned(),
                                            eval(arg_expr, &mut old_vars, funcs, ret)?,
                                        ));
                                    }
                                    let result = eval(body, vars, funcs, ret);
                                    *ret = None;
                                    *vars = old_vars;
                                    return result;
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                },
                x => return Err(format!("Tried to call non-function: `{:?}`", x)),
            }

            Err(format!(
                "Couldn't find function: `{name:?}` with {} arg/s.",
                fn_args_len
            ))
        }
        Expr::If {
            condition,
            body,
            else_,
        } => {
            let res = eval(&condition, vars, funcs, ret)?;
            if res.is_true() {
                return eval(&body, vars, funcs, ret);
            }
            if let Some(else_block) = else_ {
                return eval(&else_block, vars, funcs, ret);
            }

            Ok(Value::Number(0.0))
        }

        Expr::While { condition, body } => {
            while eval(&condition, vars, funcs, ret)?.is_true() {
                eval(&body, vars, funcs, ret)?;
                if let Some(val) = ret {
                    return Ok(val.to_owned());
                }
            }
            Ok(Value::Number(0.0))
        }

        Expr::Neg(a) => Ok(Value::Number(-eval(a, vars, funcs, ret)?.num())),
        Expr::Add(a, b) => Ok(Value::Number(
            eval(a, vars, funcs, ret)?.num() + eval(b, vars, funcs, ret)?.num(),
        )),
        Expr::Sub(a, b) => Ok(Value::Number(
            eval(a, vars, funcs, ret)?.num() - eval(b, vars, funcs, ret)?.num(),
        )),
        Expr::Mul(a, b) => Ok(Value::Number(
            eval(a, vars, funcs, ret)?.num() * eval(b, vars, funcs, ret)?.num(),
        )),
        Expr::Div(a, b) => Ok(Value::Number(
            eval(a, vars, funcs, ret)?.num() / eval(b, vars, funcs, ret)?.num(),
        )),
        Expr::Let { name, rhs } => {
            let rhs = eval(rhs, vars, funcs, ret)?;
            vars.push((name.to_owned(), rhs.clone()));
            Ok(rhs)
        }
        Expr::ExpressionList(list, then) => {
            for expr in list {
                match expr {
                    Expr::Return(expr) => {
                        return eval(expr, vars, funcs, ret);
                    }
                    _ => {
                        eval(expr, vars, funcs, ret)?;
                        if ret.is_some() {
                            return Ok(ret.clone().unwrap());
                        }
                    }
                }
            }
            eval(then, vars, funcs, ret)
        }
    }
}
