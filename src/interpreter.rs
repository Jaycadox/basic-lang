use crate::shared::*;
use std::sync::{Arc, Mutex};

fn generate_vars() -> Vec<Expr> {
    vec![Expr::Identifier(crate::library::generate_library())]
}

pub fn eval<'a>(
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
            return if eval(lhs, vars, funcs, ret)?.num() < eval(rhs, vars, funcs, ret)?.num()
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
