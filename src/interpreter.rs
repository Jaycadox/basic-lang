use crate::shared::*;
use std::sync::{Arc, Mutex};

fn generate_vars() -> Vec<Expr> {
    vec![Expr::Identifier(crate::library::generate_library())]
}

pub struct State {
    vars: Vec<(String, Value)>,
    funcs: Vec<Expr>,
    ret: Option<Value>,
    limited_enviorment: bool,
    ticks: (u64, u64)
}

impl State {
    pub fn new() -> Self {
        Self {
            vars: Vec::new(),
            funcs: Vec::new(),
            ret: None,
            limited_enviorment: false,
            ticks: (0, 0)
        }
    }

    pub fn limited_enviorment(max_ticks: u64) -> Self {
        Self {
            vars: Vec::new(),
            funcs: Vec::new(),
            ret: None,
            limited_enviorment: true,
            ticks: (0, max_ticks)
        }
    }
}

pub fn eval_repl<'a>(
    mut expr: Expr,
    repl: Expr,
    state: &'a mut State,
    jit: bool
) -> Result<Value, String> {

    if let Expr::Global(gbl) = &mut expr {
        for expr in gbl {
            if let Expr::Fn { name, body, .. } = expr {
                if name == "main" {
                    *body = Box::new(repl);
                    break;
                }
            }
        }
        if jit {
            return crate::shared::run_jit(expr.clone(), false, false, false);
        }

        return eval(&expr.clone(), state);
    }
    Err("eval_repl expects root to be Global".into())
}

pub fn eval<'a>(
    expr: &'a Expr,
    state: &'a mut State
) -> Result<Value, String> {
    
    if state.limited_enviorment {
        if state.ticks.0 >= state.ticks.1 {
            return Err("Too many ticks!".into());
        }
    }

    state.ticks.0 += 1;

    match expr {
        Expr::Global(exprs) => {
            for expr in exprs {
                eval(expr, state)?;
            }
            let call = Expr::Call(
                Box::new(Expr::Property(
                    Box::new(Expr::Identifier(Value::String("main".to_owned()))),
                    vec![],
                )),
                generate_vars(),
            );
            let vars = state.vars.clone();
            let funcs = state.funcs.clone();
            let ret = state.ret.clone();
            let limited_enviorment = state.limited_enviorment;
            let ticks = state.ticks;
            eval(&call, &mut State { vars , funcs, ret, limited_enviorment, ticks })
        }
        Expr::Fn { .. } => {
            state.funcs.push(expr.clone());
            Ok(Value::String("function".to_owned()))
        }
        Expr::Return(expr) => {
            let ret_val = eval(expr, state)?;
            state.ret = Some(ret_val.clone());
            return Ok(ret_val);
        }
        Expr::List(items) => {
            let item_values = items
                .iter()
                .map(|i| eval(i, state).unwrap().clone());
            Ok(Value::List(Arc::new(Mutex::new(item_values.collect()))))
        }
        Expr::Index(val, i) => eval(val, state)?
            .index(&eval(i, state)?)
            .ok_or("tried to index non indexable item".to_owned()),
        Expr::Eq(lhs, rhs) => {
            return if eval(lhs, state)?.string() == eval(rhs, state)?.string()
            {
                Ok(Value::Number(1.0))
            } else {
                Ok(Value::Number(0.0))
            }
        }
        Expr::LessThan(lhs, rhs) => {
            return if eval(lhs, state)?.num() < eval(rhs, state)?.num()
            {
                Ok(Value::Number(1.0))
            } else {
                Ok(Value::Number(0.0))
            }
        }
        Expr::Not(val) => Ok(Value::Boolean(!eval(val, state)?.is_true())),
        Expr::And(lhs, rhs) => Ok(Value::Boolean(
            eval(lhs, state)?.is_true() && eval(rhs, state)?.is_true(),
        )),
        Expr::Or(lhs, rhs) => Ok(Value::Boolean(
            eval(lhs, state)?.is_true() || eval(rhs, state)?.is_true(),
        )),
        Expr::NotEq(lhs, rhs) => {
            return if eval(lhs, state)?.string() != eval(rhs, state)?.string()
            {
                Ok(Value::Number(1.0))
            } else {
                Ok(Value::Number(0.0))
            }
        }
        Expr::Identifier(name) => Ok(name.to_owned()),
        Expr::Property(obj, property) => {
            if property.len() != 0 && state.limited_enviorment {
                return Err("Attempt to access external property from limited enviorment".into());
            }

            let obj_name = eval(obj, state)?.string();

            for func in state.funcs.iter() {
                match func {
                    Expr::Fn { name, .. } => {
                        if name == &obj_name {
                            return Ok(Value::Function(Box::new(func.to_owned().clone())));
                        }
                    }
                    _ => {}
                }
            }
            for (name, val) in state.vars.iter().rev() {
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

            for (name, val) in state.vars.iter() {
                if *name == obj_name {
                    if let Value::ExternalFunctionLibrary(lib) = val.clone() {
                        let key = property
                            .iter()
                            .map(|e| eval(e, state).unwrap().string())
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
            let old_vars = state.vars.clone();
            let fn_name = name;
            let fn_args_len = args.len();
            let fn_args = args;

            let func_result = eval(fn_name, state)?;
            match func_result {
                Value::ExternalFunction(efunc) => {
                    return efunc(
                        args.iter()
                            .map(|e| eval(e, state).unwrap())
                            .collect(),
                    );
                }
                Value::Function(func) => match *func {
                    Expr::Fn { name, .. } => {
                        let nfunc_name = name;
                        for func in state.funcs.clone() {
                            match func {
                                Expr::Fn { name, args, body } => {
                                    if *name != nfunc_name {
                                        continue;
                                    }

                                    for (arg_name, arg_expr) in std::iter::zip(args, fn_args) {
                                        let val = eval(arg_expr, state)?.clone();
                                        state.vars.push((
                                            arg_name.to_owned(),
                                            val,
                                        ));
                                    }
                                    let result = eval(&body, state);
                                    state.ret = None;
                                    state.vars = old_vars;
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
            let res = eval(&condition, state)?;
            if res.is_true() {
                return eval(&body, state);
            }
            if let Some(else_block) = else_ {
                return eval(&else_block, state);
            }

            Ok(Value::Number(0.0))
        }

        Expr::While { condition, body } => {
            while eval(&condition, state)?.is_true() {
                eval(&body, state)?;
                if let Some(val) = &state.ret {
                    return Ok(val.to_owned());
                }
            }
            Ok(Value::Number(0.0))
        }

        Expr::Neg(a) => Ok(Value::Number(-eval(a, state)?.num())),
        Expr::Add(a, b) => Ok(Value::Number(
            eval(a, state)?.num() + eval(b, state)?.num(),
        )),
        Expr::Sub(a, b) => Ok(Value::Number(
            eval(a, state)?.num() - eval(b, state)?.num(),
        )),
        Expr::Mul(a, b) => Ok(Value::Number(
            eval(a, state)?.num() * eval(b, state)?.num(),
        )),
        Expr::Div(a, b) => Ok(Value::Number(
            eval(a, state)?.num() / eval(b, state)?.num(),
        )),
        Expr::Let { name, rhs } => {
            let rhs = eval(rhs, state)?;
            state.vars.push((name.to_owned(), rhs.clone()));
            Ok(rhs)
        }
        Expr::ExpressionList(list, then) => {
            for expr in list {
                match expr {
                    Expr::Return(expr) => {
                        return eval(expr, state);
                    }
                    _ => {
                        eval(expr, state)?;
                        if state.ret.is_some() {
                            return Ok(state.ret.clone().unwrap());
                        }
                    }
                }
            }
            eval(then, state)
        }
    }
}
