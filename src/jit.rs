use core::panic;
use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift::prelude::{FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use crate::shared::Expr;

pub struct Generator {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
    verbose: bool
}

impl Generator {
    pub fn new(verbose: bool) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            verbose
        }
    }
    pub fn compile(&mut self, expr: &Expr) -> Result<*const u8, String> {
        match expr {
            Expr::Fn { name, .. } => {
                if self.verbose {
                    println!("> Building IR...");
                }


                // TODO: translate function AST into cranelift IR
                self.translate(expr)?;

                let id = self
                    .module
                    .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
                    .map_err(|e| e.to_string())?;

                self.module
                    .define_function(id, &mut self.ctx)
                    .map_err(|e| e.to_string())?;
                self.module.clear_context(&mut self.ctx);

                self.module.finalize_definitions().unwrap();

                let code = self.module.get_finalized_function(id);
                

                Ok(code)
            }
            _ => {
                panic!("JIT: attempt to compile non-function")
            }
        }
    }

    fn translate(&mut self, func: &Expr) -> Result<(), String> {
        match func {
            Expr::Fn { args, body, .. } => {
                if self.verbose {
                    println!(">> Generating function signature...")
                }
                let float = types::F64;
                for _p in args {
                    self.ctx.func.signature.params.push(AbiParam::new(float));
                }

                self.ctx.func.signature.returns.push(AbiParam::new(float));

                if self.verbose {
                    println!(">> Generating function...")
                }

                let mut builder =
                    FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
                let entry_block = builder.create_block();
                builder.append_block_params_for_function_params(entry_block);
                builder.switch_to_block(entry_block);
                builder.seal_block(entry_block);

                if self.verbose {
                    println!(">> Declaring variables...")
                }

                let variables = declare_variables(&mut builder, func, entry_block);
                
                if self.verbose {
                    for (i, (name, _)) in variables.iter().enumerate() {
                        println!(">>> Variable `{name}` ({i})")
                    }
                }
                
                let mut trans = FunctionTranslator {
                    int: types::F64,
                    builder,
                    variables,
                    module: &mut self.module,
                    just_returned: false,
                    verbose: self.verbose
                };
                
                if self.verbose {
                    println!(">> Generating code...")
                }

                let ret = trans.translate_expr(*body.clone());
                if !trans.just_returned {
                    trans.builder.ins().return_(&[ret]);
                }
                
                trans.builder.finalize();
            }
            _ => {
                panic!("Must translate a function")
            }
        }

        Ok(())
    }
}

fn declare_variables(
    builder: &mut FunctionBuilder,
    func: &Expr,
    entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    fn declare_variables_impl(
        builder: &mut FunctionBuilder,
        expr: Expr,
        vars: &mut HashMap<String, Variable>,
        block: Block,
        index: &mut usize,
    ) {
        match expr {
            Expr::Identifier(_) => {

            }
            Expr::Property(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                for p in rhs {
                    declare_variables_impl(builder, p, vars, block, index);
                }
            },
            Expr::List(items) => {
                for i in items {
                    declare_variables_impl(builder, i, vars, block, index);
                }
            },
            Expr::Neg(e) => {
                declare_variables_impl(builder, *e, vars, block, index);
            },

            Expr::Add(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::Eq(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::Index(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::NotEq(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::Not(e) => {
                declare_variables_impl(builder, *e, vars, block, index);
            },
            Expr::And(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::Or(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::Sub(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::Mul(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::Div(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
            Expr::If { condition, body, else_ } => {
                declare_variables_impl(builder, *condition, vars, block, index);
                declare_variables_impl(builder, *body, vars, block, index);
                if let Some(else_block) = else_ {
                    declare_variables_impl(builder, *else_block, vars, block, index);
                }
            },
            Expr::While { condition, body } => {
                declare_variables_impl(builder, *condition, vars, block, index);
                declare_variables_impl(builder, *body, vars, block, index);
            },
            Expr::Return(e) => {
                declare_variables_impl(builder, *e, vars, block, index);
            },
            Expr::ExpressionList(exprs, ret) => {
                for exp in exprs {
                    declare_variables_impl(builder, exp, vars, block, index);
                }
                declare_variables_impl(builder, *ret, vars, block, index);
            },
            Expr::Call(lhs, rhs) => {
                for exp in rhs {
                    declare_variables_impl(builder, exp, vars, block, index);
                }
                declare_variables_impl(builder, *lhs, vars, block, index);
            },
            Expr::Let { name, rhs } => {
                declare_variables_impl(builder, *rhs, vars, block, index);
                declare_variable(builder, vars, index, &name);
            },
            Expr::Fn { body, .. } => {
                declare_variables_impl(builder, *body, vars, block, index);
            },
            Expr::Global(_) => panic!("JIT should not need to compile from Global, instead it should be given functions to compile"),
            Expr::LessThan(lhs, rhs) => {
                declare_variables_impl(builder, *lhs, vars, block, index);
                declare_variables_impl(builder, *rhs, vars, block, index);
            },
        };
    }

    match &func {
        Expr::Fn { args, .. } => {
            for (i, name) in args.iter().enumerate() {
                let val = builder.block_params(entry_block)[i];
                let var = declare_variable(builder, &mut variables, &mut index, name);
                builder.def_var(var, val);
            }

            declare_variables_impl(
                builder,
                func.clone(),
                &mut variables,
                entry_block,
                &mut index,
            );
        }
        _ => {
            panic!("tried to jit compile declared variables for non function")
        }
    }
    variables
}

fn declare_variable(
    builder: &mut FunctionBuilder,
    vars: &mut HashMap<String, Variable>,
    index: &mut usize,
    name: &str,
) -> Variable {
    let var = Variable::new(*index);
    if !vars.contains_key(name) {
        vars.insert(name.into(), var);
        builder.declare_var(var, types::F64);
        *index += 1;
    }
    var
}

struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
    just_returned: bool,
    verbose: bool
}

impl<'a> FunctionTranslator<'a> {
    fn translate_expr(&mut self, expr: Expr) -> Value {
        self.just_returned = false;
        match expr {
            Expr::Identifier(id) => match id {
                crate::shared::Value::Number(x) => self.builder.ins().f64const(x),
                crate::shared::Value::String(name) => {
                    let variable = self
                        .variables
                        .get(&name)
                        .expect("attempt to get undefined variable");
                    self.builder.use_var(*variable)
                }
                _ => panic!("JIT: Only supports numeric literals and variables"),
            },
            Expr::Property(name, properties) => {
                if properties.len() != 0 {
                    panic!("JIT: properties not supported. ex. the standard library");
                }

                self.translate_expr(*name)
            }
            Expr::List(_) => todo!("JIT: lists are unsupported"),
            Expr::Neg(e) => {
                let val = self.translate_expr(*e);
                let neg1 = self.builder.ins().f64const(-1.0);

                self.builder.ins().fmul(val, neg1)
            }
            Expr::Add(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                self.builder.ins().fadd(lhs, rhs)
            }
            Expr::Eq(lhs, rhs) => self.fcmp(FloatCC::Equal, *lhs, *rhs),
            Expr::LessThan(lhs, rhs) => self.fcmp(FloatCC::LessThan, *lhs, *rhs),
            Expr::Index(_, _) => todo!("JIT: index not supported"),
            Expr::NotEq(lhs, rhs) => self.fcmp(FloatCC::NotEqual, *lhs, *rhs),
            Expr::Not(e) => {
                // This is branchless, and works on floats, so it isn't perfect, the value really has to be either 0 or 1, but we can't check this
                let e = self.translate_expr(*e);
                let one = self.builder.ins().f64const(1.0);

                self.builder.ins().fsub(one, e)
            }
            Expr::And(lhs, rhs) => {
                let (lhs, rhs) = self.bools(*lhs, *rhs);
                let res = self.builder.ins().band(lhs, rhs);

                self.builder.ins().fcvt_from_uint(types::F64, res)
            }
            Expr::Or(lhs, rhs) => {
                let (lhs, rhs) = self.bools(*lhs, *rhs);
                let res = self.builder.ins().bor(lhs, rhs);

                self.builder.ins().fcvt_from_uint(types::F64, res)
            }
            Expr::Sub(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                self.builder.ins().fsub(lhs, rhs)
            }
            Expr::Mul(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                self.builder.ins().fmul(lhs, rhs)
            }
            Expr::Div(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                self.builder.ins().fdiv(lhs, rhs)
            }
            Expr::If {
                condition,
                body,
                else_,
            } => {
                let condition_value = self.bool(*condition);
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                self.builder.append_block_param(merge_block, self.int);

                self.builder
                    .ins()
                    .brif(condition_value, then_block, &[], else_block, &[]);
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_return = self.translate_expr(*body);

                if !self.just_returned {
                    self.builder.ins().jump(merge_block, &[then_return]);
                }
                

                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let mut else_return = self.builder.ins().f64const(0.0);

                self.just_returned = false;

                if let Some(else_) = else_ {
                    else_return = self.translate_expr(*else_);
                }

                if !self.just_returned {
                    self.builder.ins().jump(merge_block, &[else_return]);
                }
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);

                let phi = self.builder.block_params(merge_block)[0];

                phi
            }
            Expr::While { condition, body } => {
                let header_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                self.builder.ins().jump(header_block, &[]);
                self.builder.switch_to_block(header_block);

                let condition_value = self.bool(*condition);
                self.builder
                    .ins()
                    .brif(condition_value, body_block, &[], exit_block, &[]);

                self.builder.switch_to_block(body_block);
                

                let ret = self.translate_expr(*body);
                if !self.just_returned {
                    self.builder.ins().jump(header_block, &[]);
                }
                
                self.builder.seal_block(body_block);
                self.builder.switch_to_block(exit_block);

                self.builder.seal_block(header_block);
                self.builder.seal_block(exit_block);

                ret
            }
            Expr::Return(ret) => {
                let val = self.translate_expr(*ret);
                self.builder.ins().return_(&[val]);
                self.just_returned = true;
                val
            }
            Expr::ExpressionList(exprs, ret) => {
                if self.verbose {
                    println!(">> Expression list with count: {}", exprs.len() + 1);
                }
                for e in exprs {
                    self.translate_expr(e);
                }

                self.translate_expr(*ret)
            }
            Expr::Call(e, args) => {
                if let crate::Expr::Property(prefix, _) = *e {
                    if let crate::Expr::Identifier(id) = *prefix {
                        if let crate::Value::String(name) = id {
                            let mut sig = self.module.make_signature();
                            for _arg in &args {
                                sig.params.push(AbiParam::new(self.int))
                            }

                            sig.returns.push(AbiParam::new(self.int));

                            let callee = self.module
                                .declare_function(&name, Linkage::Import, &sig).expect("error whilst declaring function call");
                            let local_calle = self.module.declare_func_in_func(callee, self.builder.func);

                            let mut arg_values = Vec::new();
                            for arg in args {
                                arg_values.push(self.translate_expr(arg));
                            }

                            let call = self.builder.ins().call(local_calle, &arg_values);
                            return self.builder.inst_results(call)[0];
                        }
                    }
                }
                panic!("JIT: Cannot dynamically call functions")
            },
            Expr::Let { name, rhs } => {
                let value = self.translate_expr(*rhs);
                let variable = self
                    .variables
                    .get(&name)
                    .expect("JIT: internal error, variable not found. Should not happen");
                self.builder.def_var(*variable, value);
                value
            }
            Expr::Fn { .. } => {
                panic!("JIT: function expr should not be JIT compiled directly")
            }
            Expr::Global(_) => panic!("JIT: global expr should not be JIT compiled directly"),
        }
    }

    fn fcmp(&mut self, compare: FloatCC, lhs: Expr, rhs: Expr) -> Value {
        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);
        let res = self.builder.ins().fcmp(compare, lhs, rhs);

        self.builder.ins().fcvt_from_uint(types::F64, res)
    }

    fn bools(&mut self, lhs: Expr, rhs: Expr) -> (Value, Value) {
        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);

        let lval = self.builder.ins().fcvt_to_uint(types::I64, lhs);
        let rval = self.builder.ins().fcvt_to_uint(types::I64, rhs);

        let const_one = self.builder.ins().iconst(types::I64, 1);

        let compare_1 = self.builder.ins().icmp(IntCC::Equal, lval, const_one);
        let compare_2 = self.builder.ins().icmp(IntCC::Equal, rval, const_one);

        (compare_1, compare_2)
    }

    fn bool(&mut self, lhs: Expr) -> Value {
        let lhs = self.translate_expr(lhs);
        let lval = self.builder.ins().fcvt_to_uint(types::I64, lhs);
        let const_one = self.builder.ins().iconst(types::I64, 1);
        let compare = self.builder.ins().icmp(IntCC::Equal, lval, const_one);

        compare
    }
}
