use clap::Parser;
use serde_derive::*;
use std::sync::{Arc, Mutex};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Box<Expr>),
    List(Arc<Mutex<Vec<Value>>>),
    #[serde(skip_serializing, skip_deserializing)]
    ExternalFunction(fn(Vec<Value>) -> Result<Value, String>),
    ExternalFunctionLibrary(std::collections::HashMap<Vec<String>, Box<Value>>),
}

impl Value {
    pub fn num(&self) -> f64 {
        if let Self::Number(n) = self {
            return *n;
        }
        panic!("Runtime error: attempt to pull number value out of non-number")
    }
    pub fn string(&self) -> String {
        return match self {
            Self::String(n) => n.to_owned(),
            Self::ExternalFunction(f) => format!("external function: {f:?}"),
            Self::ExternalFunctionLibrary(l) => format!("external function library: {l:?}"),
            Self::Number(n) => n.to_string().to_owned(),
            Self::Function(f) => format!("internal function: {f:?}"),
            Self::Boolean(b) => {
                if *b {
                    format!("true")
                } else {
                    format!("fasle")
                }
            }
            Self::List(l) => format!("list: {:?}", l.lock().unwrap()),
        };
    }
    pub fn is_true(&self) -> bool {
        return match &self {
            Value::Number(n) => *n == 1.0,
            Value::Boolean(b) => *b,
            _ => false,
        };
    }
    pub fn index(&self, i: &Self) -> Option<Self> {
        let i = i.num() as usize;
        return match &self {
            Value::String(n) => Some(Value::String((n.chars().nth(i)).unwrap().to_string())),
            Value::List(n) => Some(n.lock().unwrap()[i].to_owned()),
            _ => None,
        };
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    Identifier(Value),
    Property(Box<Expr>, Vec<Expr>),
    List(Vec<Expr>),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    If {
        condition: Box<Expr>,
        body: Box<Expr>,
        else_: Option<Box<Expr>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Expr>,
    },
    Return(Box<Expr>),
    ExpressionList(Vec<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Let {
        name: String,
        rhs: Box<Expr>,
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Box<Expr>,
    },
    Global(Vec<Expr>),
}

#[derive(Parser, Debug)]
pub struct Args {
    /// Input file
    #[arg(short, long)]
    pub input: String,

    /// Serialize the file to AST bytecode
    #[arg(short, long, default_value_t = false)]
    pub serialize: bool,

    /// Deserialize a script from AST bytecode
    #[arg(short, long, default_value_t = false)]
    pub deserialize: bool,

    /// Experimental JIT compiler
    #[arg(short, long, default_value_t = false)]
    pub jit: bool,

    /// Verbose logging
    #[arg(short, long, default_value_t = false)]
    pub verbose: bool,
}

impl Args {
    pub fn get() -> Self {
        Self::parse()
    }
}
