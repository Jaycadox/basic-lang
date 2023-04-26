use crate::shared::Value;
use dynfmt::Format;
use rand::Rng;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub fn generate_library() -> crate::shared::Value {
    let mut lib = HashMap::new();

    lib.insert(
        vec!["println".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            println!("{}", args[0].string());
            Ok(Value::Number(0.0))
        })),
    );
    lib.insert(
        vec!["print".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            print!("{}", args[0].string());
            Ok(Value::Number(0.0))
        })),
    );

    lib.insert(
        vec!["readln".to_owned()],
        Box::new(Value::ExternalFunction(|_| {
            let mut line = String::new();
            return match std::io::stdin().read_line(&mut line) {
                Ok(_) => Ok(Value::String(line.replace('\n', "").replace('\r', ""))),
                Err(e) => Err(format!("readline error: {}", e)),
            };
        })),
    );

    lib.insert(
        vec!["read".to_owned()],
        Box::new(Value::ExternalFunction(|_| {
            Ok(Value::String(
                console::Term::stdout().read_char().unwrap().to_string(),
            ))
        })),
    );

    lib.insert(
        vec!["strlen".to_owned()],
        Box::new(Value::ExternalFunction(|args| match &args.get(0) {
            Some(arg) => match arg {
                Value::String(s) => return Ok(Value::Number(s.len() as f64)),
                _ => return Err(format!("strlen called on: {:?}", args[0])),
            },
            _ => return Err(format!("strlen arguments are empty")),
        })),
    );

    lib.insert(
        vec!["error".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            let err = args.get(0);
            return match err {
                Some(v) => Err(v.string()),
                _ => Err("user called error".to_owned()),
            };
        })),
    );

    lib.insert(
        vec!["str".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() != 1 {
                return Err(format!("str can only have 1 argument"));
            }
            Ok(Value::String(args[0].string()))
        })),
    );

    lib.insert(
        vec!["char_from_ascii".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() != 1 {
                return Err(format!("char_from_ascii can only have 1 argument"));
            }
            Ok(Value::String(
                (char::from_u32(args[0].num() as u32)).unwrap().to_string(),
            ))
        })),
    );

    lib.insert(
        vec!["ascii_from_char".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() != 1 {
                return Err(format!("ascii_from_char can only have 1 argument"));
            }
            Ok(Value::Number(
                args[0].string().bytes().nth(0).unwrap() as f64
            ))
        })),
    );

    lib.insert(
        vec!["format".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() == 0 {
                return Err(format!("err arguments are empty"));
            }

            let formatted_args = args[1..]
                .iter()
                .map(|s| format!("{}", s.string()))
                .collect::<Vec<String>>();
            match dynfmt::SimpleCurlyFormat.format(&args[0].string(), formatted_args) {
                Ok(s) => Ok(Value::String(s.to_string())),
                Err(e) => Err(format!("format error: `{}`", e)),
            }
        })),
    );

    // Rand functions
    lib.insert(
        vec!["rng".to_owned(), "rand".to_owned()],
        Box::new(Value::ExternalFunction(|_| {
            Ok(Value::Number(rand::thread_rng().gen::<f64>()))
        })),
    );
    lib.insert(
        vec!["rng".to_owned(), "randint".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() != 2 {
                return Err(format!("rng.randint expects 2 arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::Number(lower), Value::Number(upper)) => Ok(Value::Number(
                    rand::thread_rng().gen_range(*lower as i32..*upper as i32) as f64,
                )),
                _ => Err(format!("rng.randint expects both arguments to be numbers")),
            }
        })),
    );

    // List functions
    lib.insert(
        vec!["list".to_owned(), "with_size".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() != 2 {
                return Err(format!("list.with_size can only have 2 arguments"));
            }
            let size = args[0].num() as usize;
            let mut values = Vec::with_capacity(size);
            for _ in 0..size {
                values.push(args[1].clone());
            }

            Ok(Value::List(Arc::new(Mutex::new(values))))
        })),
    );
    lib.insert(
        vec!["list".to_owned(), "size".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() != 1 {
                return Err(format!("list.size can only have 2 arguments"));
            }
            return match &args[0] {
                Value::List(items) => Ok(Value::Number(items.lock().unwrap().len() as f64)),
                _ => Err("called list.size on non list".to_owned()),
            };
        })),
    );
    lib.insert(
        vec!["list".to_owned(), "set".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() != 3 {
                return Err(format!("list.set can only have 3 arguments"));
            }
            return match &args[0] {
                Value::List(items) => {
                    items.lock().unwrap()[args[1].num() as usize] = args[2].clone();
                    Ok(Value::Boolean(true))
                }
                _ => Err("called list.set on non list".to_owned()),
            };
        })),
    );

    lib.insert(
        vec!["list".to_owned(), "clone".to_owned()],
        Box::new(Value::ExternalFunction(|args| {
            if args.len() != 1 {
                return Err(format!("list.clone can only have 1 argument"));
            }
            return match &args[0] {
                Value::List(items) => {
                    let list = items.lock().unwrap().clone();
                    Ok(Value::List(Arc::new(Mutex::new(list))))
                }
                _ => Err("called list.clone on non list".to_owned()),
            };
        })),
    );

    crate::shared::Value::ExternalFunctionLibrary(lib)
}
