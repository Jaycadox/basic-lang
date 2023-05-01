use chumsky::prelude::*;
use crate::shared::*;

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
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