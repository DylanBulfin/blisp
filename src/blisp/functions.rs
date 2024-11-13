use crate::{
    blisp::{
        interpreter::{AbstractType, ValueData},
        lexer::Type,
    },
    error::InterpreteResult,
};

use super::{
    interpreter::{Argument, ArgumentType, State, Value},
    lexer::ReservedIdent,
};

pub fn eval_function(func: ReservedIdent, args: Vec<Argument>) -> InterpreteResult<Value> {
    assert_eq!(
        args.iter().map(Argument::get_type).collect::<Vec<_>>(),
        get_arg_types(func)
    );

    match func {
        ReservedIdent::Add => eval_add(args),
        _ => unimplemented!(),
    }
}

pub fn get_arg_types(func: ReservedIdent) -> Vec<ArgumentType> {
    match func {
        ReservedIdent::Add
        | ReservedIdent::Sub
        | ReservedIdent::Div
        | ReservedIdent::Mul
        | ReservedIdent::Eq
        | ReservedIdent::Neq
        | ReservedIdent::Leq
        | ReservedIdent::Geq
        | ReservedIdent::Lt
        | ReservedIdent::Gt
        | ReservedIdent::And
        | ReservedIdent::Concat
        | ReservedIdent::While
        | ReservedIdent::Or
        | ReservedIdent::Take
        | ReservedIdent::Prepend => vec![ArgumentType::Value; 2],

        ReservedIdent::Write
        | ReservedIdent::Read
        | ReservedIdent::Eval
        | ReservedIdent::ToString => vec![ArgumentType::Value],

        ReservedIdent::Set | ReservedIdent::Def => vec![ArgumentType::Ident, ArgumentType::Value],

        ReservedIdent::Init => vec![ArgumentType::Ident, ArgumentType::Type],

        ReservedIdent::If => vec![ArgumentType::Value; 3],
    }
}

macro_rules! result_value_helper {
    (ct; $type:ident, $func:ident, $val1:ident, $val2:ident, $restype:ident, $op:ident) => {{
        Value::new(
            AbstractType::ConcreteType(Type::$type),
            ValueData::$type($restype::$op($val1.$func()?, $val2.$func()?)),
        )
    }};
    ($type:ident, $func:ident, $val1:ident, $val2:ident, $restype:ident, $op:ident) => {{
        Value::new(
            AbstractType::$type,
            ValueData::$type($restype::$op($val1.$func()?, $val2.$func()?)),
        )
    }};
}

pub fn eval_add(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    assert!(args.len() == 2);

    let (arg1, arg2) = (args.pop().unwrap(), args.pop().unwrap());

    let ty = AbstractType::coerce_types(arg1.try_get_val_type()?, arg2.try_get_val_type()?)?;

    let (val1, val2) = (arg1.try_get_val()?, arg2.try_get_val()?);

    use std::ops::Add;

    match ty {
        AbstractType::Number => Ok(result_value_helper!(
            Number,
            try_as_number,
            val1,
            val2,
            u64,
            add
        )),
        AbstractType::NegNumber => Ok(result_value_helper!(
            NegNumber,
            try_as_negnumber,
            val1,
            val2,
            i64,
            add
        )),
        AbstractType::List => Err(format!(
            "Unexpectedly encountered AbstractType::List in eval step: {:?}",
            ty
        )
        .into()),
        AbstractType::ConcreteType(ct) => match ct {
            Type::Int => Ok(result_value_helper!(ct; Int, try_as_int, val1, val2, i64, add)),
            Type::UInt => Ok(result_value_helper!(ct; UInt, try_as_uint, val1, val2, u64, add)),
            Type::Float => Ok(result_value_helper!(ct; Float, try_as_float, val1, val2, f64, add)),
            Type::Unit => Ok(Value::new(Type::Unit.into(), ValueData::Unit)),
            Type::List(_) => unimplemented!(),
            _ => Err(format!("Unable to add values of type {:?}", ct).into()),
        },
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        blisp::{
            interpreter::{eval, Argument, State, Value},
            lexer::tokenize,
            parser::parse_prog,
        },
        error::InterpreTestResult,
    };

    use super::eval_add;

    #[test]
    fn eval_add_test() -> InterpreTestResult {
        let args = vec![Argument::Value(1.2.into()), Argument::Value(2.5.into())];

        let res = eval_add(args)?;

        assert_eq!(res, Value::from(3.7));

        Ok(())
    }

    #[test]
    fn basic_e2e() -> InterpreTestResult {
        let input1 = "(+ 1.5 1)";
        let input2 = "(add 1.5 1)";

        let tokens1 = tokenize(input1.chars().collect())?;
        let tokens2 = tokenize(input2.chars().collect())?;

        assert_eq!(tokens1, tokens2);

        let node = parse_prog(tokens1.as_slice())?;
        let val = eval(node.0)?;

        assert_eq!(val, 2.5.into());

        Ok(())
    }

    #[test]
    fn nested_add_test() -> InterpreTestResult {
        let input1 = "(+ 2 (add 1.5 1))";
        let input2 = "(add 2 (+ 1.5 1))";

        let tokens1 = tokenize(input1.chars().collect())?;
        let tokens2 = tokenize(input2.chars().collect())?;

        assert_eq!(tokens1, tokens2);

        let node = parse_prog(tokens1.as_slice())?;
        let val = eval(node.0)?;

        assert_eq!(val, 4.5.into());

        Ok(())
    }

    #[should_panic(expected = "Unable to coerce Float into UInt")]
    #[test]
    fn invalid_type_test1() {
        let input = "(+ 1u (add 1.5 1))";

        let tokens = tokenize(input.chars().collect()).expect("Failed lexing");

        let node = parse_prog(tokens.as_slice()).expect("Failed parsing");
        eval(node.0).unwrap();
    }
}
