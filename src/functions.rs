use crate::{
    error::InterpreteResult,
    interpreter::{AbstractType, ValueData},
    lexer::Type,
};

use super::{
    interpreter::{Argument, ArgumentType, State, Value},
    lexer::ReservedIdent,
};

//TODO: At this point every value's type should have been solidified. I should create a
// new type ConcreteValue or something that enforces this

pub fn eval_function(
    func: ReservedIdent,
    args: Vec<Argument>,
    state: &mut State,
) -> InterpreteResult<Value> {
    assert_eq!(
        args.iter().map(Argument::get_type).collect::<Vec<_>>(),
        get_arg_types(func)
    );

    match func {
        ReservedIdent::Add => eval_add(args),
        ReservedIdent::Sub => eval_sub(args),
        ReservedIdent::Mul => eval_mul(args),
        ReservedIdent::Div => eval_div(args),
        ReservedIdent::Or => eval_or(args),
        ReservedIdent::And => eval_and(args),
        ReservedIdent::Not => eval_not(args),
        ReservedIdent::Eq => eval_eq(args),
        ReservedIdent::Ne => eval_neq(args),
        ReservedIdent::Lt => eval_lt(args),
        ReservedIdent::Gt => eval_gt(args),
        ReservedIdent::Le => eval_leq(args),
        ReservedIdent::Ge => eval_geq(args),
        ReservedIdent::ToString => eval_tostring(args),
        ReservedIdent::Concat => eval_concat(args),
        ReservedIdent::Prepend => eval_prepend(args),
        ReservedIdent::Eval => eval_eval(args),
        ReservedIdent::Take => eval_take(args),
        ReservedIdent::Def => eval_def(args, state),
        ReservedIdent::Set => eval_set(args, state),
        ReservedIdent::Init => eval_init(args, state),
        ReservedIdent::Return => eval_return(args),
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
        | ReservedIdent::Ne
        | ReservedIdent::Le
        | ReservedIdent::Ge
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
        | ReservedIdent::ToString
        | ReservedIdent::Not
        | ReservedIdent::Return => vec![ArgumentType::Value],

        ReservedIdent::Set | ReservedIdent::Def => vec![ArgumentType::Ident, ArgumentType::Value],

        ReservedIdent::Init => vec![ArgumentType::Ident, ArgumentType::Type],

        ReservedIdent::If => vec![ArgumentType::Value; 3],
    }
}

//TODO clean this up
macro_rules! num_result_value_helper {
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

macro_rules! bool_result_value_helper {
    ($func:ident, $val1:ident, $val2:ident, $op:ident) => {{
        Value::new(
            AbstractType::ConcreteType(Type::Bool),
            ValueData::Bool(*(&$val1.$func()?.$op(&$val2.$func()?))),
        )
    }};
}

macro_rules! eval_math_func {
    ($func:ident, $args:ident) => {{
        #[allow(unused_imports)]
        use std::ops::{Add, Mul, Sub, Div};

        assert!($args.len() == 2);

        // Since Vec::pop() removes from back
        let (arg2, arg1) = ($args.pop().unwrap(), $args.pop().unwrap());

        let ty = AbstractType::coerce_types(
            arg1.try_get_val_type()?.clone(),
            arg2.try_get_val_type()?.clone(),
        )?;

        let (val1, val2) = (arg1.try_get_val()?, arg2.try_get_val()?);

        match ty {
            AbstractType::Number => Ok(num_result_value_helper!(
                Number,
                try_as_number,
                val1,
                val2,
                i64,
                $func
            )),
            AbstractType::NegNumber => Ok(num_result_value_helper!(
                NegNumber,
                try_as_negnumber,
                val1,
                val2,
                i64,
                $func
            )),
            AbstractType::List => Err(format!(
                "Unexpectedly encountered AbstractType::List in eval step: {:?}",
                ty
            )
            .into()),
            AbstractType::ConcreteType(ct) => match ct {
                Type::Int => Ok(num_result_value_helper!(ct; Int, try_as_int, val1, val2, i64, $func)),
                Type::UInt => Ok(num_result_value_helper!(ct; UInt, try_as_uint, val1, val2, u64, $func)),
                Type::Float => Ok(num_result_value_helper!(ct; Float, try_as_float, val1, val2, f64, $func)),
                Type::Unit => Ok(Value::new(Type::Unit.into(), ValueData::Unit)),
                Type::List(_) => unimplemented!(),
                _ => Err(format!("Unable to {} values of type {:?}", stringify!($func), ct).into()),
            },
            AbstractType::Return => Err(format!("Unable to {} values of type Return", stringify!($func)).into())
        }
    }};
}

pub fn eval_add(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_math_func!(add, args)
}

pub fn eval_sub(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_math_func!(sub, args)
}

pub fn eval_mul(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_math_func!(mul, args)
}

pub fn eval_div(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_math_func!(div, args)
}

macro_rules! eval_bool_bin_func {
    ($func:ident, $args:ident) => {{
        #[allow(unused_imports)]
        use std::ops::{BitAnd, BitOr};

        assert!($args.len() == 2);

        let (arg1, arg2) = ($args.pop().unwrap(), $args.pop().unwrap());

        if arg1.try_get_val_type()? != Type::Bool.into()
            || arg2.try_get_val_type()? != Type::Bool.into()
        {
            Err(format!("{} is only defined on bool values", stringify!($func)).into())
        } else {
            Ok(arg1
                .try_get_val()?
                .try_as_bool()?
                .$func(arg2.try_get_val()?.try_as_bool()?)
                .into())
        }
    }};
}

pub fn eval_and(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_bool_bin_func!(bitand, args)
}

pub fn eval_or(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_bool_bin_func!(bitor, args)
}

pub fn eval_not(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    assert!(args.len() == 1);

    let arg = args.pop().unwrap();

    if arg.try_get_val_type()? != Type::Bool.into() {
        Err("Not is only defined on bool values".into())
    } else {
        Ok((!arg.try_get_val()?.try_as_bool()?).into())
    }
}

macro_rules! eval_cmp_function {
    ($func:ident, $args:ident) => {{
        #[allow(unused_imports)]
        use std::cmp::PartialOrd;

        assert!($args.len() == 2);

        // Since Vec::pop() removes from back
        let (arg2, arg1) = ($args.pop().unwrap(), $args.pop().unwrap());

        let ty = AbstractType::coerce_types(
            arg1.try_get_val_type()?.clone(),
            arg2.try_get_val_type()?.clone(),
        )?;

        let (val1, val2) = (arg1.try_get_val()?, arg2.try_get_val()?);

        match ty {
            AbstractType::Number => Ok(bool_result_value_helper!(try_as_number, val1, val2, $func)),
            AbstractType::NegNumber => Ok(bool_result_value_helper!(
                try_as_negnumber,
                val1,
                val2,
                $func
            )),
            AbstractType::List => Err(format!(
                "Unexpectedly encountered AbstractType::List in eval step: {:?}",
                ty
            )
            .into()),
            AbstractType::ConcreteType(ct) => match ct {
                Type::Int => Ok(bool_result_value_helper!(try_as_int, val1, val2, $func)),
                Type::UInt => Ok(bool_result_value_helper!(try_as_uint, val1, val2, $func)),
                Type::Float => Ok(bool_result_value_helper!(try_as_float, val1, val2, $func)),
                Type::Bool => Ok(bool_result_value_helper!(try_as_bool, val1, val2, $func)),
                Type::Char => Ok(bool_result_value_helper!(try_as_char, val1, val2, $func)),
                Type::List(_) => unimplemented!(),
                _ => Err(format!("Unable to {} values of type {:?}", stringify!($func), ct).into()),
            },
            AbstractType::Return => {
                Err(format!("Unable to {} values of type Return", stringify!($func)).into())
            }
        }
    }};
}

pub fn eval_eq(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_cmp_function!(eq, args)
}

pub fn eval_neq(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_cmp_function!(ne, args)
}

pub fn eval_lt(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_cmp_function!(lt, args)
}

pub fn eval_gt(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_cmp_function!(gt, args)
}

pub fn eval_leq(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_cmp_function!(le, args)
}

pub fn eval_geq(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    eval_cmp_function!(ge, args)
}

pub fn eval_concat(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    assert!(args.len() == 2);

    let (arg2, arg1) = (args.pop().unwrap(), args.pop().unwrap());

    let (val1, val2) = (arg1.try_get_val()?, arg2.try_get_val()?);

    match (val1.ty(), val2.ty()) {
        (
            AbstractType::ConcreteType(Type::List(lt1)),
            AbstractType::ConcreteType(Type::List(lt2)),
        ) => {
            if lt1 != lt2 {
                return Err("Unable to concatenate lists of different types".into());
            }

            Ok(Value::new(
                AbstractType::ConcreteType(Type::List(lt1.clone())),
                ValueData::List(
                    val1.try_as_list()?
                        .iter()
                        .chain(val2.try_as_list()?.iter())
                        .cloned()
                        .collect(),
                ),
            ))
        }
        _ => Err("Unable to concatenate non-list types".into()),
    }
}
pub fn eval_prepend(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    assert!(args.len() == 2);

    let (arg2, arg1) = (args.pop().unwrap(), args.pop().unwrap());

    let (val1, val2) = (arg1.try_get_val()?, arg2.try_get_val()?);

    match val2.ty() {
        AbstractType::ConcreteType(Type::List(ct2)) => match val1.ty() {
            AbstractType::ConcreteType(ct1) => {
                if &*(ct2.clone()) != ct1 {
                    Err("Unable to prepend list of different type".into())
                } else {
                    Ok(Value::new(
                        AbstractType::ConcreteType(Type::List(ct2.clone())),
                        ValueData::List(
                            [val1.clone()]
                                .iter()
                                .chain(val2.try_as_list()?)
                                .cloned()
                                .collect(),
                        ),
                    ))
                }
            }
            AbstractType::Number => {
                let ct2 = *ct2.clone();
                if ct2 == Type::Int || ct2 == Type::UInt || ct2 == Type::Float {
                    Ok(Value::new(
                        AbstractType::ConcreteType(Type::List(Box::new(ct2))),
                        ValueData::List(
                            [val1.clone()]
                                .iter()
                                .chain(val2.try_as_list()?.iter())
                                .cloned()
                                .collect(),
                        ),
                    ))
                } else {
                    Err("Unable to prepend number to non-number list".into())
                }
            }
            AbstractType::NegNumber => {
                let ct2 = *ct2.clone();
                if ct2 == Type::Int || ct2 == Type::Float {
                    Ok(Value::new(
                        AbstractType::ConcreteType(Type::List(Box::new(ct2))),
                        ValueData::List(
                            [val1.clone()]
                                .iter()
                                .chain(val2.try_as_list()?.iter())
                                .cloned()
                                .collect(),
                        ),
                    ))
                } else {
                    Err("Unable to prepend number to non-number list".into())
                }
            }

            _ => Err("Unable to concatenate lists of different types".into()),
        },

        _ => Err("Unable to concatenate non-list types".into()),
    }
}

pub fn eval_tostring(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    assert!(args.len() == 1);

    match args[0].try_get_val_type()? {
        AbstractType::ConcreteType(_) | AbstractType::Number | AbstractType::NegNumber => {
            Ok(args.pop().unwrap().try_get_val()?.to_string().into())
        }
        _ => Err("Unable to convert non-concrete list type to string".into()),
    }
}

pub fn eval_take(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    assert!(args.len() == 2);

    let (arg2, arg1) = (args.pop().unwrap(), args.pop().unwrap());

    let (val1, val2) = (arg1.try_get_val()?, arg2.try_get_val()?);

    match val2.ty() {
        AbstractType::ConcreteType(Type::List(ct2)) => {
            let n = match val1.val() {
                ValueData::Int(i) | ValueData::Number(i) | ValueData::NegNumber(i) => *i as usize,
                ValueData::UInt(i) => *i as usize,
                _ => return Err("Take only accepts int, uint, Number, NegNumber".into()),
            };

            Ok(Value::new(
                Type::List(ct2.clone()).into(),
                ValueData::List(val2.try_as_list()?.iter().take(n).cloned().collect()),
            ))
        }
        _ => Err("Unable to take elements from non-list type".into()),
    }
}

pub fn eval_eval(args: Vec<Argument>) -> InterpreteResult<Value> {
    assert!(args.len() == 1);

    Ok(().into())
}

pub fn eval_def(mut args: Vec<Argument>, state: &mut State) -> InterpreteResult<Value> {
    assert!(args.len() == 2);

    let (arg2, arg1) = (args.pop().unwrap(), args.pop().unwrap());

    let ident = arg1.try_get_ident()?;
    let val = arg2.try_get_val()?;

    state.create_var(ident.to_string(), val.clone())?;

    Ok(().into())
}

pub fn eval_init(mut args: Vec<Argument>, state: &mut State) -> InterpreteResult<Value> {
    assert!(args.len() == 2);

    let (arg2, arg1) = (args.pop().unwrap(), args.pop().unwrap());

    let ident = arg1.try_get_ident()?;
    let ty = arg2.try_get_type()?;

    state.init_var(ident.to_string(), ty.clone())?;

    Ok(().into())
}

pub fn eval_set(mut args: Vec<Argument>, state: &mut State) -> InterpreteResult<Value> {
    assert!(args.len() == 2);

    let (arg2, arg1) = (args.pop().unwrap(), args.pop().unwrap());

    let ident = arg1.try_get_ident()?;
    let val = arg2.try_get_val()?;

    state.set_var(ident.to_string(), val.clone())?;

    Ok(().into())
}

pub fn eval_return(mut args: Vec<Argument>) -> InterpreteResult<Value> {
    assert!(args.len() == 1);

    let mut val = args.pop().unwrap().try_get_val()?.clone();

    val.set_ty(AbstractType::Return);

    Ok(val)
}

#[cfg(test)]
mod tests {

    use crate::{
        error::{InterpreTestResult, InterpreteResult},
        interpreter::{eval, AbstractType, Argument, State, Value, ValueData},
        lexer::{tokenize, Type},
        macros::list_value_helper,
        parser::parse_prog,
    };

    use super::eval_add;

    macro_rules! do_interpret_test {
        ($([$input:expr, $output:expr]),+) => {{
            $(
                let mut tokens = tokenize($input.chars().collect())?;
                let node = parse_prog(&mut tokens)?;
                let val = eval(node)?;

                assert_eq!(val, $output);
            )+
        }};
    }

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

        let mut tokens1 = tokenize(input1.chars().collect())?;
        let tokens2 = tokenize(input2.chars().collect())?;

        assert_eq!(tokens1, tokens2);

        let node = parse_prog(&mut tokens1)?;
        let val = eval(node)?;

        assert_eq!(val, 2.5.into());

        Ok(())
    }

    #[test]
    fn nested_add_test() -> InterpreTestResult {
        let input1 = "(+ 2 (add 1.5 1))";
        let input2 = "(add 2 (+ 1.5 1))";

        let mut tokens1 = tokenize(input1.chars().collect())?;
        let tokens2 = tokenize(input2.chars().collect())?;

        assert_eq!(tokens1, tokens2);

        let node = parse_prog(&mut tokens1)?;
        let val = eval(node)?;

        assert_eq!(val, 4.5.into());

        Ok(())
    }

    #[should_panic(expected = "Unable to coerce UInt into Float")]
    #[test]
    fn invalid_type_test1() {
        let input = "(+ 1u (add 1.5 1))";

        let mut tokens = tokenize(input.chars().collect()).expect("Failed lexing");

        let node = parse_prog(&mut tokens).expect("Failed parsing");
        eval(node).unwrap();
    }

    #[test]
    fn full_math_test() -> InterpreTestResult {
        // 120 / (4 * (1 + (3 - 2.5))) = 20
        let input = "(/ 120 (* 4.0 (+ 1.0 (- 3 2.5))))";
        let expected = 20f64.into();

        do_interpret_test!([input, expected]);

        Ok(())
    }

    #[test]
    fn bool_algebra_test() -> InterpreTestResult {
        do_interpret_test!(
            ["(and true true)", true.into()],
            ["(and true false)", false.into()],
            ["(or true false)", true.into()],
            ["(or false false)", false.into()],
            ["(not true)", false.into()],
            ["(not false)", true.into()],
            [
                "(and (or true false) (not (not (and false true))))",
                false.into()
            ]
        );

        Ok(())
    }

    #[test]
    fn cmp_test() -> InterpreTestResult {
        do_interpret_test!(
            ["(eq 3 3.0)", true.into()],
            ["(eq 3 4)", false.into()],
            ["(ne 3 4)", true.into()],
            ["(lt 3 4)", true.into()],
            ["(gt 3 4)", false.into()],
            ["(le 3 4)", true.into()],
            ["(ge 3 4)", false.into()],
            ["(eq (+ -1.45 2.13) (/ 6.800 10))", true.into()]
        );

        Ok(())
    }

    #[test]
    fn tostring_test() -> InterpreTestResult {
        do_interpret_test!(
            ["(tostring 3)", "3".into()],
            ["(tostring true)", "true".into()],
            ["(tostring (add 1 2))", "3".into()],
            ["(tostring [1 2 3])", "[1, 2, 3]".into()],
            ["(tostring (tostring 3))", "\"3\"".into()]
        );

        Ok(())
    }

    // This is here to demonstrate how lists can only have elements of a single type. It
    // is mainly for the purpose of showing why eval is useful
    #[should_panic(expected = "Unable to coerce Float into UInt")]
    #[test]
    fn mismatched_statement_list_test() {
        let input = "([
            (+ 1.1 2)
            (- 5 2u)
        ])";

        let mut tokens = tokenize(input.chars().collect()).expect("Failed lexing");

        let node = parse_prog(&mut tokens).expect("Failed parsing");

        let _val = eval(node).unwrap();
    }

    #[test]
    fn eval_test() -> InterpreTestResult {
        // By using eval we transform each val to a unit type and avoid the type issue
        // shown above
        do_interpret_test!([
            "([
            (eval (+ 1.1 2))
            (eval (- 5 2u))
        ])",
            Value::new(
                Type::List(Box::new(Type::Unit)).into(),
                ValueData::List(vec![().into(), ().into()])
            )
        ]);

        Ok(())
    }

    #[test]
    fn concat_test() -> InterpreTestResult {
        do_interpret_test!(
            [
                "(concat [1 2 3] [4 5 6])",
                list_value_helper![
                    ct: Int;
                    Value::new_number(1),
                    Value::new_number(2),
                    Value::new_number(3),
                    Value::new_number(4),
                    Value::new_number(5),
                    Value::new_number(6)
                ]
            ],
            [
                "(concat [1 2f 3] [4.0 5 6])",
                list_value_helper![
                    ct: Float;
                    Value::new_number(1),
                    2.0.into(),
                    Value::new_number(3),
                    4.0.into(),
                    Value::new_number(5),
                    Value::new_number(6)
                ]
            ]
        );

        Ok(())
    }

    // This test demonstrates a limitation of my approach to the type system. Since each
    // list's type is solidified on rule evaluation, the interpreter sees this as a
    // concrete list of type Int and a concrete list of type Float. This is why the
    // failure happens
    #[should_panic(expected = "Unable to concatenate lists of different types")]
    #[test]
    fn concat_mismatched_test() {
        let input = "(concat [1 2 3] [4.0 5 6])";

        let mut tokens = tokenize(input.chars().collect()).expect("Failed lexing");

        let node = parse_prog(&mut tokens).expect("Failed parsing");

        let _val = eval(node).unwrap();
    }

    #[test]
    fn prepend_test() -> InterpreTestResult {
        do_interpret_test!(
            [
                "(prepend 1 [2 3 4])",
                list_value_helper![
                    ct: Int;
                    Value::new_number(1),
                    Value::new_number(2),
                    Value::new_number(3),
                    Value::new_number(4)
                ]
            ],
            [
                "(prepend [1.4] [[2.5] [3.6]])",
                list_value_helper![
                    nested: Float;
                    list_value_helper![ct: Float; 1.4.into()],
                    list_value_helper![ct: Float; 2.5.into()],
                    list_value_helper![ct: Float; 3.6.into()]
                ]
            ],
            [
                "(concat (prepend [1.0] [[1.4]]) [[2.5] [3.6]])",
                list_value_helper![
                    nested: Float;
                    list_value_helper![ct: Float; 1.0.into()],
                    list_value_helper![ct: Float; 1.4.into()],
                    list_value_helper![ct: Float; 2.5.into()],
                    list_value_helper![ct: Float; 3.6.into()]
                ]
            ]
        );

        Ok(())
    }

    #[test]
    fn take_test() -> InterpreTestResult {
        do_interpret_test!(
            [
                "(take 1 [2 3 4])",
                list_value_helper![
                    ct: Int;
                    Value::new_number(2)
                ]
            ],
            [
                "(take 2 [[2.5] [3.6] [4.7]])",
                list_value_helper![
                    nested: Float;
                    list_value_helper![ct: Float; 2.5.into()],
                    list_value_helper![ct: Float; 3.6.into()]
                ]
            ],
            [
                "(concat (prepend (take 2 [12.3 4.2 1.3 1.2 2.3]) [[1.4]]) [[2.5] [3.6]])",
                list_value_helper![
                    nested: Float;
                    list_value_helper![ct: Float; 12.3.into(), 4.2.into()],
                    list_value_helper![ct: Float; 1.4.into()],
                    list_value_helper![ct: Float; 2.5.into()],
                    list_value_helper![ct: Float; 3.6.into()]
                ]
            ]
        );

        Ok(())
    }

    #[test]
    fn basic_var_test() -> InterpreTestResult {
        do_interpret_test!(
            [
                "
                ([
                    (def a 10f) 
                    (eval (return a))
                ])
                ",
                Value::new(AbstractType::Return, ValueData::Float(10.0))
            ],
            [
                "
                ([
                    (init a float) 
                    (set a 12)
                    (return a)
                ])
                ",
                Value::new(AbstractType::Return, ValueData::Float(12.0))
            ],
            [
                "
                ([
                    (init a uint) 
                    (set a 1)
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (return a)
                ])
                ",
                Value::new(AbstractType::Return, ValueData::UInt(9))
            ],
            [
                "
                ([
                    (init a uint) 
                    (set a 1)
                    (return a)
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                    (set a (+ a 1))
                ])
                ",
                Value::new(AbstractType::Return, ValueData::UInt(1))
            ]
        );

        Ok(())
    }
}
