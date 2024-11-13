use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::format,
};

use crate::{
    blisp::{functions::eval_function, macros::leaf_node_pattern},
    error::{InterpreTestResult, InterpretError, InterpreteResult},
};

use super::{
    lexer::{LiteralSuffix, NumLiteral, ReservedIdent, Type},
    macros::{list_value_helper, rule_node_pattern},
    parser::{Node, ParseToken, ParseTree, Rule, RuleNodeData},
};

/// Contains variable dictionary
pub struct State {
    vars: HashMap<String, Option<Value>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    /// Get the value of the variable with specified identifier. Returns an Err if the
    pub fn get_var(&self, ident: &str) -> InterpreteResult<&Value> {
        self.vars
            .get(ident)
            .map(Option::as_ref)
            .ok_or(format!("Variable has not been initialized at all: {}", ident).into())
            .and_then(|o| o.ok_or("Variable has been initialized but not set".into()))
    }

    pub fn create_var(&mut self, ident: String, val: Option<Value>) -> InterpreteResult<()> {
        match self.vars.entry(ident) {
            Entry::Vacant(e) => {
                e.insert(val);
                Ok(())
            }
            Entry::Occupied(e) => {
                Err(format!("Already have a variable called: {}", e.key()).into())
            }
        }
    }

    pub fn set_var(&mut self, ident: String, val: Value) -> InterpreteResult<()> {
        match self.vars.entry(ident) {
            Entry::Occupied(mut e) => {
                e.insert(Some(val));
                Ok(())
            }
            Entry::Vacant(e) => {
                Err(format!("No variable exists with identifier {}", e.key()).into())
            }
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

/// Holds the runtime type of the value. Number means it can be `uint, int, float` when
/// needed. NegNumber means it can be `int, float` when needed.
#[derive(Debug, PartialEq, Clone)]
pub enum AbstractType {
    ConcreteType(Type),
    Number,
    NegNumber,
    List,
}

impl AbstractType {
    pub fn coerce_types(
        first: AbstractType,
        second: AbstractType,
    ) -> InterpreteResult<AbstractType> {
        match &first {
            ty @ AbstractType::List => {
                if matches!(second, AbstractType::ConcreteType(Type::List(_)))
                    || second == AbstractType::List
                {
                    Ok(second)
                } else {
                    Err(format!("Unable to coerce list into {:?}", ty).into())
                }
            }
            ty @ AbstractType::Number => {
                if second == AbstractType::Number
                    || second == AbstractType::NegNumber
                    || second == AbstractType::ConcreteType(Type::Int)
                    || second == AbstractType::ConcreteType(Type::UInt)
                    || second == AbstractType::ConcreteType(Type::Float)
                {
                    Ok(second)
                } else {
                    Err(format!("Unable to coerce Number into {:?}", ty).into())
                }
            }
            ty @ AbstractType::NegNumber => {
                if second == AbstractType::NegNumber
                    || second == AbstractType::ConcreteType(Type::Int)
                    || second == AbstractType::ConcreteType(Type::Float)
                {
                    Ok(second)
                } else if second == AbstractType::Number {
                    Ok(first)
                } else {
                    Err(format!("Unable to coerce NegNumber into {:?}", ty).into())
                }
            }
            AbstractType::ConcreteType(ct) => match &second {
                AbstractType::ConcreteType(ct2) => {
                    if ct == ct2 {
                        Ok(second)
                    } else {
                        Err(format!("Unable to coerce {:?} into {:?}", ct, ct2).into())
                    }
                }
                ty @ AbstractType::Number => {
                    if ct == &Type::Int || ct == &Type::UInt || ct == &Type::Float {
                        Ok(first)
                    } else {
                        Err(format!("Unable to coerce Number into {:?}", ty).into())
                    }
                }
                ty @ AbstractType::NegNumber => {
                    if ct == &Type::Int || ct == &Type::Float {
                        Ok(first)
                    } else {
                        Err(format!("Unable to coerce NegNumber into {:?}", ty).into())
                    }
                }
                ty @ AbstractType::List => {
                    if matches!(ct, Type::List(_)) {
                        Ok(first)
                    } else {
                        Err(format!("Unable to coerce List into {:?}", ty).into())
                    }
                }
            },
        }
    }
}

impl From<Type> for AbstractType {
    fn from(value: Type) -> Self {
        Self::ConcreteType(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ValueData {
    Int(i64),
    UInt(u64),
    Float(f64),
    List(Vec<Value>),
    Unit,
    Char(u8),
    Bool(bool),
    // Abstract types below
    Number(u64),
    NegNumber(i64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Value {
    ty: AbstractType,
    val: ValueData,
}

impl Value {
    pub fn new(ty: AbstractType, val: ValueData) -> Self {
        Self { ty, val }
    }

    pub fn is_list(&self) -> bool {
        self.ty == AbstractType::List
    }

    /// Only defined for `Number` typed vars
    pub fn try_as_number(&self) -> InterpreteResult<u64> {
        if let ValueData::Number(n) = self.val {
            Ok(n)
        } else {
            Err(format!("Tried to convert non-number value to number: {:?}", self).into())
        }
    }

    /// Only defined for `Number` and `NegNumber` typed vars
    pub fn try_as_negnumber(&self) -> InterpreteResult<i64> {
        if let ValueData::Number(n) = self.val {
            // TODO check bounds
            Ok(n as i64)
        } else if let ValueData::NegNumber(n) = self.val {
            Ok(n)
        } else {
            Err(format!("Tried to convert invalid value to negnumber: {:?}", self).into())
        }
    }

    /// Only defined for `Number`, `NegNumber`, and `int` typed vars
    pub fn try_as_int(&self) -> InterpreteResult<i64> {
        match self.val {
            ValueData::Number(n) => Ok(n as i64),
            ValueData::NegNumber(n) => Ok(n),
            ValueData::Int(n) => Ok(n),
            _ => Err(format!("Tried to convert invalid value to int: {:?}", self).into()),
        }
    }

    /// Only defined for `Number` and `uint` typed vars
    pub fn try_as_uint(&self) -> InterpreteResult<u64> {
        match self.val {
            ValueData::Number(n) => Ok(n),
            ValueData::UInt(n) => Ok(n),
            _ => Err(format!("Tried to convert invalid value to uint: {:?}", self).into()),
        }
    }

    /// Only defined for `Number`, `NegNumber`, and `float` type vars
    pub fn try_as_float(&self) -> InterpreteResult<f64> {
        match self.val {
            ValueData::Number(n) => Ok(n as f64),
            ValueData::NegNumber(n) => Ok(n as f64),
            ValueData::Float(f) => Ok(f),
            _ => Err(format!("Tried to convert invalid value to float: {:?}", self).into()),
        }
    }

    /// Only defined for `List` types (Abstract list type should never be around at this
    /// point)
    pub fn try_as_list(&self) -> InterpreteResult<Vec<Value>> {
        match &self.val {
            ValueData::List(vals) => Ok(vals.clone()),
            _ => Err(format!("Tried to convert invalid value to list: {:?}", self).into()),
        }
    }

    /// Only defined for `Unit` type
    pub fn try_as_unit(&self) -> InterpreteResult<()> {
        match &self.val {
            ValueData::Unit => Ok(()),
            _ => Err(format!("Tried to convert invalid value to unit: {:?}", self).into()),
        }
    }

    /// Only defined for `Char` type
    pub fn try_as_char(&self) -> InterpreteResult<u8> {
        match self.val {
            ValueData::Char(c) => Ok(c),
            _ => Err(format!("Tried to convert invalid value to char: {:?}", self).into()),
        }
    }

    /// Only defined for `Bool` type
    pub fn try_as_bool(&self) -> InterpreteResult<bool> {
        match self.val {
            ValueData::Bool(b) => Ok(b),
            _ => Err(format!("Tried to convert invalid value to bool: {:?}", self).into()),
        }
    }
}

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Self {
            ty: Type::Char.into(),
            val: ValueData::Char(value),
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self {
            ty: Type::Float.into(),
            val: ValueData::Float(value),
        }
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value {
            ty: Type::List(Box::new(Type::Char)).into(),
            val: ValueData::List(value.as_bytes().iter().copied().map(Value::from).collect()),
        }
    }
}

impl From<()> for Value {
    fn from(value: ()) -> Self {
        Value {
            ty: Type::Unit.into(),
            val: ValueData::Unit,
        }
    }
}

impl TryFrom<NumLiteral> for Value {
    type Error = InterpretError;

    fn try_from(value: NumLiteral) -> Result<Self, Self::Error> {
        match value {
            NumLiteral {
                suffix: LiteralSuffix::None,
                negative,
                float,
                int_part,
                ..
            } => {
                if float {
                    Ok(value.to_f64_checked()?.into())
                } else if negative {
                    Ok(Value::new(
                        AbstractType::NegNumber,
                        ValueData::NegNumber(-(int_part as i64)),
                    ))
                } else {
                    Ok(Value::new(
                        AbstractType::Number,
                        ValueData::Number(int_part),
                    ))
                }
            }
            NumLiteral {
                suffix: LiteralSuffix::Char,
                negative,
                float,
                int_part,
                ..
            } => {
                if float || negative || int_part > 255 {
                    Err(
                        format!("Unable to process NumLiteral with Char suffix: {:?}", value)
                            .into(),
                    )
                } else {
                    Ok(Value::new(
                        Type::Char.into(),
                        ValueData::Char(int_part as u8),
                    ))
                }
            }
            NumLiteral {
                suffix: LiteralSuffix::Float,
                ..
            } => Ok(value.to_f64_checked()?.into()),
            NumLiteral {
                suffix: LiteralSuffix::Unsigned,
                negative,
                float,
                int_part,
                ..
            } => {
                if float || negative {
                    Err(format!(
                        "Unable to process NumLiteral with Unsigned suffix: {:?}",
                        value
                    )
                    .into())
                } else {
                    Ok(Value::new(Type::UInt.into(), ValueData::UInt(int_part)))
                }
            }
        }
    }
}

impl TryFrom<ParseToken> for Value {
    type Error = InterpretError;

    fn try_from(value: ParseToken) -> Result<Self, Self::Error> {
        match value {
            ParseToken::NumLiteral(n) => Self::try_from(n),
            ParseToken::CharLiteral(c) => Ok(c.into()),
            ParseToken::UnitLiteral => Ok(Value::new(Type::Unit.into(), ValueData::Unit)),
            ParseToken::StringLiteral(s) => Ok(s.into()),
            t => Err("Expected a literal token".into()), //ParseToken::Ident(_) => todo!(),
                                                         //ParseToken::Type(_) => todo!(),
                                                         //ParseToken::Reserved(_) => todo!(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
/// This holds the type of an argument. When executing a function we first check for the
/// accepted arguments for the function via crate::blisp::functions::get_arg_types
pub enum ArgumentType {
    /// Specifies a value type (may include variables as well).
    Value,
    Type,
    // This indicates an ident is required, as in `(set <ident> 3)`
    Ident,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Argument {
    Value(Value),
    Type(Type),
    Ident(String),
}

impl From<Value> for Argument {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}

impl Argument {
    pub fn get_type(&self) -> ArgumentType {
        match self {
            Self::Value(_) => ArgumentType::Value,
            Self::Type(_) => ArgumentType::Type,
            Self::Ident(_) => ArgumentType::Ident,
        }
    }

    pub fn is_val(&self) -> bool {
        matches!(self, Argument::Value(_))
    }

    pub fn try_get_val(&self) -> InterpreteResult<&Value> {
        if let Self::Value(v) = self {
            Ok(v)
        } else {
            Err(format!("Attempted to get Value from non-Value argument {:?}", self).into())
        }
    }

    /// If this is a Value-type argument get its associated type
    pub fn try_get_val_type(&self) -> InterpreteResult<AbstractType> {
        let ty = self.try_get_val()?.ty.clone();

        match ty {
            AbstractType::ConcreteType(ct) => Ok(ct.into()),
            AbstractType::List => Err(format!(
                "Unexpectedly found abstract list type when parsing argument {:?}",
                self
            )
            .into()),
            t => Ok(t),
        }
    }
}

pub struct Func {
    f: ReservedIdent,
    args: Vec<Argument>,
}

pub fn eval(node: Node) -> InterpreteResult<Value> {
    let mut state = State::new();

    eval_prog_node(node, &mut state)
}

pub fn eval_node(node: Node) -> InterpreteResult<Value> {
    unimplemented!()
}

//fn eval_rule_node(node: Node, state: &mut state) -> InterpreteResult<Value> {
//    if let Node::Rule(data) = node {
//        match data {
//            RuleNodeData { rule: Rule::Prog, children }
//        }
//    }
//}

fn eval_leaf_node(node: Node, state: &State) -> InterpreteResult<Value> {
    if let Node::Leaf(tok) = node {
        match tok {
            ParseToken::NumLiteral(n) => n.try_into(),
            ParseToken::CharLiteral(c) => Ok(c.into()),
            ParseToken::UnitLiteral => Ok(().into()),
            ParseToken::StringLiteral(s) => Ok(s.into()),
            ParseToken::Ident(i) => state.get_var(&i).cloned(),
            t => Err(format!("Expected literal or identifier, found {:?}", t).into()),
        }
    } else {
        Err(format!("Expected leaf node, got {:?}", node).into())
    }
}

fn eval_prog_node(node: Node, state: &mut State) -> InterpreteResult<Value> {
    if let Node::Rule(RuleNodeData {
        rule: Rule::Prog,
        mut children,
    }) = node
    {
        assert!(children.len() == 1);
        eval_expr_node(children.pop().unwrap(), state)
    } else {
        Err(format!("Expected Prog node, found: {:?}", node).into())
    }
}

fn eval_expr_node(node: Node, state: &mut State) -> InterpreteResult<Value> {
    if let Node::Rule(RuleNodeData {
        rule: Rule::Expr,
        mut children,
    }) = node
    {
        assert!(children.len() == 1);
        eval_expr_body_node(children.pop().unwrap(), state)
    } else {
        Err(format!("Expected Expr node, found: {:?}", node).into())
    }
}

fn eval_expr_body_node(node: Node, state: &mut State) -> InterpreteResult<Value> {
    if let Node::Rule(RuleNodeData {
        rule: Rule::ExprBody,
        mut children,
    }) = node
    {
        assert!(children.len() == 1);
        let node = children.pop().unwrap();

        match &node {
            Node::Rule(RuleNodeData {
                rule: Rule::Val, ..
            }) => eval_val_node(node, state),
            Node::Rule(RuleNodeData {
                rule: Rule::FuncCall,
                ..
            }) => eval_func_call_node(node, state),
            _ => Err(format!(
                "Encountered unexpected node when evaluating expression body: {:?}",
                node
            )
            .into()),
        }
    } else {
        Err(format!("Expected ExprBody node, found: {:?}", node).into())
    }
}

fn eval_val_node(node: Node, state: &mut State) -> InterpreteResult<Value> {
    if let rule_node_pattern!(Val;mut children) = node {
        assert!(children.len() == 1);

        match children.pop().unwrap() {
            leaf_node_pattern!(CharLiteral(c)) => Ok(c.into()),
            leaf_node_pattern!(StringLiteral(s)) => Ok(s.into()),
            leaf_node_pattern!(NumLiteral(n)) => n.try_into(),
            leaf_node_pattern!(UnitLiteral) => Ok(().into()),
            leaf_node_pattern!(Ident(i)) => state.get_var(&i).cloned(),
            rule_node_pattern!(List => node) => eval_list_node(node, state),
            rule_node_pattern!(Expr => node) => eval_expr_node(node, state),
            n => Err(format!("Encountered invalid node when evaluating Val: {:?}", n).into()),
        }
    } else {
        Err(format!("Expected Val node, found: {:?}", node).into())
    }
}

fn eval_func_call_node(node: Node, state: &mut State) -> InterpreteResult<Value> {
    if let rule_node_pattern!(FuncCall; mut children) = node {
        assert!(children.len() == 2);

        let args_node = children.pop().unwrap();

        match children.pop().unwrap() {
            leaf_node_pattern!(Reserved(rsv)) => {
                let func = rsv;
                let args = eval_args_node(args_node, state)?;

                eval_function(func, args)
            }
            n => Err(format!("Expected function name, found {:?}", n).into()),
        }
    } else {
        Err(format!("Expected FuncCall node, found: {:?}", node).into())
    }
}

fn eval_args_node(node: Node, state: &mut State) -> InterpreteResult<Vec<Argument>> {
    if let rule_node_pattern!(Args; mut children) = node {
        if children.len() == 1 {
            // Reached terminal state, nearly done
            match children.pop().unwrap() {
                rule_node_pattern!(Val => node) => {
                    Ok([eval_val_node(node, state)?.into()].to_vec())
                }
                n => Err(format!("Expected Val while parsing ListBody, found: {:?}", n).into()),
            }
        } else {
            assert!(children.len() == 2);

            let mut tail = eval_args_node(children.pop().unwrap(), state)?;
            let val = eval_val_node(children.pop().unwrap(), state)?;

            let mut res = vec![val.into()];
            res.append(&mut tail);

            Ok(res)
        }
    } else {
        Err(format!("Expected ListBody node, found: {:?}", node).into())
    }
}

fn eval_list_node(node: Node, state: &mut State) -> InterpreteResult<Value> {
    if let Node::Rule(RuleNodeData {
        rule: Rule::List,
        mut children,
    }) = node
    {
        assert!(children.len() == 1);
        if let Value {
            val: ValueData::List(vals),
            ..
        } = eval_list_body_node(children.pop().unwrap(), state)?
        {
            let ty = check_list_type(vals.iter().collect())?;

            Ok(Value {
                ty: Type::List(Box::new(ty)).into(),
                val: ValueData::List(vals),
            })
        } else {
            Err("Malformed ListBody result".into())
        }
    } else {
        Err(format!("Expected Args node, found: {:?}", node).into())
    }
}

fn eval_list_body_node(node: Node, state: &mut State) -> InterpreteResult<Value> {
    if let rule_node_pattern!(ListBody; mut children) = node {
        if children.len() == 1 {
            // Reached terminal state, nearly done
            match children.pop().unwrap() {
                rule_node_pattern!(Val => node) => {
                    Ok(list_value_helper![eval_val_node(node, state)?])
                }
                n => Err(format!("Expected Val while parsing ListBody, found: {:?}", n).into()),
            }
        } else {
            assert!(children.len() == 2);

            let tail = eval_list_body_node(children.pop().unwrap(), state)?;
            let val = eval_val_node(children.pop().unwrap(), state)?;

            if let Value {
                val: ValueData::List(mut vec),
                ..
            } = tail
            {
                let mut res = vec![val];
                res.append(&mut vec);

                Ok(Value::new(AbstractType::List, ValueData::List(res)))
            } else {
                Err(format!("Malformed ListBody result: {:?}", tail).into())
            }
        }
    } else {
        Err(format!("Expected ListBody node, found: {:?}", node).into())
    }
}

fn check_list_type(vec: Vec<&Value>) -> InterpreteResult<Type> {
    let init = vec[0];

    let ty = vec
        .iter()
        .map(|v| v.ty.clone())
        .try_fold(init.ty.clone(), AbstractType::coerce_types)?;

    match ty {
        AbstractType::Number | AbstractType::NegNumber => Ok(Type::Int),
        AbstractType::ConcreteType(ct) => Ok(ct),
        AbstractType::List => {
            // Need to recursively find the type of the nested lists
            if let Value {
                val: ValueData::List(vals),
                ..
            } = init
            {
                let init = check_list_type(vals.iter().collect())?;

                // Fold over sublists of current list, trying to match types
                let ty = vec
                    .iter()
                    .map(|&v| {
                        if let Value {
                            val: ValueData::List(_),
                            ..
                        } = v
                        {
                            AbstractType::ConcreteType(
                                check_list_type(vals.iter().collect())
                                    .expect("Something went wrong when parsing sublist types"),
                            )
                        } else {
                            panic!("Something went wrong when parsing sublist types")
                        }
                    })
                    .try_fold(AbstractType::ConcreteType(init), AbstractType::coerce_types)?;

                if let AbstractType::ConcreteType(ct) = ty {
                    Ok(ct)
                } else {
                    Err(format!(
                        "Unable to find a concrete type for the list, found type: {:?}",
                        ty
                    )
                    .into())
                }
            } else {
                Err(format!(
                    "Got {:?} as type of the list but initial value is not a list: {:?}",
                    ty, init
                )
                .into())
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        blisp::{lexer::tokenize, macros::assert_fails, parser::parse_prog},
        error::InterpreTestResult,
    };

    use super::*;

    #[test]
    fn num_literal_conversion_test() -> InterpreTestResult {
        let num1 = NumLiteral::new_int_with_suffix(1, true, 'f');
        let num2 = NumLiteral::new_float(1, 5, false);
        let num3 = NumLiteral::new_int_with_suffix(48, false, 'c');
        let num4 = NumLiteral::new_int(48, false);

        assert_eq!(num1.to_f64_checked().unwrap(), -1f64);
        assert_eq!(num2.to_f64_checked().unwrap(), 1.5f64);
        assert_eq!(Value::try_from(num3)?, Value::from(b'0'));
        assert_eq!(
            Value::try_from(num4)?,
            Value::new(AbstractType::Number, ValueData::Number(48))
        );

        Ok(())
    }

    assert_fails!(
        num_literal_invalid_test1 =>
        Value::try_from(NumLiteral::new_int_with_suffix(1, true, 'c')).unwrap()
    );

    assert_fails!(
        num_literal_invalid_test2 =>
        Value::try_from(NumLiteral::new_int_with_suffix(1, true, 'u')).unwrap()
    );

    macro_rules! do_eval_test {
        ($([$input:expr, $value:expr]),+ $(,)?) => {
            $(
                {
                    let input = $input.chars().collect();
                    let tokens = tokenize(input)?;
                    let prog = parse_prog(tokens.as_slice())?;
                    let value = eval(prog.0)?;

                    assert_eq!(value, $value);
                }
            )+
        };
    }

    #[test]
    fn basic_val_test() -> InterpreTestResult {
        let input1 = "(-1.2)".chars().collect();
        let tokens1 = tokenize(input1)?;
        let prog1 = parse_prog(tokens1.as_slice())?;
        let value1 = eval(prog1.0)?;
        let exp1 = Value {
            ty: Type::Float.into(),
            val: ValueData::Float(-1.2),
        };

        assert_eq!(value1, exp1);

        do_eval_test!(
            ["(48c)", Value::from(b'0')],
            [
                "(123)",
                Value::new(AbstractType::Number, ValueData::Number(123))
            ],
            [
                "(-123)",
                Value::new(AbstractType::NegNumber, ValueData::NegNumber(-123))
            ],
            [
                "([1 2])",
                Value::new(
                    AbstractType::ConcreteType(Type::List(Box::new(Type::Int))),
                    ValueData::List(vec![
                        Value::new(AbstractType::Number, ValueData::Number(1)),
                        Value::new(AbstractType::Number, ValueData::Number(2)),
                    ])
                )
            ],
            [
                "([-1 2])",
                Value::new(
                    AbstractType::ConcreteType(Type::List(Box::new(Type::Int))),
                    ValueData::List(vec![
                        Value::new(AbstractType::NegNumber, ValueData::NegNumber(-1)),
                        Value::new(AbstractType::Number, ValueData::Number(2)),
                    ])
                )
            ],
            [
                "([1 2u])",
                Value::new(
                    AbstractType::ConcreteType(Type::List(Box::new(Type::UInt))),
                    ValueData::List(vec![
                        Value::new(AbstractType::Number, ValueData::Number(1)),
                        Value::new(Type::UInt.into(), ValueData::UInt(2)),
                    ])
                )
            ],
            [
                "([['a' 'b' 'c'] \"bcd\"])",
                Value::new(
                    AbstractType::ConcreteType(Type::List(Box::new(Type::List(Box::new(
                        Type::Char
                    ))))),
                    ValueData::List(vec![
                        Value::new(
                            Type::List(Box::new(Type::Char)).into(),
                            ValueData::List(vec![b'a'.into(), b'b'.into(), b'c'.into()])
                        ),
                        Value::new(
                            Type::List(Box::new(Type::Char)).into(),
                            ValueData::List(vec![b'b'.into(), b'c'.into(), b'd'.into()])
                        ),
                    ])
                )
            ]
        );

        Ok(())
    }
}
