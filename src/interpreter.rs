use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{format, Display, Formatter},
    io::{stdin, stdout, BufRead, Read, Stdin, Stdout, Write},
};

use crate::{
    error::{InterpreTestResult, InterpretError, InterpreteResult},
    functions::eval_function,
    macros::{leaf_node_pattern, rule_node_helper},
};

use super::{
    lexer::{LiteralSuffix, NumLiteral, ReservedIdent, Type},
    macros::{list_value_helper, rule_node_pattern},
    parser::{Node, ParseToken, ParseTree, Rule, RuleNodeData},
};

/// Contains variable dictionary
pub struct State<R, W>
where
    R: Read,
    W: Write,
{
    vars: HashMap<String, (Type, Option<Value>)>,
    reader: R,
    writer: W,
}

impl State<Stdin, Stdout> {
    pub fn new_with_defaults() -> Self {
        Self {
            vars: HashMap::new(),
            reader: stdin(),
            writer: stdout(),
        }
    }
}

impl<R, W> State<R, W>
where
    R: Read,
    W: Write,
{
    pub fn new(reader: R, writer: W) -> Self {
        Self {
            vars: HashMap::new(),
            reader,
            writer,
        }
    }

    /// Get the value of the variable with specified identifier. Returns an Err if the
    pub fn get_var(&self, ident: &str) -> InterpreteResult<&Value> {
        self.vars
            .get(ident)
            .map(|(a, b)| (a, b.as_ref()))
            .ok_or(format!("Variable has not been initialized at all: {}", ident).into())
            .and_then(|o| {
                o.1.ok_or("Variable has been initialized but not set".into())
            })
    }

    pub fn create_var(&mut self, ident: String, val: Value) -> InterpreteResult<()> {
        match self.vars.entry(ident) {
            Entry::Vacant(e) => {
                let ty = match &val.ty {
                    AbstractType::ConcreteType(t) => t.clone(),
                    AbstractType::NegNumber | AbstractType::Number => Type::Int,
                    AbstractType::List => {
                        return Err("Cannot create a variable with abstract list type".into())
                    }
                    ty => return Err(format!("Cannot create a variable with {:?} type", ty).into()),
                };
                e.insert((ty, Some(val)));
                Ok(())
            }
            Entry::Occupied(e) => {
                Err(format!("Already have a variable called: {}", e.key()).into())
            }
        }
    }

    pub fn init_var(&mut self, ident: String, ty: Type) -> InterpreteResult<()> {
        match self.vars.entry(ident) {
            Entry::Vacant(e) => {
                e.insert((ty, None));
                Ok(())
            }
            Entry::Occupied(e) => {
                Err(format!("Already have a variable called: {}", e.key()).into())
            }
        }
    }

    pub fn set_var(&mut self, ident: String, mut val: Value) -> InterpreteResult<()> {
        match self.vars.entry(ident) {
            Entry::Occupied(mut e) => {
                val.ty = AbstractType::coerce_types(val.ty, e.get().0.clone().into())?;
                val.update_data()?;
                e.get_mut().1 = Some(val);
                Ok(())
            }
            Entry::Vacant(e) => {
                Err(format!("No variable exists with identifier {}", e.key()).into())
            }
        }
    }

    pub fn write(&mut self, data: Value) -> InterpreteResult<()> {
        Ok(self.writer.write_fmt(format_args!("{}\n", data))?)
    }

    pub fn read(&mut self, cnt: usize) -> InterpreteResult<String> {
        let mut buf = vec![0u8; cnt];

        // Using read_exact for now, probably need to change later
        self.reader.read_exact(&mut buf)?;

        Ok(buf.into_iter().map(|c| c as char).collect())
    }
}

impl Default for State<Stdin, Stdout> {
    fn default() -> Self {
        Self::new_with_defaults()
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
    // May change how I implement returning at some point
    Return,
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
                ty => Err(format!("Attempt to coerce into type {:?}", ty).into()),
            },
            ty => Err(format!("Attempt to coerce type {:?}", ty).into()),
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
    Number(i64),
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

    pub fn new_number(val: i64) -> Self {
        Self {
            ty: AbstractType::Number,
            val: ValueData::Number(val),
        }
    }

    pub fn new_negnumber(val: i64) -> Self {
        Self {
            ty: AbstractType::NegNumber,
            val: ValueData::NegNumber(val),
        }
    }

    pub fn update_data(&mut self) -> InterpreteResult<()> {
        if let AbstractType::ConcreteType(ct) = &self.ty {
            match ct {
                Type::Int => self.val = ValueData::Int(self.try_as_int()?),
                Type::UInt => self.val = ValueData::UInt(self.try_as_uint()?),
                Type::Float => self.val = ValueData::Float(self.try_as_float()?),
                Type::Unit => self.val = ValueData::Unit,
                Type::Char => self.val = ValueData::Char(self.try_as_char()?),
                Type::Bool => self.val = ValueData::Bool(self.try_as_bool()?),
                Type::List(_) => (), // Don't care about types of list values
            }

            Ok(())
        } else {
            Err(format!("Can't update data type on non-concrete type {:?}", self).into())
        }
    }

    pub fn val(&self) -> &ValueData {
        &self.val
    }

    pub fn ty(&self) -> &AbstractType {
        &self.ty
    }

    pub fn is_list(&self) -> bool {
        self.ty == AbstractType::List || matches!(self.ty, AbstractType::ConcreteType(_))
    }

    pub fn is_concrete_list(&self) -> bool {
        matches!(self.ty, AbstractType::ConcreteType(Type::List(_)))
    }

    /// Only defined for `Number` typed vars
    pub fn try_as_number(&self) -> InterpreteResult<i64> {
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
            ValueData::Number(n) => Ok(n as u64),
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
    pub fn try_as_list(&self) -> InterpreteResult<&Vec<Value>> {
        match &self.val {
            ValueData::List(vals) => Ok(vals),
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

    pub fn set_ty(&mut self, ty: AbstractType) {
        self.ty = ty;
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.val {
            ValueData::UInt(n) => f.write_fmt(format_args!("{}", n)),
            ValueData::Number(i) | ValueData::NegNumber(i) | ValueData::Int(i) => {
                f.write_fmt(format_args!("{}", i))
            }
            ValueData::Float(x) => f.write_fmt(format_args!("{}", x)),
            ValueData::Bool(b) => f.write_fmt(format_args!("{}", b)),
            ValueData::Char(c) => f.write_fmt(format_args!("'{}'", *c as char)),
            ValueData::Unit => f.write_str("()"),
            ValueData::List(v) => {
                if self.ty == Type::List(Box::new(Type::Char)).into() {
                    let s = v
                        .iter()
                        .map(|c| {
                            c.try_as_char()
                                .expect("Unable to read string contents as char")
                                as char
                        })
                        .collect::<String>();
                    f.write_fmt(format_args!("\"{}\"", s))
                } else {
                    let s = v
                        .iter()
                        .map(|val| val.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    f.write_fmt(format_args!("[{}]", s))
                }
            }
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

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self {
            ty: Type::Int.into(),
            val: ValueData::Int(value),
        }
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Self {
            ty: Type::UInt.into(),
            val: ValueData::UInt(value),
        }
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::from(value.as_str())
    }
}
impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value {
            ty: Type::List(Box::new(Type::Char)).into(),
            val: ValueData::List(value.as_bytes().iter().copied().map(Value::from).collect()),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value {
            ty: Type::Bool.into(),
            val: ValueData::Bool(value),
        }
    }
}

impl From<()> for Value {
    fn from(_value: ()) -> Self {
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
                        ValueData::Number(int_part as i64),
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

    pub fn try_get_type(&self) -> InterpreteResult<&Type> {
        if let Self::Type(t) = self {
            Ok(t)
        } else {
            Err(format!("Attempted to get Type from non-Type argument {:?}", self).into())
        }
    }

    pub fn try_get_ident(&self) -> InterpreteResult<&str> {
        if let Self::Ident(i) = self {
            Ok(i)
        } else {
            Err(format!("Attempted to get Ident from non-Ident argument {:?}", self).into())
        }
    }
}

pub struct Func {
    f: ReservedIdent,
    args: Vec<Argument>,
}

pub fn eval_custom<R, W>(node: Node, reader: R, writer: W) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
    let mut state = State::new(reader, writer);

    eval_prog_node(node, &mut state)
}

pub fn eval(node: Node) -> InterpreteResult<Value> {
    let mut state = State::new_with_defaults();

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

fn eval_prog_node<R, W>(node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
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

fn eval_expr_node<R, W>(node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
    if let Node::Rule(RuleNodeData {
        rule: Rule::Expr,
        mut children,
    }) = node
    {
        assert!(children.len() == 1);
        let val = eval_expr_body_node(children.pop().unwrap(), state)?;

        Ok(val)
    } else {
        Err(format!("Expected Expr node, found: {:?}", node).into())
    }
}

fn eval_expr_body_node<R, W>(node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
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

fn eval_val_node<R, W>(node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
    if let rule_node_pattern!(Val; mut children) = node {
        assert!(children.len() == 1);

        match children.pop().unwrap() {
            leaf_node_pattern!(CharLiteral(c)) => Ok(c.into()),
            leaf_node_pattern!(StringLiteral(s)) => Ok(s.into()),
            leaf_node_pattern!(NumLiteral(n)) => n.try_into(),
            leaf_node_pattern!(BoolLiteral(n)) => Ok(n.into()),
            leaf_node_pattern!(UnitLiteral) => Ok(().into()),
            leaf_node_pattern!(Ident(s)) => state.get_var(&s).cloned(),
            rule_node_pattern!(List => node) => eval_list_node(node, state),
            rule_node_pattern!(Expr => node) => eval_expr_node(node, state),
            n => Err(format!("Encountered invalid node when evaluating Val: {:?}", n).into()),
        }
    } else {
        Err(format!("Expected Val node, found: {:?}", node).into())
    }
}

fn eval_special_arg(node: Node) -> InterpreteResult<Argument> {
    if let rule_node_pattern!(Val; mut children) = node {
        match children.pop().unwrap() {
            leaf_node_pattern!(Ident(s)) => Ok(Argument::Ident(s)),
            leaf_node_pattern!(Type(s)) => Ok(Argument::Type(s)),
            n => Err(format!(
                "Encountered invalid node when evaluating special argument: {:?}",
                n
            )
            .into()),
        }
    } else {
        Err(format!("Expected Val node, found: {:?}", node).into())
    }
}

// For now a while statement never has a value, may add `break` in the future
fn eval_while_loop<R, W>(args_node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
    if let rule_node_pattern!(Args; mut arg_children) = args_node.clone() {
        assert!(arg_children.len() == 2);
        let body = arg_children.pop().unwrap();
        let cond_node = arg_children.pop().unwrap();
        if let rule_node_pattern!(Val => val_node) = cond_node {
            let val = eval_val_node(val_node.clone(), state)?;

            if val.ty == AbstractType::Return {
                return Ok(val);
            }

            let mut cond = if val.ty == Type::Bool.into() {
                val.try_as_bool()?
            } else {
                return Err(
                    format!("Expected boolean for while condition, found {:?}", val).into(),
                );
            };

            while cond {
                let arg_val = eval_args_node(body.clone(), state, 0)?.pop().unwrap();
                if arg_val.is_val() && arg_val.try_get_val_type()? == AbstractType::Return {
                    return Ok(arg_val.try_get_val()?.clone());
                }

                // Recalculate the condition in case one of its values has
                // changed
                let val = eval_val_node(val_node.clone(), state)?;
                if val.ty == Type::Bool.into() {
                    cond = val.try_as_bool()?;
                } else {
                    return Err(
                        format!("Expected boolean for while condition, found {:?}", val).into(),
                    );
                }
            }

            // Loop evaluates to () unless Return statement found
            Ok(().into())
        } else {
            Err(format!("Expected Val node, found: {:?}", cond_node).into())
        }
    } else {
        Err(format!("Expected Args node, found: {:?}", args_node).into())
    }
}

fn eval_if_statement<R, W>(args_node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
    if let rule_node_pattern!(Args; mut arg_children) = args_node.clone() {
        assert!(arg_children.len() == 2);
        let body = arg_children.pop().unwrap();
        let cond_node = arg_children.pop().unwrap();
        if let rule_node_pattern!(Val => val_node) = cond_node {
            let val = eval_val_node(val_node.clone(), state)?;
            let cond = if val.ty == Type::Bool.into() {
                val.try_as_bool()?
            } else {
                return Err(format!("Expected boolean for if condition, found {:?}", val).into());
            };

            if cond {
                // Evaluate and return first argument
                if let rule_node_pattern!(Args; mut body_children) = body {
                    assert!(body_children.len() == 2);

                    let (_, arg1) = (body_children.pop(), body_children.pop().unwrap());

                    if let rule_node_pattern!(Val => arg1_val) = arg1 {
                        eval_val_node(arg1_val, state)
                    } else {
                        Err(format!(
                            "Expected Val node when parsing if statement, found {:?}",
                            arg1
                        )
                        .into())
                    }
                } else {
                    Err(format!(
                        "Expected Args node when parsing if statement, found {:?}",
                        body
                    )
                    .into())
                }
            } else {
                // Evaluate and return second argument
                if let rule_node_pattern!(Args; mut body_children) = body {
                    assert!(body_children.len() == 2);

                    let arg2 = body_children.pop().unwrap();

                    if let rule_node_pattern!(Args; mut arg2_children) = arg2 {
                        assert!(arg2_children.len() == 1);

                        let arg2_node = arg2_children.pop().unwrap();

                        if let rule_node_pattern!(Val => arg2_val) = arg2_node {
                            eval_val_node(arg2_val, state)
                        } else {
                            Err(format!(
                                "Expected Val node when parsing second branch of if statement, found {:?}",
                                arg2_node
                            )
                            .into())
                        }
                    } else {
                        Err(format!(
                            "Expected Args node when parsing second branch of if statement, found {:?}",
                            arg2
                        )
                        .into())
                    }
                } else {
                    Err(format!(
                        "Expected Args node when parsing if statement, found {:?}",
                        body
                    )
                    .into())
                }
            }
        } else {
            Err(format!("Expected Val node, found: {:?}", cond_node).into())
        }
    } else {
        Err(format!("Expected Args node, found: {:?}", args_node).into())
    }
}

fn eval_func_call_node<R, W>(node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
    if let rule_node_pattern!(FuncCall; mut children) = node {
        assert!(children.len() == 2);

        let args_node = children.pop().unwrap();

        match children.pop().unwrap() {
            leaf_node_pattern!(Reserved(rsv)) => {
                // The number of Val-type arguments that should be skipped at the start
                let cnt = match rsv {
                    ReservedIdent::Init => 2,
                    ReservedIdent::Def | ReservedIdent::Set => 1,
                    _ => 0,
                };

                // Control flow needs to be handled in this method
                if rsv == ReservedIdent::While {
                    Ok(eval_while_loop(args_node, state)?.into())
                } else if rsv == ReservedIdent::If {
                    eval_if_statement(args_node, state)
                } else {
                    let args = eval_args_node(args_node, state, cnt)?;

                    for arg in args.iter() {
                        if arg.is_val() && arg.try_get_val_type()? == AbstractType::Return {
                            return Ok(arg.try_get_val()?.clone());
                        }
                    }

                    eval_function(rsv, args, state)
                }
            }
            n => Err(format!("Expected function name, found {:?}", n).into()),
        }
    } else {
        Err(format!("Expected FuncCall node, found: {:?}", node).into())
    }
}

fn eval_args_node<R, W>(
    node: Node,
    state: &mut State<R, W>,
    cnt: usize,
) -> InterpreteResult<Vec<Argument>>
where
    R: Read,
    W: Write,
{
    if cnt > 0 {
        if let rule_node_pattern!(Args; mut children) = node {
            if children.len() == 1 {
                let child = children.pop().unwrap();

                return Ok(vec![eval_special_arg(child)?]);
            } else {
                let (tail, arg) = (children.pop().unwrap(), children.pop().unwrap());

                return Ok([eval_special_arg(arg)?]
                    .iter()
                    .chain(eval_args_node(tail, state, cnt - 1)?.iter())
                    .cloned()
                    .collect());
            }
        } else {
            return Err(format!(
                "Found normal Value where Type or Ident was expected: {:?}",
                node
            )
            .into());
        }
    }

    if let rule_node_pattern!(Args; mut children) = node {
        if children.len() == 1 {
            // Reached terminal state, nearly done
            match children.pop().unwrap() {
                rule_node_pattern!(Val => node) => {
                    let val = eval_val_node(node, state)?;

                    Ok([val.into()].to_vec())
                }
                n => Err(format!("Expected Val while parsing ListBody, found: {:?}", n).into()),
            }
        } else {
            assert!(children.len() == 2);

            let mut tail = eval_args_node(children.pop().unwrap(), state, 0)?;
            let val = eval_val_node(children.pop().unwrap(), state)?;

            if val.ty == AbstractType::Return {
                return Ok([val.into()].to_vec());
            }

            let mut res = vec![val.into()];
            res.append(&mut tail);

            Ok(res)
        }
    } else {
        Err(format!("Expected ListBody node, found: {:?}", node).into())
    }
}

fn eval_list_node<R, W>(node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
    if let Node::Rule(RuleNodeData {
        rule: Rule::List,
        mut children,
    }) = node
    {
        assert!(children.len() == 1);

        let val = eval_list_body_node(children.pop().unwrap(), state)?;
        if val.ty == AbstractType::Return {
            return Ok(val.clone());
        }

        if let Value {
            val: ValueData::List(vals),
            ..
        } = val
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

fn eval_list_body_node<R, W>(node: Node, state: &mut State<R, W>) -> InterpreteResult<Value>
where
    R: Read,
    W: Write,
{
    if let rule_node_pattern!(ListBody; mut children) = node {
        if children.len() == 1 {
            // Reached terminal state, nearly done
            match children.pop().unwrap() {
                rule_node_pattern!(Val => node) => {
                    let val = eval_val_node(node, state)?;

                    if val.ty == AbstractType::Return {
                        return Ok(val);
                    }
                    Ok(list_value_helper![val])
                }
                n => Err(format!("Expected Val while parsing ListBody, found: {:?}", n).into()),
            }
        } else {
            assert!(children.len() == 2);

            let tail_node = children.pop().unwrap();

            let val = eval_val_node(children.pop().unwrap(), state)?;
            if val.ty == AbstractType::Return {
                return Ok(val);
            }

            let tail = eval_list_body_node(tail_node, state)?;
            if tail.ty == AbstractType::Return {
                return Ok(tail);
            }

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
        ty => Err(format!("Cannot type-check list with type {:?}", ty).into()),
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        error::InterpreTestResult,
        lexer::tokenize,
        macros::{assert_fails, do_interpret_test},
        parser::parse_prog,
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
                    let mut tokens = tokenize(input)?;
                    let prog = parse_prog(&mut tokens)?;
                    let value = eval(prog)?;

                    assert_eq!(value, $value);
                }
            )+
        };
    }

    #[test]
    fn basic_val_test() -> InterpreTestResult {
        let input1 = "(-1.2)".chars().collect();
        let mut tokens1 = tokenize(input1)?;
        let prog1 = parse_prog(&mut tokens1)?;
        let value1 = eval(prog1)?;
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

    #[test]
    fn tostring_test() {
        let vals: Vec<Value> = vec![
            1i64.into(),
            1.0.into(),
            true.into(),
            ().into(),
            b'a'.into(),
            "ABCDE".into(),
            Value::new(
                Type::List(Box::new(Type::Int)).into(),
                ValueData::List(vec![
                    12i64.into(),
                    14i64.into(),
                    16i64.into(),
                    123512351325i64.into(),
                ]),
            ),
        ];

        let s = vals.iter().map(|val| val.to_string()).collect::<Vec<_>>();

        assert_eq!(
            s,
            vec![
                "1",
                "1",
                "true",
                "()",
                "'a'",
                "\"ABCDE\"",
                "[12, 14, 16, 123512351325]"
            ]
        );
    }

    #[test]
    fn while_loop_test() -> InterpreTestResult {
        do_interpret_test!([
            "([
                (def a 10u)
                (while (lt a 20) (set a (+ a 1)))
                (return (+ a 5))
            ])",
            Value::new(AbstractType::Return, ValueData::UInt(25))
        ]);

        Ok(())
    }

    #[test]
    fn if_statement_test() -> InterpreTestResult {
        do_interpret_test!([
            "([
                (def a 10u)
                (if (true) (set a (* a 2)) (set a (/ a 2)))
                (if (ne a 20) (set a 10000) (set a (/ a 3)))
                (return (+ a 5))
            ])",
            Value::new(AbstractType::Return, ValueData::UInt(11))
        ]);

        Ok(())
    }
}
