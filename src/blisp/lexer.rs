use crate::error::{InterpretError, InterpreteResult};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LiteralSuffix {
    None,
    Unsigned,
    Float,
    Char,
}

impl From<char> for LiteralSuffix {
    fn from(value: char) -> Self {
        match value {
            'c' => LiteralSuffix::Char,
            'u' => LiteralSuffix::Unsigned,
            'f' => LiteralSuffix::Float,
            _ => panic!(
                "Attempted to convert invalid char {} to LiteralSuffix",
                value
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct NumLiteral {
    pub(crate) negative: bool,
    pub(crate) int_part: u64,
    pub(crate) float: bool,
    pub(crate) dec_part: u64,
    pub(crate) suffix: LiteralSuffix,
}

impl NumLiteral {
    pub fn new_int(int_part: u64, negative: bool) -> Self {
        Self {
            int_part,
            negative,
            suffix: LiteralSuffix::None,
            dec_part: 0,
            float: false,
        }
    }

    pub fn new_int_with_suffix(int_part: u64, negative: bool, suffix: char) -> Self {
        Self {
            int_part,
            negative,
            suffix: suffix.into(),
            dec_part: 0,
            float: false,
        }
    }

    pub fn new_float(int_part: u64, dec_part: u64, negative: bool) -> Self {
        Self {
            int_part,
            dec_part,
            negative,
            float: true,
            suffix: LiteralSuffix::None,
        }
    }

    pub fn new_float_with_suffix(
        int_part: u64,
        dec_part: u64,
        negative: bool,
        suffix: char,
    ) -> Self {
        Self {
            int_part,
            dec_part,
            negative,
            float: true,
            suffix: suffix.into(),
        }
    }

    pub fn to_f64_checked(self) -> InterpreteResult<f64> {
        if !self.float && self.suffix != LiteralSuffix::Float {
            return Err(format!("Unable to convert NumLiteral {:?} to f64", self).into());
        }

        let val = if self.dec_part == 0 {
            self.int_part as f64
        } else {
            let mag = self.dec_part.ilog10();

            self.int_part as f64 + ((self.dec_part as f64) / (10u64.pow(mag + 1) as f64))
        };

        if self.negative {
            Ok(-val)
        } else {
            Ok(val)
        }
    }

    pub fn negative(&self) -> bool {
        self.negative
    }

    pub fn int_part(&self) -> u64 {
        self.int_part
    }

    pub fn float(&self) -> bool {
        self.float
    }

    pub fn dec_part(&self) -> u64 {
        self.dec_part
    }

    pub fn suffix(&self) -> LiteralSuffix {
        self.suffix
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ReservedIdent {
    // Math
    Add,
    Sub,
    Div,
    Mul,

    // I/O
    Write,
    Read,

    // Control flow
    If,
    While,

    // Boolean ops
    Eq,
    Neq,
    Leq,
    Geq,
    Lt,
    Gt,
    And,
    Or,

    // Vars
    Set,
    Init,
    Def,

    // Collections
    Concat,
    Prepend,
    Take,
    // Removed until/unless tuple type is reintroduced
    //Split,

    // Convenience
    Eval,
    ToString,
}

impl TryFrom<&str> for ReservedIdent {
    type Error = InterpretError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "add" => Ok(Self::Add),
            "sub" => Ok(Self::Sub),
            "div" => Ok(Self::Div),
            "mul" => Ok(Self::Mul),
            "write" => Ok(Self::Write),
            "read" => Ok(Self::Read),
            "if" => Ok(Self::If),
            "while" => Ok(Self::While),
            "eq" => Ok(Self::Eq),
            "neq" => Ok(Self::Neq),
            "leq" => Ok(Self::Leq),
            "geq" => Ok(Self::Geq),
            "lt" => Ok(Self::Lt),
            "gt" => Ok(Self::Gt),
            "and" => Ok(Self::And),
            "or" => Ok(Self::Or),
            "set" => Ok(Self::Set),
            "init" => Ok(Self::Init),
            "def" => Ok(Self::Def),
            "concat" => Ok(Self::Concat),
            "prepend" => Ok(Self::Prepend),
            "take" => Ok(Self::Take),
            "eval" => Ok(Self::Eval),
            "tostring" => Ok(Self::ToString),
            _ => Err("Not a valid reserved identifier".into()),
        }
    }
}
impl TryFrom<String> for ReservedIdent {
    type Error = InterpretError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int,
    UInt,
    Float,
    List(Box<Type>),
    Unit,
    Char,
    Bool,
    //String, // Probably want to leave out until a need arises, not sure if useful
}

impl TryFrom<&str> for Type {
    type Error = InterpretError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if !value.is_ascii() {
            return Err("Unable to convert non-ascii value to Type".into());
        }

        match value {
            "int" => Ok(Self::Int),
            "uint" => Ok(Self::UInt),
            "float" => Ok(Self::Float),
            "unit" => Ok(Self::Unit),
            "char" => Ok(Self::Char),
            "bool" => Ok(Self::Bool),
            _ => {
                if value.len() >= 5
                    && &value[0..5] == "list<"
                    && value.as_bytes()[value.len() - 1] == b'>'
                {
                    if let Ok(subtype) = Self::try_from(&value[5..value.len() - 1]) {
                        Ok(Self::List(Box::new(subtype)))
                    } else {
                        Err("Unable to parse subtype of list".into())
                    }
                } else if value.len() > 6
                    && &value[0..6] == "tuple<"
                    && value.as_bytes()[value.len() - 1] == b'>'
                {
                    unimplemented!()
                } else {
                    Err("Invalid type: {value}".into())
                }
            }
        }
    }
}
impl TryFrom<String> for Type {
    type Error = InterpretError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    NumLiteral(NumLiteral),
    CharLiteral(u8),
    UnitLiteral,
    StringLiteral(String),
    Ident(String),
    Type(Type),
    Reserved(ReservedIdent),
    LParen,
    RParen,
    LBrack,
    RBrack,
    EOF,
}

macro_rules! token_helper{
    ($([$isfunc:ident, $assfunc:ident, $var:ident, $innertype:ty]);+) => {
        impl Token {
            $(
                pub fn $isfunc(&self) -> bool {
                    match self {
                        Self::$var(_) => true,
                        _ => false,
                    }
                }

                pub fn $assfunc(&self) -> crate::error::InterpreteResult<&$innertype> {
                    match self {
                        Self::$var(v) => Ok(v),
                        _ => Err(format!("Assertion failed, self: {:?}", self).into()),
                    }
                }
            )+
        }
    };
}

token_helper!(
    [is_num, assert_num, NumLiteral, NumLiteral];
    [is_char, assert_char, CharLiteral, u8];
    [is_string, assert_string, StringLiteral, String];
    [is_ident, assert_ident, Ident, String];
    [is_type, assert_type, Type, Type];
    [is_reserved, assert_reserved, Reserved, ReservedIdent]
);

impl From<NumLiteral> for Token {
    fn from(value: NumLiteral) -> Self {
        Self::NumLiteral(value)
    }
}

impl From<ReservedIdent> for Token {
    fn from(value: ReservedIdent) -> Self {
        Self::Reserved(value)
    }
}

impl From<Type> for Token {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
}

// Easier testing
impl From<&str> for Token {
    fn from(value: &str) -> Self {
        Self::StringLiteral(value.to_string())
    }
}
impl From<String> for Token {
    fn from(value: String) -> Self {
        Self::StringLiteral(value)
    }
}

impl From<u8> for Token {
    fn from(value: u8) -> Self {
        Self::CharLiteral(value)
    }
}

fn handle_char_literal(input: &[char]) -> InterpreteResult<u8> {
    // input[0] points at opening `'`
    // we don't handle escape characters so we can assume that the body of the char literal will
    // take up exactly one byte of input.
    if *input
        .get(2)
        .ok_or("Reached end of input unexpectedly while parsing a char literal")?
        != '\''
    {
        Err("Did not find closing \' where expected while processing a char literal".into())
    } else {
        Ok(input[1] as u8)
    }
}

fn handle_string_literal(input: &[char]) -> InterpreteResult<String> {
    // Starting on character directly after opening "
    let mut curr_index = 1;
    let mut curr_str = String::new();

    loop {
        if curr_index >= input.len() {
            return Err("Unexpectedly reached end of input while parsing a string literal".into());
        }

        match input[curr_index] {
            '\"' => break,
            c => curr_str.push(c),
        }

        curr_index += 1;
    }

    Ok(curr_str)
}

// There are three cases for any identifier:
// 1. Reserved function name such as `add`. These are parsed into `Token::Reserve(..)`
// 2. Type name such as `int` or `list<tuple<int, char>>`, these are parsed to `Token::Type(..)`
// 3. User-defined name for variables, these are parsed to `Token::Ident`
//
// First I parse the identifier, including alphanumeric characters and `<>` (only valid in types)
fn handle_identifier(input: &[char]) -> InterpreteResult<(Token, usize)> {
    let mut curr_index = 0;
    let mut curr_ident = String::new();

    // Any identifier with <> must be a type, this allows me to ensure that I treat it as such
    let mut forced_type = false;

    loop {
        if curr_index >= input.len() {
            return Err("Unexpectedly reached end of input while parsing an identifier".into());
        }

        match input[curr_index] {
            'a'..='z' | 'A'..='Z' | '0'..='9' => {
                curr_ident.push(input[curr_index]);
            }
            '<' | '>' => {
                forced_type = true;
                curr_ident.push(input[curr_index]);
            }
            _ => break,
        }

        curr_index += 1;
    }

    let adj = curr_ident.len() - 1;

    if forced_type {
        Ok((Token::from(Type::try_from(curr_ident.as_str())?), adj))
    } else if let Ok(ty) = Type::try_from(curr_ident.as_str()) {
        Ok((Token::from(ty), adj))
    } else if let Ok(rsv) = ReservedIdent::try_from(curr_ident.as_str()) {
        Ok((Token::from(rsv), adj))
    } else {
        Ok((Token::Ident(curr_ident), adj))
    }
}

fn handle_num_literal(input: &[char]) -> InterpreteResult<(NumLiteral, usize)> {
    let mut curr_index = 0;

    let negative = input[0] == '-';
    if negative {
        curr_index += 1;
    }

    // This check explicitly ensures we have a digit at the start of the number before the real
    // parsing
    let mut int_part = input[curr_index].to_digit(10).ok_or(format!(
        "Unexpected char while parsing number: {}",
        input[curr_index]
    ))? as u64;
    curr_index += 1;

    let mut float = false;
    let mut dec_part = 0;
    let mut suffix = LiteralSuffix::None;

    loop {
        if curr_index >= input.len() {
            break;
        }
        match input[curr_index] {
            '0'..='9' => {
                if float {
                    dec_part *= 10;
                    dec_part += input[curr_index].to_digit(10).unwrap() as u64;
                } else {
                    int_part *= 10;
                    int_part += input[curr_index].to_digit(10).unwrap() as u64;
                }

                curr_index += 1;
            }
            '.' => {
                float = true;
                curr_index += 1;
            }
            'u' => {
                suffix = LiteralSuffix::Unsigned;
                curr_index += 1;
                break;
            }
            'f' => {
                suffix = LiteralSuffix::Float;
                curr_index += 1;
                break;
            }
            'c' => {
                suffix = LiteralSuffix::Char;
                curr_index += 1;
                break;
            }
            _ => break,
        }
    }

    Ok((
        NumLiteral {
            int_part,
            dec_part,
            float,
            suffix,
            negative,
        },
        curr_index,
    ))
}

pub fn tokenize(input: Vec<char>) -> InterpreteResult<Vec<Token>> {
    // This way I don't need to worry about testing for ascii in every method
    let input: Vec<char> = input.into_iter().filter(|c| c.is_ascii()).collect();

    let mut curr_index = 0;
    let mut res = Vec::new();

    loop {
        if curr_index >= input.len() {
            break;
        }

        match input[curr_index] {
            '+' => res.push(ReservedIdent::Add.into()),
            '/' => res.push(ReservedIdent::Div.into()),
            '*' => res.push(ReservedIdent::Mul.into()),
            '(' => {
                // Important to note that this means `( )` is not a valid unit literal
                if *input
                    .get(curr_index + 1)
                    .ok_or("Unexpectedly reached end of input")?
                    == ')'
                {
                    res.push(Token::UnitLiteral);
                    curr_index += 1;
                } else {
                    res.push(Token::LParen);
                }
            }
            ')' => res.push(Token::RParen),
            '[' => res.push(Token::LBrack),
            ']' => res.push(Token::RBrack),
            '0'..='9' => {
                let (lit, count) = handle_num_literal(&input[curr_index..])?;
                curr_index += count - 1;
                res.push(Token::NumLiteral(lit))
            }
            '\'' => {
                let c = handle_char_literal(&input[curr_index..])?;
                // Since a char literal takes up 3 characters
                curr_index += 2;
                res.push(Token::CharLiteral(c));
            }
            '\"' => {
                let s = handle_string_literal(&input[curr_index..])?;
                // Need to ultimately shift by s.len() + 2, including standard shift by 1
                curr_index += s.len() + 1;
                res.push(Token::StringLiteral(s));
            }
            '-' => {
                if *input
                    .get(curr_index + 1)
                    .ok_or("Unexpectedly reached end of input")?
                    == ' '
                {
                    res.push(ReservedIdent::Sub.into());
                } else {
                    let (lit, count) = handle_num_literal(&input[curr_index..])?;
                    curr_index += count - 1;
                    res.push(Token::NumLiteral(lit));
                }
            }
            'a'..='z' | 'A'..='Z' => {
                let (tok, adj) = handle_identifier(&input[curr_index..])?;
                res.push(tok);
                curr_index += adj;
            }
            ' ' => (),
            c => return Err(format!("Haven't implemented the char {}", c).into()),
        };

        curr_index += 1;
    }

    res.push(Token::EOF);

    Ok(res)
}

#[cfg(test)]
mod tests {
    use crate::error::InterpreTestResult;

    use super::*;

    #[test]
    fn parentheses() -> InterpreTestResult {
        let (input1, output1) = (
            // Without the space this is interpreted as a unit literal
            "(( ))".chars().collect(),
            [
                Token::LParen,
                Token::LParen,
                Token::RParen,
                Token::RParen,
                Token::EOF,
            ],
        );
        let (input2, output2) = (
            "((( ))".chars().collect(),
            [
                Token::LParen,
                Token::LParen,
                Token::LParen,
                Token::RParen,
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);
        assert_eq!(tokenize(input2)?, output2);

        Ok(())
    }

    #[test]
    fn brackets() -> InterpreTestResult {
        let (input1, output1) = (
            "([])".chars().collect(),
            [
                Token::LParen,
                Token::LBrack,
                Token::RBrack,
                Token::RParen,
                Token::EOF,
            ],
        );
        let (input2, output2) = (
            "([[])".chars().collect(),
            [
                Token::LParen,
                Token::LBrack,
                Token::LBrack,
                Token::RBrack,
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);
        assert_eq!(tokenize(input2)?, output2);

        Ok(())
    }

    #[test]
    fn num_literals() -> InterpreTestResult {
        let (input1, output1) = (
            "(1243)".chars().collect(),
            [
                Token::LParen,
                Token::from(NumLiteral::new_int(1243, false)),
                Token::RParen,
                Token::EOF,
            ],
        );
        let (input2, output2) = (
            "(-124.3)".chars().collect(),
            [
                Token::LParen,
                Token::from(NumLiteral::new_float(124, 3, true)),
                Token::RParen,
                Token::EOF,
            ],
        );
        let (input3, output3) = (
            "(-124f)".chars().collect(),
            [
                Token::LParen,
                Token::from(NumLiteral::new_int_with_suffix(124, true, 'f')),
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);
        assert_eq!(tokenize(input2)?, output2);
        assert_eq!(tokenize(input3)?, output3);

        Ok(())
    }

    #[test]
    fn math_test() -> InterpreTestResult {
        let (input1, output1) = (
            // The lexer does no syntax or type analysis so this is fine
            "(+  -15.23f 1243u )".chars().collect(),
            [
                Token::LParen,
                ReservedIdent::Add.into(),
                Token::from(NumLiteral::new_float_with_suffix(15, 23, true, 'f')),
                Token::from(NumLiteral::new_int_with_suffix(1243, false, 'u')),
                Token::RParen,
                Token::EOF,
            ],
        );
        let (input2, output2) = (
            "(- -124c (/ 0.3 123u))".chars().collect(),
            [
                Token::LParen,
                ReservedIdent::Sub.into(),
                Token::from(NumLiteral::new_int_with_suffix(124, true, 'c')),
                Token::LParen,
                ReservedIdent::Div.into(),
                Token::from(NumLiteral::new_float(0, 3, false)),
                Token::from(NumLiteral::new_int_with_suffix(123, false, 'u')),
                Token::RParen,
                Token::RParen,
                Token::EOF,
            ],
        );
        let (input3, output3) = (
            "(* (/ 2 9) -124f)".chars().collect(),
            [
                Token::LParen,
                ReservedIdent::Mul.into(),
                Token::LParen,
                ReservedIdent::Div.into(),
                Token::from(NumLiteral::new_int(2, false)),
                Token::from(NumLiteral::new_int(9, false)),
                Token::RParen,
                Token::from(NumLiteral::new_int_with_suffix(124, true, 'f')),
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);
        assert_eq!(tokenize(input2)?, output2);
        assert_eq!(tokenize(input3)?, output3);

        Ok(())
    }

    #[test]
    fn char_literal_test() -> InterpreTestResult {
        let (input1, output1) = (
            "(['a' 'n' '?'] 'Z' '0')".chars().collect(),
            [
                Token::LParen,
                Token::LBrack,
                Token::CharLiteral(b'a'),
                Token::CharLiteral(b'n'),
                Token::CharLiteral(b'?'),
                Token::RBrack,
                Token::CharLiteral(b'Z'),
                Token::CharLiteral(b'0'),
                Token::RParen,
                Token::EOF,
            ],
        );
        let (input2, output2) = (
            // ''' is valid because of how I naively parse char literals
            "(- (+ 'a' 1c) '`' ''')".chars().collect(),
            [
                Token::LParen,
                ReservedIdent::Sub.into(),
                Token::LParen,
                ReservedIdent::Add.into(),
                Token::CharLiteral(b'a'),
                Token::from(NumLiteral::new_int_with_suffix(1, false, 'c')),
                Token::RParen,
                Token::CharLiteral(b'`'),
                Token::CharLiteral(b'\''),
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);
        assert_eq!(tokenize(input2)?, output2);

        Ok(())
    }

    #[test]
    fn string_literal_test() -> InterpreTestResult {
        let (input1, output1) = (
            "([\"ABC\" \"TESTSTR\"] 'Z' \"FNIDENSIEN\")"
                .chars()
                .collect(),
            [
                Token::LParen,
                Token::LBrack,
                Token::from("ABC"),
                Token::from("TESTSTR"),
                Token::RBrack,
                Token::CharLiteral(b'Z'),
                Token::from("FNIDENSIEN"),
                Token::RParen,
                Token::EOF,
            ],
        );
        let (input2, output2) = (
            // ''' is valid because of how I naively parse char literals
            "(- (+ \"AIENdkfqw\" 1c) '`' \"AIENdenqiekS81\" \"))\\n\")"
                .chars()
                .collect(),
            [
                Token::LParen,
                ReservedIdent::Sub.into(),
                Token::LParen,
                ReservedIdent::Add.into(),
                Token::from("AIENdkfqw"),
                Token::from(NumLiteral::new_int_with_suffix(1, false, 'c')),
                Token::RParen,
                Token::CharLiteral(b'`'),
                Token::from("AIENdenqiekS81"),
                // Defining it as below to makes sure the escaping of the `\` is working
                Token::from(String::from_iter([')', ')', '\\', 'n'])),
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);
        assert_eq!(tokenize(input2)?, output2);

        Ok(())
    }

    #[test]
    fn type_ident_test() -> InterpreTestResult {
        // TODO Add and test the tuple type, parsing it will be annoying so I haven't
        // done it yet
        let (input1, output1) = (
            "(int uint float char list<char> list<list<uint>>)"
                .chars()
                .collect(),
            [
                Token::LParen,
                Token::Type(Type::Int),
                Token::Type(Type::UInt),
                Token::Type(Type::Float),
                Token::Type(Type::Char),
                Token::Type(Type::List(Box::new(Type::Char))),
                Token::Type(Type::List(Box::new(Type::List(Box::new(Type::UInt))))),
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);

        Ok(())
    }

    #[test]
    fn reserved_ident_test() -> InterpreTestResult {
        let (input1, output1) = (
            "(add + sub - div / mul * write read if while eq neq leq geq lt gt and or set init def concat prepend take eval tostring)".chars().collect(),
            [
                Token::LParen,
                ReservedIdent::Add.into(),
                ReservedIdent::Add.into(),
                ReservedIdent::Sub.into(),
                ReservedIdent::Sub.into(),
                ReservedIdent::Div.into(),
                ReservedIdent::Div.into(),
                ReservedIdent::Mul.into(),
                ReservedIdent::Mul.into(),
                ReservedIdent::Write.into(),
                ReservedIdent::Read.into(),
                ReservedIdent::If.into(),
                ReservedIdent::While.into(),
                ReservedIdent::Eq.into(),
                ReservedIdent::Neq.into(),
                ReservedIdent::Leq.into(),
                ReservedIdent::Geq.into(),
                ReservedIdent::Lt.into(),
                ReservedIdent::Gt.into(),
                ReservedIdent::And.into(),
                ReservedIdent::Or.into(),
                ReservedIdent::Set.into(),
                ReservedIdent::Init.into(),
                ReservedIdent::Def.into(),
                ReservedIdent::Concat.into(),
                ReservedIdent::Prepend.into(),
                ReservedIdent::Take.into(),
                ReservedIdent::Eval.into(),
                ReservedIdent::ToString.into(),
                Token::RParen,
                Token::EOF,
            ]
        );

        let (input2, output2) = (
            "(tostring \"ANTS\" [\"ASTR\" 'a' -1u] 'b')"
                .chars()
                .collect(),
            [
                Token::LParen,
                ReservedIdent::ToString.into(),
                Token::from("ANTS"),
                Token::LBrack,
                Token::from("ASTR"),
                Token::from(b'a'),
                Token::from(NumLiteral::new_int_with_suffix(1, true, 'u')),
                Token::RBrack,
                Token::from(b'b'),
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);
        assert_eq!(tokenize(input2)?, output2);

        Ok(())
    }

    #[test]
    fn variable_ident_test() -> InterpreTestResult {
        let (input1, output1) = (
            "(a3t int ade3 inavd iaerkds9 iaernds[)".chars().collect(),
            [
                Token::LParen,
                Token::Ident("a3t".to_string()),
                Token::Type(Type::Int),
                Token::Ident("ade3".to_string()),
                Token::Ident("inavd".to_string()),
                Token::Ident("iaerkds9".to_string()),
                Token::Ident("iaernds".to_string()),
                Token::LBrack,
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);

        Ok(())
    }

    #[test]
    fn list_test() -> InterpreTestResult {
        let (input1, output1) = (
            "([-14.6 [12.4 'c' \"ABCD\"] ()])".chars().collect(),
            [
                Token::LParen,
                Token::LBrack,
                Token::from(NumLiteral::new_float(14, 6, true)),
                Token::LBrack,
                Token::from(NumLiteral::new_float(12, 4, false)),
                Token::from(b'c'),
                Token::from("ABCD"),
                Token::RBrack,
                Token::UnitLiteral,
                Token::RBrack,
                Token::RParen,
                Token::EOF,
            ],
        );

        assert_eq!(tokenize(input1)?, output1);

        Ok(())
    }

}
