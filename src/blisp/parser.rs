use std::rc::Rc;

use crate::error::{InterpretError, InterpreteResult};

use super::{
    lexer::{NumLiteral, ReservedIdent, Token, Type},
    macros::{rule_node_helper, val_pattern},
};

// usize is the number of tokens "consumed"
type ParseResult = InterpreteResult<(Node, usize)>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Rule {
    Prog,
    Expr,
    ExprBody,
    Val,
    List,
    ListBody,
    FuncCall,
    Args,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseToken {
    NumLiteral(NumLiteral),
    CharLiteral(u8),
    UnitLiteral,
    StringLiteral(String),
    Ident(String),
    Type(Type),
    Reserved(ReservedIdent),
}

impl From<NumLiteral> for ParseToken {
    fn from(value: NumLiteral) -> Self {
        Self::NumLiteral(value)
    }
}

impl From<u8> for ParseToken {
    fn from(value: u8) -> Self {
        Self::CharLiteral(value)
    }
}

impl From<String> for ParseToken {
    fn from(value: String) -> Self {
        Self::StringLiteral(value)
    }
}
impl From<&str> for ParseToken {
    fn from(value: &str) -> Self {
        Self::StringLiteral(value.to_string())
    }
}

impl From<Type> for ParseToken {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
}

impl From<ReservedIdent> for ParseToken {
    fn from(value: ReservedIdent) -> Self {
        Self::Reserved(value)
    }
}

impl TryFrom<Token> for ParseToken {
    type Error = InterpretError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::NumLiteral(n) => Ok(Self::NumLiteral(n)),
            Token::CharLiteral(c) => Ok(Self::CharLiteral(c)),
            Token::UnitLiteral => Ok(Self::UnitLiteral),
            Token::StringLiteral(s) => Ok(Self::StringLiteral(s)),
            Token::Ident(i) => Ok(Self::Ident(i)),
            Token::Type(t) => Ok(Self::Type(t)),
            Token::Reserved(r) => Ok(Self::Reserved(r)),
            t => Err(format!("{:?} is not a valid ParseToken", t).into()),
        }
    }
}

pub fn parse_prog(tokens: &[Token]) -> ParseResult {
    let (child, cnt) = parse_expr(tokens)?;
    let node = rule_node_helper!(Prog, child);

    if tokens
        .get(cnt)
        .ok_or("Unexpected end of token stream before EOF")?
        != &Token::EOF
    {
        Err(format!("Unexpected token where EOF was expected: {:?}", tokens[cnt]).into())
    } else {
        Ok((node, cnt))
    }
}

fn parse_expr(tokens: &[Token]) -> ParseResult {
    if tokens[0] == Token::LParen {
        let (child, cnt) = parse_expr_body(&tokens[1..])?;
        let node = rule_node_helper!(Expr, [child]);

        if tokens[cnt + 1] == Token::RParen {
            Ok((node, cnt + 2))
        } else {
            Err(format!(
                "Expected ) while parsing expression, encountered {:?}",
                tokens[cnt + 1]
            )
            .into())
        }
    } else {
        Err(format!(
            "Expected ( while parsing expression, encountered {:?}",
            tokens[0]
        )
        .into())
    }
}

fn parse_expr_body(tokens: &[Token]) -> ParseResult {
    match &tokens[0] {
        Token::Reserved(_) => {
            let (child, cnt) = parse_func_call(&tokens[0..])?;
            let node = rule_node_helper!(ExprBody, child);

            Ok((node, cnt))
        }
        val_pattern!() => {
            // We have <Val> and need to process it
            let (child, cnt) = parse_val(&tokens[0..])?;
            let node = rule_node_helper!(ExprBody, child);

            Ok((node, cnt))
        }
        t => Err(format!(
            "Unexpected token encountered while parsing expression body: {:?}",
            t
        )
        .into()),
    }
}

fn parse_func_call(tokens: &[Token]) -> ParseResult {
    let func = tokens[0].assert_reserved()?;

    let (child, cnt) = parse_args(&tokens[1..])?;
    let node = rule_node_helper!(FuncCall, [Node::Leaf(ParseToken::from(*func)), child]);

    Ok((node, cnt + 1))
}

fn parse_args(tokens: &[Token]) -> ParseResult {
    match &tokens[0] {
        val_pattern!() => {
            // We have <Val> and need to process it
            let (val, val_cnt) = parse_val(tokens)?;

            Ok(
                if tokens.get(val_cnt).ok_or::<InterpretError>(
                    "Unexpectedly reached end of input while parsing arguments".into(),
                )? == &Token::RParen
                {
                    (rule_node_helper!(Args, val), val_cnt)
                } else {
                    let (tail, tail_cnt) = parse_args(&tokens[val_cnt..])?;
                    (rule_node_helper!(Args, [val, tail]), val_cnt + tail_cnt)
                },
            )
        }
        t => Err(format!(
            "Unexpected token encountered while parsing arguments: {:?}",
            t
        )
        .into()),
    }
}

fn parse_val(tokens: &[Token]) -> ParseResult {
    match &tokens[0] {
        Token::LBrack => {
            let (child, cnt) = parse_list(tokens)?;
            let node = rule_node_helper!(Val, child);

            Ok((node, cnt))
        }
        Token::LParen => {
            let (child, cnt) = parse_expr(tokens)?;
            let node = rule_node_helper!(Val, child);

            Ok((node, cnt))
        }
        // terminals specifies that we want to leave out LBrack and LParen
        val_pattern!(terminals) => {
            let child = Node::Leaf(tokens[0].clone().try_into()?);
            let node = rule_node_helper!(Val, child);

            Ok((node, 1))
        }
        _ => Err(format!("Unexpected token while parsing value: {:?}", tokens[0]).into()),
    }
}

fn parse_list(tokens: &[Token]) -> ParseResult {
    if tokens[0] == Token::LBrack {
        let (child, cnt) = parse_list_body(&tokens[1..])?;
        let node = rule_node_helper!(List, [child.clone()]);

        if tokens[cnt + 1] == Token::RBrack {
            Ok((node, cnt + 2))
        } else {
            Err(format!(
                "Expected ] while parsing list, encountered {:?}",
                tokens[cnt + 1]
            )
            .into())
        }
    } else {
        Err(format!("Expected [ while parsing list, encountered {:?}", tokens[0]).into())
    }
}

fn parse_list_body(tokens: &[Token]) -> ParseResult {
    match &tokens[0] {
        val_pattern!() => {
            // We have <Val> and need to process it
            let (val, val_cnt) = parse_val(tokens)?;

            Ok(
                if tokens.get(val_cnt).ok_or::<InterpretError>(
                    "Unexpectedly reached end of input while trying to parse list".into(),
                )? == &Token::RBrack
                {
                    (rule_node_helper!(ListBody, val), val_cnt)
                } else {
                    let (tail, tail_cnt) = parse_list_body(&tokens[val_cnt..])?;
                    (rule_node_helper!(ListBody, [val, tail]), val_cnt + tail_cnt)
                },
            )
        }
        t => Err(format!(
            "Unexpected token encountered while parsing expression body: {:?}",
            t
        )
        .into()),
    }
}

// Want to create functions that "execute a rule" by gobbling tokens and return Nodes
pub struct ParseTree {
    prog: Node,
}

impl ParseTree {
    pub fn init(input: Vec<Token>) -> InterpreteResult<Self> {
        let (prog, _) = parse_prog(&input)?;

        Ok(Self { prog })
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Node {
    Leaf(ParseToken),
    Rule(RuleNodeData),
}

impl Node {
    pub fn is_val(&self) -> bool {
        matches!(
            self,
            Self::Rule(RuleNodeData {
                rule: Rule::Val,
                ..
            })
        )
    }
    pub fn is_func_call(&self) -> bool {
        matches!(
            self,
            Self::Rule(RuleNodeData {
                rule: Rule::FuncCall,
                ..
            })
        )
    }
}

impl From<ParseToken> for Node {
    fn from(value: ParseToken) -> Self {
        Node::Leaf(value)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct RuleNodeData {
    pub(crate) rule: Rule,
    pub(crate) children: Vec<Node>,
}

impl RuleNodeData {
    pub fn new(rule: Rule, children: Vec<Node>) -> Self {
        Self { rule, children }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        blisp::{
            lexer::{tokenize, NumLiteral, Token, Type},
            macros::{
                assert_fails, assert_fails_parser, func_call_node_helper, list_node_helper,
                prog_node_helper, val_node_helper,
            },
            parser::parse_val,
        },
        error::InterpreTestResult,
    };

    use super::*;

    macro_rules! do_parse_test {
        ($([$input:expr, $node:expr, $count:literal]),+) => {
            {
                $(
                    {
                        let input = $input.chars().collect();
                        let tokens = tokenize(input)?;

                        assert_eq!(parse_prog(&tokens)?, ($node, $count));
                    }
                )+

                Ok(())
            }
        };
    }

    #[test]
    fn parse_literal_val_test() -> InterpreTestResult {
        do_parse_test!(
            [
                "(-1.24f)",
                prog_node_helper!(val_node_helper!(ParseToken::from(
                    NumLiteral::new_float_with_suffix(1, 24, true, 'f')
                ))),
                3
            ],
            [
                "(arstien)",
                prog_node_helper!(val_node_helper!(ParseToken::Ident("arstien".to_string()))),
                3
            ],
            [
                "(uint)",
                prog_node_helper!(val_node_helper!(ParseToken::Type(Type::UInt))),
                3
            ],
            [
                "('a')",
                prog_node_helper!(val_node_helper!(ParseToken::CharLiteral(b'a'))),
                3
            ],
            [
                "(\"teststr\")",
                prog_node_helper!(val_node_helper!(ParseToken::StringLiteral(
                    "teststr".to_string()
                ))),
                3
            ],
            [
                "(())",
                prog_node_helper!(val_node_helper!(ParseToken::UnitLiteral)),
                3
            ]
        )
    }

    #[test]
    fn parse_list_test() -> InterpreTestResult {
        let node1 = list_node_helper!(
            val_node_helper!(ParseToken::from(NumLiteral::new_float(12, 4, false))),
            val_node_helper!(ParseToken::CharLiteral(b'c')),
            val_node_helper!(ParseToken::from("ABCD".to_string()))
        );
        let node2 = list_node_helper!(
            val_node_helper!(ParseToken::from(NumLiteral::new_float(14, 6, true))),
            val_node_helper!([list_node_helper!(
                val_node_helper!(ParseToken::from(NumLiteral::new_float(12, 4, false))),
                val_node_helper!(ParseToken::CharLiteral(b'c')),
                val_node_helper!(ParseToken::from("ABCD".to_string()))
            )]),
            val_node_helper!(ParseToken::UnitLiteral)
        );

        do_parse_test!(
            [
                "([12.4 'c' \"ABCD\"])",
                prog_node_helper!(val_node_helper!([node1])),
                7
            ],
            [
                "([-14.6 [12.4 'c' \"ABCD\"] ()])",
                prog_node_helper!(val_node_helper!([node2])),
                11
            ]
        )
    }

    #[test]
    fn parse_func_call_test() -> InterpreTestResult {
        let node1 = func_call_node_helper!(
            Add,
            [
                val_node_helper!(ParseToken::from("ASTR")),
                val_node_helper!(ParseToken::from(b'a')),
                val_node_helper!(ParseToken::from(NumLiteral::new_int_with_suffix(
                    1, true, 'u'
                )))
            ]
        );
        let node2 = func_call_node_helper!(
            ToString,
            [
                val_node_helper!(ParseToken::from("ANTS")),
                val_node_helper!([list_node_helper!(
                    val_node_helper!(ParseToken::from("ASTR")),
                    val_node_helper!(ParseToken::from(b'a')),
                    val_node_helper!(ParseToken::from(NumLiteral::new_int_with_suffix(
                        1, true, 'u'
                    )))
                )]),
                val_node_helper!(ParseToken::from(b'b'))
            ]
        );

        do_parse_test!(
            ["(+ \"ASTR\" 'a' -1u)", prog_node_helper!(node1), 6],
            [
                "(tostring \"ANTS\" [\"ASTR\" 'a' -1u] 'b')",
                prog_node_helper!(node2),
                10
            ]
        )
    }
}
