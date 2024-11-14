use std::collections::VecDeque;

use crate::error::{InterpretError, InterpreteResult};

use crate::macros::{peek_tok_safe, pop_tok_safe};
use crate::{
    lexer::{NumLiteral, ReservedIdent, Token, Type},
    macros::{rule_node_helper, val_pattern},
};

// usize is the number of tokens "consumed"
type ParseResult = InterpreteResult<Node>;

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

pub fn parse_prog(tokens: &mut VecDeque<Token>) -> ParseResult {
    let child = parse_expr(tokens)?;
    let node = rule_node_helper!(Prog, child);

    match pop_tok_safe!(tokens, "Prog") {
        Token::EOF => Ok(node),
        t => Err(format!("Unexpected token where EOF was expected: {:?}", t).into()),
    }
}

fn parse_expr(tokens: &mut VecDeque<Token>) -> ParseResult {
    match pop_tok_safe!(tokens, "Expr") {
        Token::LParen => {
            let child = parse_expr_body(tokens)?;
            let node = rule_node_helper!(Expr, [child]);

            match pop_tok_safe!(tokens, "Expr") {
                Token::RParen => Ok(node),
                t => {
                    Err(format!("Expected ) while parsing expression, encountered {:?}", t).into())
                }
            }
        }
        t => Err(format!("Expected ( while parsing expression, encountered {:?}", t).into()),
    }
}

fn parse_expr_body(tokens: &mut VecDeque<Token>) -> ParseResult {
    match peek_tok_safe!(tokens, "ExprBody") {
        Token::Reserved(_) => {
            let child = parse_func_call(tokens)?;
            let node = rule_node_helper!(ExprBody, child);

            Ok(node)
        }
        val_pattern!() => {
            // We have <Val> and need to process it
            let child = parse_val(tokens)?;
            let node = rule_node_helper!(ExprBody, child);

            Ok(node)
        }
        t => Err(format!(
            "Unexpected token encountered while parsing expression body: {:?}",
            t
        )
        .into()),
    }
}

fn parse_func_call(tokens: &mut VecDeque<Token>) -> ParseResult {
    let func = pop_tok_safe!(tokens, "FuncCall").assert_reserved()?;

    let child = parse_args(tokens)?;
    let node = rule_node_helper!(FuncCall, [Node::Leaf(ParseToken::from(func)), child]);

    Ok(node)
}

fn parse_args(tokens: &mut VecDeque<Token>) -> ParseResult {
    match peek_tok_safe!(tokens, "Args") {
        val_pattern!() => {
            // We have <Val> and need to process it
            let val = parse_val(tokens)?;

            Ok(if peek_tok_safe!(tokens, "Args") == &Token::RParen {
                rule_node_helper!(Args, val)
            } else {
                let tail = parse_args(tokens)?;
                rule_node_helper!(Args, [val, tail])
            })
        }
        t => Err(format!(
            "Unexpected token encountered while parsing arguments: {:?}",
            t
        )
        .into()),
    }
}

fn parse_val(tokens: &mut VecDeque<Token>) -> ParseResult {
    match peek_tok_safe!(tokens, "Val") {
        Token::LBrack => {
            let child = parse_list(tokens)?;
            let node = rule_node_helper!(Val, child);

            Ok(node)
        }
        Token::LParen => {
            let child = parse_expr(tokens)?;
            let node = rule_node_helper!(Val, child);

            Ok(node)
        }
        // terminals specifies that we want to leave out LBrack and LParen
        val_pattern!(terminals) => {
            let child = Node::Leaf(pop_tok_safe!(tokens, "Val").try_into()?);
            let node = rule_node_helper!(Val, child);

            Ok(node)
        }
        t => Err(format!("Unexpected token while parsing value: {:?}", t).into()),
    }
}

fn parse_list(tokens: &mut VecDeque<Token>) -> ParseResult {
    if pop_tok_safe!(tokens, "List") == Token::LBrack {
        let child = parse_list_body(tokens)?;
        let node = rule_node_helper!(List, [child]);

        match pop_tok_safe!(tokens, "List") {
            Token::RBrack => Ok(node),
            t => Err(format!("Expected ] while parsing list, encountered {:?}", t).into()),
        }
    } else {
        Err(format!("Expected [ while parsing list, encountered {:?}", tokens[0]).into())
    }
}

fn parse_list_body(tokens: &mut VecDeque<Token>) -> ParseResult {
    match peek_tok_safe!(tokens, "ListBody") {
        val_pattern!() => {
            // We have <Val> and need to process it
            let val = parse_val(tokens)?;

            Ok(if peek_tok_safe!(tokens, "ListBody") == &Token::RBrack {
                rule_node_helper!(ListBody, val)
            } else {
                let tail = parse_list_body(tokens)?;
                rule_node_helper!(ListBody, [val, tail])
            })
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
        error::InterpreTestResult,
        lexer::{tokenize, NumLiteral, Token, Type},
        macros::{
            assert_fails, assert_fails_parser, func_call_node_helper, list_node_helper,
            prog_node_helper, val_node_helper,
        },
        parser::parse_val,
    };

    use super::*;

    macro_rules! do_parse_test {
        ($([$input:expr, $node:expr]),+) => {
            {
                $(
                    {
                        let input = $input.chars().collect();
                        let mut tokens = tokenize(input)?;

                        assert_eq!(parse_prog(&mut tokens)?, $node);
                        assert!(tokens.len() == 0);
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
                )))
            ],
            [
                "(arstien)",
                prog_node_helper!(val_node_helper!(ParseToken::Ident("arstien".to_string())))
            ],
            [
                "(uint)",
                prog_node_helper!(val_node_helper!(ParseToken::Type(Type::UInt)))
            ],
            [
                "('a')",
                prog_node_helper!(val_node_helper!(ParseToken::CharLiteral(b'a')))
            ],
            [
                "(\"teststr\")",
                prog_node_helper!(val_node_helper!(ParseToken::StringLiteral(
                    "teststr".to_string()
                )))
            ],
            [
                "(())",
                prog_node_helper!(val_node_helper!(ParseToken::UnitLiteral))
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
                prog_node_helper!(val_node_helper!([node1]))
            ],
            [
                "([-14.6 [12.4 'c' \"ABCD\"] ()])",
                prog_node_helper!(val_node_helper!([node2]))
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
            ["(+ \"ASTR\" 'a' -1u)", prog_node_helper!(node1)],
            [
                "(tostring \"ANTS\" [\"ASTR\" 'a' -1u] 'b')",
                prog_node_helper!(node2)
            ]
        )
    }
}
