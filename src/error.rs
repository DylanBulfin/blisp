use std::{
    error::{self, Error},
    fmt::{self, Display, Formatter},
    io,
    result::{self, Result},
};

#[derive(PartialEq, Eq, Debug)]
enum InterpretErrorType {
    None,
    IOError,
}

impl Display for InterpretErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::None => "None",
            Self::IOError => "IOError",
        })
    }
}

#[derive(Debug)]
pub struct InterpretError {
    message: String,
    err_type: InterpretErrorType,
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.err_type == InterpretErrorType::None {
            f.write_fmt(format_args!("{}", self.message))
        } else {
            f.write_fmt(format_args!(
                "Error: {}, Message:{}",
                self.err_type, self.message
            ))
        }
    }
}

impl Error for InterpretError {}

impl From<String> for InterpretError {
    fn from(message: String) -> Self {
        Self {
            message,
            err_type: InterpretErrorType::None,
        }
    }
}

impl From<&str> for InterpretError {
    fn from(message: &str) -> Self {
        Self {
            message: message.to_string(),
            err_type: InterpretErrorType::None,
        }
    }
}

impl From<io::Error> for InterpretError {
    fn from(value: io::Error) -> Self {
        Self {
            err_type: InterpretErrorType::IOError,
            message: value.to_string(),
        }
    }
}

pub type InterpreteResult<T> = Result<T, InterpretError>;
pub type InterpreTestResult = InterpreteResult<()>;
