use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::Files,
};
use color_eyre::eyre::{self, eyre};

use crate::{span::Span, token::Kind};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        found: Kind,
        expected: Vec<Kind>,
        span: Span,
    },
    MissingToken {
        expected: Vec<Kind>,
        span: Span,
    },
    UnexpectedEof {
        expected: Vec<Kind>,
        span: Span,
    },
    UnterminatedString {
        span: Span,
    },
    InvalidNumber {
        literal: String,
        span: Span,
    },
    InvalidOperator {
        found: Kind,
        span: Span,
    },
    Generic {
        message: String,
        span: Span,
    },
}

macro_rules! format_kinds {
    ($kinds:expr) => {
        $kinds
            .iter()
            .map(|k| format!("{:?}", k))
            .collect::<Vec<_>>()
            .join(", ")
    };
}

macro_rules! format_kinds_truncated {
    ($kinds:expr) => {{
        let string = $kinds
            .iter()
            .map(|k| format!("{:?}", k))
            .collect::<Vec<_>>()
            .join(", ");

        let words: Vec<&str> = string.split_whitespace().collect();

        if words.len() <= 3 {
            string
        } else {
            let mut result = words[..3].join(" ");
            result.push_str("...");
            result
        }
    }};

    ($kinds:expr, $n:expr) => {{
        let string = $kinds
            .iter()
            .map(|k| format!("{:?}", k))
            .collect::<Vec<_>>()
            .join(", ");

        let words: Vec<&str> = string.split_whitespace().collect();

        if words.len() <= $n {
            string
        } else {
            let mut result = words[..$n].join(" ");
            result.push_str("...");
            result
        }
    }};
}

impl ParseError {
    pub fn into_diagnostic<'a, F>(self, file_id: F::FileId) -> Diagnostic<F::FileId>
    where
        F: Files<'a>,
    {
        match self {
            Self::UnexpectedToken {
                found,
                expected,
                span,
            } => Diagnostic::error()
                .with_message(format!("Unexpected token `{found:?}`"))
                .with_code("E001P")
                .with_labels(vec![
                    Label::primary(file_id, span.range()).with_message("here"),
                    Label::secondary(file_id, span.range()).with_message(format!(
                        "expected one of {}",
                        format_kinds_truncated!(expected, 3)
                    )),
                ])
                .with_notes(vec![
                    "Expected one of:".to_string(),
                    format_kinds!(expected),
                ]),
            Self::MissingToken { expected, span } => Diagnostic::error()
                .with_message("Missing token")
                .with_code("E002P")
                .with_label(Label::primary(file_id, span.range()).with_message("here"))
                .with_notes(vec![
                    "Expected one of:".to_string(),
                    format_kinds!(expected),
                ]),

            Self::UnexpectedEof { expected, span } => Diagnostic::error()
                .with_message("Unexpected end of file")
                .with_code("E003P")
                .with_label(Label::primary(file_id, span.range()).with_message("here"))
                .with_notes(vec![
                    "Expected one of:".to_string(),
                    format_kinds!(expected),
                ]),

            Self::UnterminatedString { span } => Diagnostic::error()
                .with_message("Unterminated string literal")
                .with_code("E004P")
                .with_label(Label::primary(file_id, span.range()).with_message("starting here")),

            Self::InvalidNumber { span, literal } => Diagnostic::error()
                .with_message(format!("Invalid numeric literal `{literal:?}`"))
                .with_code("E005P")
                .with_label(Label::primary(file_id, span.range()).with_message("here")),

            Self::InvalidOperator { span, found } => Diagnostic::error()
                .with_message(format!("Invalid operator `{found:?}`"))
                .with_code("E006P")
                .with_label(Label::primary(file_id, span.range()).with_message("here")),

            Self::Generic { span, message } => Diagnostic::error()
                .with_message(message)
                .with_code("E007P")
                .with_label(Label::primary(file_id, span.range()).with_message("here")),
        }
    }
}

impl From<ParseError> for eyre::Report {
    fn from(value: ParseError) -> Self {
        match value {
            ParseError::UnexpectedToken {
                found,
                expected,
                span,
            } => eyre!(
                "Unexpected token `{:?}` at {}. Expected one of [{}]",
                found,
                span,
                format_kinds!(expected)
            ),
            ParseError::MissingToken { expected, span } => eyre!(
                "Missing token at {}. Expected one of [{}]",
                span,
                format_kinds!(expected)
            ),
            ParseError::UnexpectedEof { expected, span } => eyre!(
                "Unexpected end of input at {span}. expected one of: [{}]",
                expected
                    .iter()
                    .map(|k| format!("{k:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            ParseError::UnterminatedString { span } => {
                eyre!("Unterminated string literal starting at {span}")
            }

            ParseError::InvalidNumber { literal, span } => {
                eyre!("Invalid number literal `{literal}` at {span}")
            }

            ParseError::InvalidOperator { found, span } => {
                eyre!("Invalid operator `{found:?}` at {span}")
            }

            ParseError::Generic { message, span } => eyre!("{message} at {span}"),
        }
    }
}
