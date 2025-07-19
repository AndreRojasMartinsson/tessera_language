pub mod ast;
pub mod errors;
pub mod lexer;
pub mod operator;
pub mod parser;
pub mod span;
pub mod token;

#[macro_export]
macro_rules! node {
    ($name:ident $(< $($generic:ident),* >)? , { $( $(#[$doc:meta])* $field:ident: $ty:ty),* $(,)? }) => {
        #[derive(Debug, PartialEq, PartialOrd, Clone)]
        pub struct $name<'a $(, $($generic),* )?> {
            pub loc: $crate::span::Span,
            pub _marker: std::marker::PhantomData<&'a str>,

            $( $(#[$doc])* pub $field: $ty, )*

        }

        impl<'a $(, $($generic),* )?> $name<'a $(, $($generic),* )?> {
            pub fn new(loc: $crate::span::Span, $($field: $ty,)*) -> Self {
                Self {
                    _marker: std::marker::PhantomData,
                    $( $field, )*
                    loc,
                }
            }
        }
    };
}
#[macro_export]
macro_rules! choice {
    ($name:ident, { $( $(#[$doc:meta])* $member:ident $(($ty:ty))? ),* $(,)? }) => {
        #[derive(Debug, PartialEq, PartialOrd, Clone)]
        pub enum $name<'a> {
            $( $(#[$doc])* $member $( ( $ty ) )?, )*
            Marker(std::marker::PhantomData::<&'a str>)
        }

    };
}
