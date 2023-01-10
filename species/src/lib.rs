use nom::IResult;

pub mod parser;
pub mod syntax;

type PResult<'a, T> = IResult<&'a str, T>;

#[macro_export]
macro_rules! nom_err {
    ($input: ident, $tag: expr) => {
        Err(nom::Err::Error(nom::error::Error::new($input, $tag)))
    };
}