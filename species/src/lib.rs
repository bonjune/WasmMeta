pub mod parser;
pub mod syntax;

#[macro_export]
macro_rules! nom_err {
    ($input: ident, $tag: expr) => {
        Err(nom::Err::Error(Error::new($input, $tag)))
    };
}