use nom::{
    bytes::complete::tag,
    error::{Error, ErrorKind},
    multi::{many1, separated_list1},
    sequence::delimited,
    IResult,
};

use crate::parser::{Command, Token, ws};

fn run(input: &str) -> Vec<Production> {
    let (_input, b) = MathBlock::parser(input).expect("parsing failed");
    b
}

#[derive(Debug, PartialEq)]
struct MathBlock<'a> {
    blocks: Vec<Production<'a>>,
}

impl<'a> MathBlock<'a> {
    fn parser(input: &str) -> IResult<&str, Vec<Production>> {
        let (input, _) = ws(input)?;
        // \begin{array}{...}
        let (input, _begin) = Command::parser(input)?;
        let (input, productions) = many1(Production::parser)(input)?;
        // \begin{end}{...}
        let (input, _end) = Command::parser(input)?;
        let (input, _) = ws(input)?;

        Ok((input, productions))
    }
}

#[derive(Debug, PartialEq)]
struct Production<'a> {
    name: &'a str,
    lhs: Command<'a>,
    rhs: Union<'a>,
}

impl<'a> Production<'a> {
    fn parser(input: &'a str) -> IResult<&str, Self> {
        // \\production{number type}
        let (input, prod) = Command::parser(input)?;
        // \numtype
        let (input, lhs) = Command::parser(input)?;
        // ::=
        let (input, _) = Token::equal(input)?;
        // \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64
        let (input, rhs) = Union::parser(input)?;
        // \\ to separate productions
        let (input, _) = tag(r"\\")(input)?;

        let result = Self {
            name: prod.args[0].name(),
            lhs,
            rhs,
        };
        Ok((input, result))
    }
}


#[derive(Debug, PartialEq)]
pub struct Union<'a> {
    pub cases: Vec<Tuple<'a>>,
}

/// ``` rust
/// use species::syntax::Union;
/// 
/// let s = r"\I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\";
/// let (_, union) = Union::parser(s).unwrap();
/// assert_eq!(union.cases.len(), 4);
/// ```
impl<'a> Union<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, cases) = separated_list1(Self::or, Tuple::parser)(input)?;
        Ok((input, Self { cases }))
    }

    fn or(input: &str) -> IResult<&str, ()> {
        let (input, _) = tag("|")(input)?;
        Ok((input, ()))
    }
}

#[derive(Debug, PartialEq)]
pub struct Tuple<'a> {
    pub elems: Vec<Command<'a>>,
}

impl<'a> Tuple<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, elems) = many1(Command::parser)(input)?;
        Ok((input, Self { elems }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Record<'a> {
    pub pairs: Vec<(Command<'a>, Command<'a>)>,
}

impl<'a> Record<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, pairs) = delimited(tag(r"\{"), many1(Self::pair), tag(r"\}"))(input)?;
        Ok((input, Self { pairs }))
    }

    fn pair(input: &str) -> IResult<&str, (Command, Command)> {
        let (input, first) = Command::parser(input)?;
        let (input, _) = tag(",")(input)?;
        let (input, second) = Command::parser(input)?;
    
        Ok((input, (first, second)))
    }
}

#[derive(Debug, PartialEq)]
pub struct SVec<'a> {
    pub over: Command<'a>
}

impl<'a> SVec<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, _) = tag("[")(input)?;
        let (input, vec) = Command::parser(input)?;
        if vec.head.name != "vec" {
            return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)));
        }
        let (input, cmd) = delimited(tag("("), Command::parser, tag(")"))(input)?;
        let (input, _) = tag("]")(input)?;
    
        Ok((input, Self { over: cmd }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn single_command() {
    //     let s = r"\begin{array}{llll}";
    //     let tokens = tokenize(s);
    //     assert_eq!(
    //         tokens,
    //         vec![TCommand(Command {
    //             head: CommandHead::new("begin", vec![]),
    //             args: vec![Str("array"), Str("llll")],
    //         },),]
    //     )
    // }

    #[test]
    fn test_union() {
        let s = r"\I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\";
        let tuples = Union::parser(s);
        println!("{:#?}", tuples)
    }

    #[test]
    fn number_type_block() {
        let s = r"\begin{array}{llll}
        \production{number type} &
        \numtype
        &::=&
        \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\
        \end{array}";
        let prods = run(s);
        println!("{:#?}", prods);
    }

    #[test]
    fn module_block() {
        let s = r"   \begin{array}{lllll}
        \production{module} & \module &::=& \{ &
          \MTYPES~\vec(\functype), \\&&&&
          \MFUNCS~\vec(\func), \\&&&&
          \MTABLES~\vec(\table), \\&&&&
          \MMEMS~\vec(\mem), \\&&&&
          \MGLOBALS~\vec(\global), \\&&&&
          \MELEMS~\vec(\elem), \\&&&&
          \MDATAS~\vec(\data), \\&&&&
          \MSTART~\start^?, \\&&&&
          \MIMPORTS~\vec(\import), \\&&&&
          \MEXPORTS~\vec(\export) \quad\} \\
        \end{array}";
        let prods = run(s);
        println!("{:#?}", prods);
    }

    #[test]
    fn many_productions() {
        let s = r"   \begin{array}{llll}
        \production{type index} & \typeidx &::=& \u32 \\
        \production{function index} & \funcidx &::=& \u32 \\
        \production{table index} & \tableidx &::=& \u32 \\
        \production{memory index} & \memidx &::=& \u32 \\
        \production{global index} & \globalidx &::=& \u32 \\
        \production{element index} & \elemidx &::=& \u32 \\
        \production{data index} & \dataidx &::=& \u32 \\
        \production{local index} & \localidx &::=& \u32 \\
        \production{label index} & \labelidx &::=& \u32 \\
        \end{array}";
        let prods = run(s);
        println!("{:#?}", prods);
    }

    // fn number_type_block_() {
    //     let s = r"   \begin{array}{llll}
    //     \production{number type} & \numtype &::=&
    //         \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\
    //     \end{array}";
    //     let tokens = tokenize(s);
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TCommand(Command {
    //                 head: CommandHead::new("begin", vec![]),
    //                 args: vec![Str("array"), Str("llll")],
    //             }),
    //             TCommand(Command {
    //                 head: CommandHead::new("production", vec![]),
    //                 args: vec![Str("number type")],
    //             }),
    //             TCommand(Command {
    //                 head: CommandHead::new("numtype", vec![]),
    //                 args: vec![],
    //             }),
    //             TWord("::="),
    //             TCommand(Command {
    //                 head: CommandHead::new("I32", vec![]),
    //                 args: vec![],
    //             }),
    //             TWord("|"),
    //             TCommand(Command {
    //                 head: CommandHead::new("I64", vec![]),
    //                 args: vec![],
    //             }),
    //             TWord("|"),
    //             TCommand(Command {
    //                 head: CommandHead::new("F32", vec![]),
    //                 args: vec![],
    //             }),
    //             TWord("|"),
    //             TCommand(Command {
    //                 head: CommandHead::new("F64", vec![]),
    //                 args: vec![],
    //             }),
    //             TCommand(Command {
    //                 head: CommandHead::new("end", vec![]),
    //                 args: vec![Str("array")],
    //             }),
    //         ]
    //     )
    // }
}
