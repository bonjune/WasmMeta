use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, none_of, one_of},
    combinator::{map, opt, recognize},
    error::{Error, ErrorKind},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded},
    IResult, Parser,
};

const LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
const EXTENDED_LETTERS: &str =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./-# ";

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
            name: prod.head.name,
            lhs,
            rhs,
        };
        Ok((input, result))
    }
}

fn ws(input: &str) -> IResult<&str, ()> {
    let tex_spaces = alt((
        tag(r"\quad"),
        tag(r"\qquad"),
        tag(r"\ "),
        tag(r"\,"),
        tag(r"\:"),
        tag(r"\;"),
        tag(r"\!"),
        tag(r"&"),
        tag(r"~"),
    ));

    let (tail, _) = delimited(multispace0, many0(tex_spaces), multispace0)(input)?;
    Ok((tail, ()))
}

#[derive(Debug, PartialEq)]
struct Union<'a> {
    cases: Vec<Tuple<'a>>,
}

impl<'a> Union<'a> {
    fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, cases) = separated_list1(Self::or, Tuple::parser)(input)?;
        Ok((input, Self { cases }))
    }

    fn or(input: &str) -> IResult<&str, ()> {
        let (input, _) = tag("|")(input)?;
        Ok((input, ()))
    }
}

#[derive(Debug, PartialEq)]
struct Tuple<'a> {
    elems: Vec<Command<'a>>,
}

impl<'a> Tuple<'a> {
    fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, elems) = many1(Command::parser)(input)?;
        Ok((input, Self { elems }))
    }
}

fn record_pair(input: &str) -> IResult<&str, (Command, Command)> {
    let (input, first) = Command::parser(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, second) = Command::parser(input)?;

    Ok((input, (first, second)))
}

fn record(input: &str) -> IResult<&str, Vec<(Command, Command)>> {
    let (input, pairs) = delimited(tag(r"\{"), many1(record_pair), tag(r"\}"))(input)?;
    Ok((input, pairs))
}

fn vec(input: &str) -> IResult<&str, Command> {
    let (input, _) = tag("[")(input)?;
    let (input, vec) = Command::parser(input)?;
    if vec.head.name != "vec" {
        return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)));
    }
    let (input, cmd) = delimited(tag("("), Command::parser, tag(")"))(input)?;
    let (input, _) = tag("]")(input)?;

    Ok((input, cmd))
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    TCommand(Command<'a>),
    TWord(&'a str),
}

impl<'a> Token<'a> {
    fn parser(input: &'a str) -> IResult<&str, Self> {
        let (tail, token) = alt((
            map(Command::parser, Self::TCommand),
            map(tag("{"), Self::TWord),
            map(tag("}"), Self::TWord),
            Self::equal,
            Self::sequence_kind,
            Self::word,
            Self::non_letter,
        ))(input)?;
        let (tail, _) = ws(tail)?;

        Ok((tail, token))
    }

    fn equal(input: &'a str) -> IResult<&str, Self> {
        let (tail, s) = tag("::=")(input)?;
        let (tail, _) = ws(tail)?;
        Ok((tail, Self::TWord(s)))
    }

    fn word(input: &'a str) -> IResult<&str, Self> {
        let (tail, s) = recognize(many1(one_of(LETTERS)))(input)?;
        let (tail, _) = ws(tail)?;
        Ok((tail, Self::TWord(s)))
    }

    fn non_letter(input: &'a str) -> IResult<&str, Self> {
        let (tail, s) = recognize(many1(none_of(LETTERS)))(input)?;
        let (tail, _) = ws(tail)?;
        Ok((tail, Self::TWord(s)))
    }

    fn sequence_kind(input: &'a str) -> IResult<&str, Self> {
        let mut parser = alt((tag("^?"), tag("^n"), tag("^+"), tag("^?")));
        let (tail, k) = parser(input)?;
        Ok((tail, Self::TWord(k)))
    }
}

#[derive(Debug, PartialEq)]
pub struct Command<'a> {
    pub head: CommandHead<'a>,
    pub args: Vec<Argument<'a>>,
    pub upnote: Option<SeqKind>,
}

#[derive(Debug, PartialEq)]
pub struct CommandHead<'a> {
    pub name: &'a str,
    pub params: Vec<&'a str>,
}

#[derive(Debug, PartialEq)]
pub enum Argument<'a> {
    Str(&'a str),
    Cmd(Box<Command<'a>>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum SeqKind {
    OptSeq,
    ManyPossibleEmpty,
    ManyN,
    ManyNonEmpty,
}

impl SeqKind {
    fn parser(input: &str) -> IResult<&str, Option<Self>> {
        let mut parser = opt(alt((tag("^?"), tag("^n"), tag("^+"), tag("^*"))));
        let (tail, t) = parser(input)?;
        match t {
            Some(s) if s == "^?" => Ok((tail, Some(SeqKind::OptSeq))),
            Some(s) if s == "^n" => Ok((tail, Some(SeqKind::ManyN))),
            Some(s) if s == "^+" => Ok((tail, Some(SeqKind::ManyNonEmpty))),
            Some(s) if s == "^*" => Ok((tail, Some(SeqKind::ManyPossibleEmpty))),
            None => Ok((tail, None)),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum TexToken<'a> {
    Command(Command<'a>),
    Word(&'a str),
}

impl<'a> Command<'a> {
    fn new(head: CommandHead<'a>, args: Vec<Argument<'a>>, upnote: Option<SeqKind>) -> Self {
        Self { head, args, upnote }
    }

    fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, _) = ws(input)?;
        let (input, head) = CommandHead::parser(input)?;
        let (input, args) = many0(Argument::parser)(input)?;
        let (input, upnote) = SeqKind::parser(input)?;
        let (input, _) = ws(input)?;

        Ok((input, Self { head, args, upnote }))
    }
}

impl<'a> CommandHead<'a> {
    fn new(name: &'a str, params: Vec<&'a str>) -> Self {
        Self { name, params }
    }

    fn parser(input: &'a str) -> IResult<&str, Self> {
        let mut name_parser = preceded(tag("\\"), recognize(many1(one_of(LETTERS))));
        let (input, name) = name_parser(input)?;

        let mut params_parser = opt(delimited(
            char('['),
            separated_list1(char(','), recognize(many1(one_of(LETTERS)))),
            char(']'),
        ));
        let (input, params) = params_parser(input)?;
        let params = params.unwrap_or_default();

        // let params = match params {
        //     Some(ps) => ps.iter().map(|cs| cs.iter().collect()).collect(),
        //     None => vec![],
        // };

        Ok((input, Self { name, params }))
    }
}

impl<'a> Argument<'a> {
    fn parser(input: &'a str) -> IResult<&str, Self> {
        let arg_parser = alt((
            map(Command::parser, |out| Self::Cmd(Box::new(out))),
            Self::str_parser,
        ));
        delimited(char('{'), arg_parser, char('}'))(input)
    }

    fn str_parser_inner(input: &str) -> IResult<&str, &str> {
        let mut parser = alt((
            delimited(char('{'), Self::str_parser_inner, char('}')),
            recognize(many0(one_of(EXTENDED_LETTERS))),
        ));

        parser.parse(input)
    }

    fn str_parser(input: &'a str) -> IResult<&str, Self> {
        map(Self::str_parser_inner, Self::Str)(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Argument::*;
    use Token::*;

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
    fn test_ws() {
        ws("").expect("ws should skip empty");
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
