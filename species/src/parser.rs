use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt, recognize},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded},
    IResult, Parser,
};
use nom::character::complete::{char, multispace0, none_of, one_of, alphanumeric1};

const LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
const EXTENDED_LETTERS: &str =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./-# ";


pub struct Token;

impl Token {
    pub fn equal(input: &str) -> IResult<&str, ()> {
        let (tail, _s) = tag("::=")(input)?;
        let (tail, _) = Self::ws(tail)?;
        Ok((tail, ()))
    }
    
    fn word(input: &str) -> IResult<&str, &str> {
        let (tail, s) = alphanumeric1(input)?;
        let (tail, _) = Self::ws(tail)?;
        Ok((tail, s))
    }

    fn non_letter(input: &str) -> IResult<&str, &str> {
        let (tail, s) = recognize(many1(none_of(LETTERS)))(input)?;
        let (tail, _) = Self::ws(tail)?;
        Ok((tail, s))
    }

    pub fn ws(input: &str) -> IResult<&str, ()> {
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


impl<'a> Command<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, _) = Token::ws(input)?;
        let (input, head) = CommandHead::parser(input)?;
        let (input, args) = many0(Argument::parser)(input)?;
        let (input, upnote) = SeqKind::parser(input)?;
        let (input, _) = Token::ws(input)?;

        Ok((input, Self { head, args, upnote }))
    }
}

impl<'a> CommandHead<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let mut name_parser = preceded(tag("\\"), alphanumeric1);
        let (input, name) = name_parser(input)?;

        let mut params_parser = opt(delimited(
            char('['),
            separated_list1(char(','), alphanumeric1),
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
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
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

    pub fn name(&self) -> &'a str {
        match self {
            Argument::Str(s) => s,
            Argument::Cmd(cmd) => cmd.head.name,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn whitespaces() {
        Token::ws("").expect("ws should skip empty");
    }
}
