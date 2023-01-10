use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::opt,
    error::{Error, ErrorKind},
    multi::{many1, separated_list1},
    sequence::delimited,
    IResult, InputIter,
};

use crate::{
    nom_err,
    parser::{Command, SeqKind, ws, equal},
};

#[derive(Debug, PartialEq)]
pub struct MathBlock<'a> {
    productions: Vec<Production<'a>>,
}

impl<'a> MathBlock<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, _) = ws(input)?;
        let (input, _begin) = Symbol::begin(input)?;
        let (input, productions) = many1(Production::parser)(input)?;
        let (input, _end) = Symbol::end(input)?;
        let (input, _) = ws(input)?;

        Ok((input, MathBlock { productions }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Production<'a> {
    name: &'a str,
    lhs: Symbol<'a>,
    rhs: Rhs<'a>,
}

impl<'a> Production<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        // \\production{number type}
        let (input, name) = Self::production_name(input)?;
        // \numtype
        let (input, lhs) = Symbol::nonterm(input)?;
        // ::=
        let (input, _) = equal(input)?;
        // \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64
        let (input, rhs) = Rhs::parser(input)?;
        let (input, _) = ws(input)?;

        let result = Self { name, lhs, rhs };
        Ok((input, result))
    }

    /// Check if next is a production, but do not comsume
    pub fn is_production(source: &'a str) -> IResult<&str, ()> {
        let (_input, _prod) = Self::parser(source)?;
        Ok((source, ()))
    }

    fn production_name(input: &'a str) -> IResult<&str, &str> {
        let (input, cmd) = Command::parser(input)?;
        if cmd.head.name == "production" {
            Ok((input, cmd.args[0].name()))
        } else {
            nom_err!(input, ErrorKind::Tag)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Rhs<'a> {
    Union(Union<'a>),
    Record(Record<'a>),
}

impl<'a> Rhs<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, union) = opt(Union::parser)(input)?;
        if let Some(union) = union {
            let (input, _) = Token::ws(input)?;
            return Ok((input, Rhs::Union(union)));
        }
        let (input, record) = opt(Record::parser)(input)?;
        if let Some(record) = record {
            let (input, _) = ws(input)?;
            return Ok((input, Rhs::Record(record)));
        }

        return nom_err!(input, ErrorKind::Alt);
    }

    pub fn len(&self) -> usize {
        match self {
            Rhs::Union(union) => union.cases.len(),
            Rhs::Record(record) => record.pairs.len(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Union<'a> {
    pub cases: Vec<Tuple<'a>>,
}

impl<'a> Union<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, cases) = separated_list1(Self::or, Tuple::parser)(input)?;
        Ok((input, Self { cases }))
    }

    fn or(input: &str) -> IResult<&str, ()> {
        let (input, _) = tag("|")(input)?;
        let (input, _) = ws(input)?;
        Ok((input, ()))
    }
}

#[derive(Debug, PartialEq)]
pub struct Tuple<'a> {
    pub elems: Vec<Symbol<'a>>,
}

impl<'a> Tuple<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let term_or_nonterm = alt((Symbol::term, Symbol::nonterm));
        let (input, elems) = many1(term_or_nonterm)(input)?;
        Ok((input, Self { elems }))
    }
}

/// ```
/// use species::syntax::Record;
///
/// let s = r"\{ \LMIN~\u32, \LMAX~\u32^? \}";
/// let (input, record) = Record::parser(s).unwrap();
/// assert_eq!(input, "");
/// assert_eq!(record.pairs.len(), 2);
/// ```
#[derive(Debug, PartialEq)]
pub struct Record<'a> {
    pub pairs: Vec<(Symbol<'a>, Symbol<'a>)>,
}

impl<'a> Record<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, pairs) = delimited(
            tag(r"\{"),
            separated_list1(tag(","), Self::pair),
            tag(r"\}"),
        )(input)?;
        Ok((input, Self { pairs }))
    }

    fn pair(input: &str) -> IResult<&str, (Symbol, Symbol)> {
        let (input, _) = ws(input)?;
        let (input, first) = Symbol::term(input)?;
        let (input, second) = alt((Symbol::vec, Symbol::nonterm))(input)?;
        let (input, _) = ws(input)?;

        Ok((input, (first, second)))
    }
}

#[derive(Debug, PartialEq)]
pub struct SVec<'a> {
    pub over: Command<'a>,
}

impl<'a> SVec<'a> {
    pub fn parser(input: &'a str) -> IResult<&str, Self> {
        let (input, _) = tag("[")(input)?;
        let (input, vec) = Command::parser(input)?;
        if vec.head.name != "vec" {
            return nom_err!(input, ErrorKind::Tag);
        }
        let (input, cmd) = delimited(tag("("), Command::parser, tag(")"))(input)?;
        let (input, _) = tag("]")(input)?;

        Ok((input, Self { over: cmd }))
    }
}

/// ``` rust
/// use species::syntax::Symbol;
///
/// let (input, end) = Symbol::end(r"\end{array}").unwrap();
/// assert_eq!(input, "");
/// assert_eq!(end, Symbol::End);
/// ```
#[derive(Debug, PartialEq)]
pub enum Symbol<'a> {
    Begin,
    End,
    Term(&'a str, Option<SeqKind>),
    Nonterm(&'a str, Option<SeqKind>),
    Vec(Box<Symbol<'a>>),
    Series(Vec<Symbol<'a>>),
}

impl<'a> Symbol<'a> {
    pub fn end(input: &'a str) -> IResult<&str, Self> {
        let (input, cmd) = Command::parser(input)?;
        if cmd.head.name == "end" {
            Ok((input, Self::End))
        } else {
            nom_err!(input, ErrorKind::Tag)
        }
    }

    pub fn begin(input: &'a str) -> IResult<&str, Self> {
        let (input, cmd) = Command::parser(input)?;
        if cmd.head.name == "begin" {
            Ok((input, Self::End))
        } else {
            nom_err!(input, ErrorKind::Tag)
        }
    }

    pub fn term(input: &'a str) -> IResult<&str, Self> {
        let (input, cmd) = Command::parser(input)?;
        if cmd.head.name == "K" {
            let name = cmd
                .args
                .first()
                .expect("command `K` must have a symbol name as an argument")
                .name();
            return Ok((input, Self::Term(name, cmd.upnote)));
        }

        if cmd.head.name.iter_elements().all(|c| {
            if c.is_ascii_alphabetic() {
                c.is_ascii_uppercase()
            } else {
                true
            }
        }) {
            return Ok((input, Self::Term(cmd.head.name, cmd.upnote)));
        }

        return nom_err!(input, ErrorKind::Tag);
    }

    pub fn nonterm(input: &'a str) -> IResult<&str, Self> {
        let (tail, cmd) = Command::parser(input)?;
        if cmd.head.name == "end" || cmd.head.name == "production" {
            return nom_err!(input, ErrorKind::Tag);
        }
        if cmd.head.name == "X" {
            let name = cmd
                .args
                .first()
                .expect("command `X` must have a symbol name as an argument")
                .name();
            return Ok((tail, Self::Nonterm(name, cmd.upnote)));
        }

        if cmd.head.name.iter_elements().all(|c| {
            if c.is_ascii_alphabetic() {
                c.is_ascii_lowercase()
            } else {
                true
            }
        }) {
            return Ok((tail, Self::Nonterm(cmd.head.name, cmd.upnote)));
        }

        return nom_err!(tail, ErrorKind::Tag);
    }

    pub fn vec(input: &'a str) -> IResult<&str, Self> {
        let (input, vec) = Command::parser(input)?;
        if vec.head.name != "vec" {
            return nom_err!(input, ErrorKind::Tag);
        }
        let (input, nt) = delimited(tag("("), Self::nonterm, tag(")"))(input)?;

        Ok((input, Self::Vec(Box::new(nt))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_union() {
        let s = r"\I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\";
        let (input, union) = Union::parser(s).unwrap();
        assert_eq!(input, "");
        assert_eq!(union.cases.len(), 4);
    }

    #[test]
    fn test_pair() {
        let s = r"\{ \LMIN~\u32, \LMAX~\u32^? \}";
        let (input, record) = Record::parser(s).unwrap();
        let pairs = record.pairs;
        assert_eq!(input, "");
        assert_eq!(pairs.len(), 2);
        let symbol = &pairs.last().unwrap().1;
        if let Symbol::Nonterm(name, sk) = symbol {
            assert_eq!(name.to_string(), "u32");
            assert_eq!(*sk, Some(SeqKind::OptSeq))
        } else {
            unreachable!()
        }
    }

    #[test]
    fn number_type_block() {
        let s = r"\begin{array}{llll}
        \production{number type} &
        \numtype
        &::=&
        \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\
        \end{array}";
        let (input, mb) = MathBlock::parser(s).unwrap();
        assert_eq!(input, "");
        assert_eq!(mb.productions.len(), 1);
        let prod = mb.productions.first().unwrap();
        match &prod.rhs {
            Rhs::Union(union) => assert_eq!(union.cases.len(), 4),
            Rhs::Record(_) => unreachable!(),
        }
    }

    #[test]
    fn limits_block() {
        let s = r"   \begin{array}{llll}
        \production{limits} & \limits &::=&
          \{ \LMIN~\u32, \LMAX~\u32^? \} \\
        \end{array}";
        let (input, mb) = MathBlock::parser(s).unwrap();
        assert_eq!(input, "");
        assert_eq!(mb.productions.len(), 1);
        let prod = mb.productions.first().unwrap();
        match &prod.rhs {
            Rhs::Union(_) => unreachable!(),
            Rhs::Record(rec) => assert_eq!(rec.pairs.len(), 2),
        }
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
        let (input, mb) = MathBlock::parser(s).unwrap();
        let prods = mb.productions;
        assert_eq!(prods.len(), 1);
        assert_eq!(prods[0].rhs.len(), 10);
    }

    #[test]
    fn indices_block() {
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
        let (input, mb) = MathBlock::parser(s).unwrap();
        assert_eq!(mb.productions.len(), 9);
    }

    #[test]
    fn table_instructions() {
        let s = r"   \begin{array}{llcl}
        \production{instruction} & \instr &::=&
          \dots \\&&|&
          \TABLEGET~\tableidx \\&&|&
          \TABLESET~\tableidx \\&&|&
          \TABLESIZE~\tableidx \\&&|&
          \TABLEGROW~\tableidx \\&&|&
          \TABLEFILL~\tableidx \\&&|&
          \TABLECOPY~\tableidx~\tableidx \\&&|&
          \TABLEINIT~\tableidx~\elemidx \\&&|&
          \ELEMDROP~\elemidx \\
        \end{array}";
        let (input, mb) = MathBlock::parser(s).unwrap();
        let prods = mb.productions; 
        assert_eq!(prods.len(), 1);
        assert_eq!(prods[0].rhs.len(), 9);
        assert_eq!(input, "");
    }
}
