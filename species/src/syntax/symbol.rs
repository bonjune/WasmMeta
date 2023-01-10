use std::fmt::Debug;

use nom::{
    branch::alt, bytes::complete::tag, character::complete::char, combinator::map,
    error::ErrorKind, multi::separated_list1, sequence::delimited, InputIter,
};

use crate::{
    nom_err,
    parser::{ws, Command, SeqKind},
    PResult,
};

#[derive(PartialEq)]
pub enum Symbol {
    STerm(String),
    SNonterm(SNonterm),
    SRecord(SRecord),
    SBracedVec(SBracedVec),
    SVec(SVec),
    SArrow(SArrow),
}

#[derive(Debug, PartialEq)]
struct STerm;

#[derive(Debug, PartialEq)]
pub struct SNonterm {
    name: String,
    seq_kind: Option<SeqKind>,
}

#[derive(PartialEq)]
pub struct SRecord {
    pairs: Vec<(String, Symbol)>,
}

impl Debug for SRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("SRecord").field(&self.pairs).finish()
    }
}

#[derive(PartialEq)]
pub struct SBracedVec {
    inner: SVec,
}

#[derive(Debug, PartialEq)]
pub struct SVec {
    over: Box<SNonterm>,
}

#[derive(Debug, PartialEq)]
pub struct SArrow {
    from: SNonterm,
    to: SNonterm,
}

impl Symbol {
    pub fn parser(input: &str) -> PResult<Self> {
        let mut parser = alt((
            map(SRecord::parser, Symbol::SRecord),
            map(SArrow::parser, Symbol::SArrow),
            map(SBracedVec::parser, Symbol::SBracedVec),
            map(SVec::parser, Symbol::SVec),
            map(SNonterm::parser, Symbol::SNonterm),
            map(STerm::parser, Symbol::STerm),
        ));

        parser(input)
    }
}

impl<'a> Command<'a> {
    fn is_terminal(&self) -> Option<&'a str> {
        if self.head.name == "K" {
            let name = self
                .args
                .first()
                .expect("command `K` must have a symbol name as an argument")
                .name();
            return Some(name);
        }

        if self.head.name.iter_elements().all(|c| {
            if c.is_ascii_alphabetic() {
                c.is_ascii_uppercase()
            } else {
                true
            }
        }) {
            return Some(self.head.name);
        }

        return None;
    }

    fn is_nonterminal(&self) -> Option<&'a str> {
        if self.head.name == "X" {
            let name = self
                .args
                .first()
                .expect("command `X` must have a symbol name as an argument")
                .name();
            return Some(name);
        }

        if self.head.name.iter_elements().all(|c| {
            if c.is_ascii_alphabetic() {
                c.is_ascii_lowercase()
            } else {
                true
            }
        }) {
            return Some(self.head.name);
        }

        return None;
    }
}

impl STerm {
    pub fn parser(input: &str) -> PResult<String> {
        let (input, cmd) = Command::parser(input)?;
        if let Some(name) = cmd.is_terminal() {
            Ok((input, name.to_string()))
        } else {
            nom_err!(input, ErrorKind::Tag)
        }
    }
}

impl SNonterm {
    pub fn parser(input: &str) -> PResult<Self> {
        let (tail, cmd) = Command::parser(input)?;
        if cmd.head.name == "end" || cmd.head.name == "production" {
            return nom_err!(input, ErrorKind::Tag);
        }
        if let Some(name) = cmd.is_nonterminal() {
            Ok((
                tail,
                Self {
                    name: name.to_string(),
                    seq_kind: cmd.upnote,
                },
            ))
        } else {
            nom_err!(input, ErrorKind::Tag)
        }
    }
}

impl SRecord {
    pub fn parser(input: &str) -> PResult<Self> {
        let (input, pairs) = delimited(
            tag(r"\{"),
            separated_list1(tag(","), Self::pair),
            tag(r"\}"),
        )(input)?;
        Ok((input, Self { pairs }))
    }

    fn pair(input: &str) -> PResult<(String, Symbol)> {
        let (input, _) = ws(input)?;
        let (input, key) = STerm::parser(input)?;

        let vec_parser = map(SVec::parser, Symbol::SVec);
        let nt_parser = map(SNonterm::parser, Symbol::SNonterm);
        let (input, value) = alt((vec_parser, nt_parser))(input)?;

        let (input, _) = ws(input)?;
        Ok((input, (key, value)))
    }
}

impl SBracedVec {
    pub fn parser(input: &str) -> PResult<Self> {
        let (input, _) = char('[')(input)?;
        let (input, inner) = SVec::parser(input)?;
        let (input, _) = char(']')(input)?;

        Ok((input, Self { inner }))
    }
}

impl SVec {
    pub fn parser(input: &str) -> PResult<Self> {
        let (input, vec) = Command::parser(input)?;
        if vec.head.name != "vec" {
            return nom_err!(input, ErrorKind::Tag);
        }
        let (input, nt) = delimited(tag("("), SNonterm::parser, tag(")"))(input)?;

        Ok((input, Self { over: Box::new(nt) }))
    }
}

impl SArrow {
    pub fn parser(input: &str) -> PResult<Self> {
        let (input, from) = SNonterm::parser(input)?;
        let (input, arrow) = Command::parser(input)?;
        if arrow.head.name != "to" {
            return nom_err!(input, ErrorKind::Tag);
        }
        let (input, to) = SNonterm::parser(input)?;

        Ok((input, Self { from, to }))
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::STerm(arg0) => f.debug_tuple("STerm").field(arg0).finish(),
            Self::SNonterm(arg0) => write!(f, "{:?}", arg0),
            Self::SRecord(arg0) => write!(f, "{:?}", arg0),
            Self::SBracedVec(arg0) => write!(f, "{:?}", arg0),
            Self::SVec(arg0) => write!(f, "{:?}", arg0),
            Self::SArrow(arg0) => write!(f, "{:?}", arg0),
        }
    }
}

impl Debug for SBracedVec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("SBracedVec").field(&self.inner.over).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_symbol {
        ($test_name: ident, $s: expr, $symbol: ident, $expected: expr) => {
            #[test]
            fn $test_name() {
                let (input, res) = $symbol::parser($s).unwrap();
                assert_eq!(input, "");
                assert_eq!(res, $expected);
            }
        };
    }

    test_symbol!(parse_terminal, r"\I32", STerm, "I32");

    test_symbol!(
        parse_nonterminal,
        r"\numtype",
        SNonterm,
        SNonterm {
            name: "numtype".to_string(),
            seq_kind: None
        }
    );

    test_symbol!(
        parse_record,
        r"\{ \LMIN~\u32, \LMAX~\u32^? \}",
        SRecord,
        SRecord {
            pairs: vec![
                (
                    "LMIN".to_string(),
                    Symbol::SNonterm(SNonterm {
                        name: "u32".to_string(),
                        seq_kind: None
                    })
                ),
                (
                    "LMAX".to_string(),
                    Symbol::SNonterm(SNonterm {
                        name: "u32".to_string(),
                        seq_kind: Some(SeqKind::OptSeq),
                    })
                ),
            ]
        }
    );
}
