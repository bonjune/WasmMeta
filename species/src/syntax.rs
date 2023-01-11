pub mod symbol;

use std::fmt::Debug;

use nom::{
    bytes::complete::tag,
    error::ErrorKind,
    multi::{many1, many_till, separated_list1},
};

use crate::{
    nom_err,
    parser::{equal, ws, Command},
    syntax::symbol::Symbol,
    PResult,
};

#[derive(Debug, PartialEq)]
pub struct MathBlock<'a> {
    productions: Vec<Production<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Production<'a> {
    name: &'a str,
    lhs: Lhs,
    rhs: Rhs,
}

#[derive(PartialEq)]
pub struct Lhs {
    names: Vec<String>,
}

#[derive(PartialEq)]
pub struct Rhs {
    elems: Vec<RhsElem>,
}

#[derive(PartialEq)]
struct RhsElem {
    symbols: Vec<Symbol>,
    cond: Option<()>,
}

impl<'a> Production<'a> {
    pub fn parser(input: &'a str) -> PResult<Self> {
        let (input, name) = Self::production_name(input)?;
        let (input, lhs) = Lhs::parser(input)?;
        let (input, rhs) = Rhs::parser(input)?;
        let (input, _) = ws(input)?;

        let result = Self { name, lhs, rhs };
        Ok((input, result))
    }

    /// Check if next is a production, but do not comsume
    pub fn is_production(source: &'a str) -> PResult<()> {
        let (_input, _prod) = Self::parser(source)?;
        Ok((source, ()))
    }

    fn production_name(input: &'a str) -> PResult<&str> {
        let (input, cmd) = Command::parser(input)?;
        if cmd.head.name == "production" {
            Ok((input, cmd.args[0].name()))
        } else {
            nom_err!(input, ErrorKind::Tag)
        }
    }
}

impl Lhs {
    fn parser(input: &str) -> PResult<Self> {
        // let (input, cmd) = Command::parser(input)?;
        let (input, (names, _)) = many_till(Command::parser, equal)(input)?;
        let names = names.iter().map(|n| n.head.name.to_string()).collect();
        let lhs = Self { names };
        Ok((input, lhs))
    }
}

impl<'a> MathBlock<'a> {
    pub fn parser(input: &'a str) -> PResult<Self> {
        let (input, _) = ws(input)?;
        let (input, _begin) = begin(input)?;
        let (input, productions) = many1(Production::parser)(input)?;
        let (input, _end) = end(input)?;
        let (input, _) = ws(input)?;

        Ok((
            input,
            MathBlock {
                productions: dbg!(productions),
            },
        ))
    }
}

impl Rhs {
    pub fn parser(input: &str) -> PResult<Self> {
        let (input, elems) = separated_list1(or, RhsElem::parser)(input)?;
        Ok((input, Self { elems }))
    }
}

impl RhsElem {
    pub fn parser(input: &str) -> PResult<Self> {
        let (input, symbols) = many1(Symbol::parser)(input)?;
        Ok((
            input,
            Self {
                symbols,
                cond: None,
            },
        ))
    }
}

pub fn begin(input: &str) -> PResult<()> {
    let (input, cmd) = Command::parser(input)?;
    if cmd.head.name == "begin" {
        Ok((input, ()))
    } else {
        nom_err!(input, ErrorKind::Tag)
    }
}

pub fn end(input: &str) -> PResult<()> {
    let (input, cmd) = Command::parser(input)?;
    if cmd.head.name == "end" {
        Ok((input, ()))
    } else {
        nom_err!(input, ErrorKind::Tag)
    }
}

pub fn or(input: &str) -> PResult<()> {
    let (input, _) = tag("|")(input)?;
    let (input, _) = ws(input)?;
    Ok((input, ()))
}

impl Debug for Lhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Lhs").field(&self.names).finish()
    }
}

impl Debug for Rhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.elems)
    }
}

impl Debug for RhsElem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RhsElem")
            .field(&self.symbols)
            .field(&self.cond)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use std::env;

    use crate::read_math_blocks;

    use super::*;

    #[test]
    fn test_all_math_blocks() {
        let path = env::var("WASMMETA_PATH").expect("Environment Variable `WASMMETA_PATH` is not set");
        let blocks = read_math_blocks(path + "/resources/spec/document/core/syntax/types.rst");
        for block in blocks {
            let (input, mb) = MathBlock::parser(&block).unwrap();
            assert_eq!(input, "");
            assert!(mb.productions.len() > 0);
        }
    }

    macro_rules! test_block {
        ($prod_name:ident, $s:expr, $prod_num:expr) => {
            #[test]
            fn $prod_name() {
                let (input, mb) = MathBlock::parser($s).unwrap();
                let prods = mb.productions;
                assert_eq!(input, "");
                assert_eq!(prods.len(), $prod_num);
            }
        };
    }

    test_block!(
        parse_number_type_block,
        r"\begin{array}{llll}
        \production{number type} &
        \numtype
        &::=&
        \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\
        \end{array}",
        1
    );

    test_block!(
        parse_vector_type_block,
        r"   \begin{array}{llll}
    \production{vector type} & \vectype &::=&
      \V128 \\
    \end{array}",
        1
    );

    test_block!(
        parse_reference_type_block,
        r"   \begin{array}{llll}
    \production{reference type} & \reftype &::=&
      \FUNCREF ~|~ \EXTERNREF \\
    \end{array}",
        1
    );

    test_block!(
        parse_value_type_block,
        r"   \begin{array}{llll}
    \production{value type} & \valtype &::=&
      \numtype ~|~ \vectype ~|~ \reftype \\
    \end{array}",
        1
    );

    test_block!(
        parse_result_type,
        r"\begin{array}{llll}
    \production{result type} & \resulttype &::=&
      [\vec(\valtype)] \\
    \end{array}",
        1
    );

    test_block!(
        parse_function_type_block,
        r"   \begin{array}{llll}
    \production{function type} & \functype &::=&
      \resulttype \to \resulttype \\
    \end{array}",
        1
    );

    test_block!(
        parse_limits_block,
        r"\begin{array}{llll}
        \production{limits} & \limits &::=&
          \{ \LMIN~\u32, \LMAX~\u32^? \} \\
        \end{array}",
        1
    );

    test_block!(
        parse_memory_type_block,
        r"   \begin{array}{llll}
    \production{memory type} & \memtype &::=&
      \limits \\
    \end{array}",
        1
    );

    test_block!(
        parse_table_type_block,
        r"   \begin{array}{llll}
    \production{table type} & \tabletype &::=&
      \limits~\reftype \\
    \end{array}",
        1
    );

    test_block!(
        parse_global_type,
        r"\begin{array}{llll}
    \production{global type} & \globaltype &::=&
      \mut~\valtype \\
    \production{mutability} & \mut &::=&
      \MCONST ~|~
      \MVAR \\
    \end{array}",
        2
    );

    test_block!(
        parse_external_types,
        r"\begin{array}{llll}
    \production{external types} & \externtype &::=&
      \ETFUNC~\functype ~|~
      \ETTABLE~\tabletype ~|~
      \ETMEM~\memtype ~|~
      \ETGLOBAL~\globaltype \\
    \end{array}",
        1
    );

    #[test]
    fn parse_rhs() {
        let s = r"\I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\";
        let (input, rhs) = Rhs::parser(s).unwrap();
        assert_eq!(input, "");
        assert_eq!(rhs.elems.len(), 4);
    }

    test_block!(
        parse_module_block,
        r"   \begin{array}{lllll}
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
        \end{array}",
        1
    );

    test_block!(
        parse_indicies_block,
        r"   \begin{array}{llll}
        \production{type index} & \typeidx &::=& \u32 \\
        \production{function index} & \funcidx &::=& \u32 \\
        \production{table index} & \tableidx &::=& \u32 \\
        \production{memory index} & \memidx &::=& \u32 \\
        \production{global index} & \globalidx &::=& \u32 \\
        \production{element index} & \elemidx &::=& \u32 \\
        \production{data index} & \dataidx &::=& \u32 \\
        \production{local index} & \localidx &::=& \u32 \\
        \production{label index} & \labelidx &::=& \u32 \\
        \end{array}",
        9
    );

    test_block!(
        parse_table_insts_block,
        r"   \begin{array}{llcl}
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
        \end{array}",
        1
    );
}
