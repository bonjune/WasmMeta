use std::{fs, path::Path};

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

fn read_math_blocks(path: &Path) -> Vec<String> {
    let content = fs::read_to_string(path).unwrap();
    let mut blocks: Vec<String> = vec![];
    let mut math_block: Vec<&str> = vec![];
    let lines = content.lines();
    let mut in_block = false;
    for line in lines {
        if in_block {
            if line.len() != 0 {
                math_block.push(line);
            } else {
                let s = math_block.concat();
                blocks.push(s);
                math_block.clear();
                in_block = false;
            }
        }
        if line.starts_with(".. math::") {
            in_block = true;
        }
    }

    blocks
}
