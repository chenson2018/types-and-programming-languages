#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(let_chains)]

#[macro_use]
extern crate lazy_static;

pub mod church;
pub mod parser;
pub mod scanner;
pub mod term;
