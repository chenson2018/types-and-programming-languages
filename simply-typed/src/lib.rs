#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(let_chains)]

pub mod builtins;
pub mod error;
pub mod parser;
pub mod scanner;
pub mod term;
pub mod types;

#[macro_use]
extern crate lazy_static;
