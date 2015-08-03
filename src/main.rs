//! muun
//! Lua like language for Experiment and Hobby

#[macro_use]
extern crate log;

#[macro_use]
mod lex;
#[macro_use]
mod parse;
mod vm;

fn main() {
    println!("Hello, world!");
}
