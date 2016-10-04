/*
Simple compiler.  We want to build trees and implement some basic
optimizations to get a sense for how Rust handles graph structures and
traversal and text generation.  The output should be x64 assembler.

Obvious optimization is register allocation.
*/


/*

prog ::= (var | fn)*
var ::= decl ";"
decl ::= type id
type ::= ("int" | "num")
fn ::= "fn" id "(" (decl ("," decl)*)? ")" ("->" type)? block
stmt ::= "if" "(" expr ")" stmt ("else" stmt)?
       | block
       | expr ";"
       | "while" "(" expr ")" stmt
       | "return" expr ";"
block ::= "{" (stmt | var)* "}"
expr ::= expr ("+" | "-" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | ">" | "<" | ">=" | "<=") expr
       | ("-" | "!") expr
       | id "=" expr
       | id "(" (expr ("," expr)*)? ")"
       | id
       | literal
       | "num" "(" expr ")"
       | "int" "(" expr ")"
       | "(" expr ")"
literal ::= intlit | floatlit

operator precedence high to low

! -              right
* / %            left
+ -              left
< <= > >=        left
== !=            left
&&               left
||               left
=                right

distinguished function 'main' without arguments.

ints double as booleans, but nums do not.

Scoping for globals is global.

Scoping for locals is from point-of-declaration to end of block.

Functions can be overloaded in argument lists.(??)

No auto coercion from int to num.

ints are i64, nums are f64.

Built-in functions:
  print(int)
  print(num)
  readi() -> int
  readn() -> num

*/

mod ast;
mod env;
mod err;
mod lex;
mod parse;
mod tycheck;

use lex::Lex;
use std::fs::File;

fn main() {
    let f = File::open("fib.pl0").unwrap();
    let prog = parse::parse(Lex::new(f)).unwrap();

    tycheck::check_program(&prog).unwrap();

    println!("{:?}", prog);
}
