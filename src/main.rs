/* My Second Rust Program */

/* A compiler for a toy language.  This builds trees and implements
 * some basic optimizations to get a sense for how Rust handles graph
 * structures and traversal and text generation.  The output is x64
 * assembler.  We use threads for concurrency when we can.
 */

/*

Syntax.

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


Semantics.

Distinguished function 'main' without arguments.

Ints double as booleans, but nums do not.

Scoping for globals is whole-program.

Scoping for parameters is from point-of-declaration to end of body.

Scoping for locals is from point-of-declaration to end of block.

Functions can be overloaded in argument lists. [Not implemented]

No auto coercion from int to num or vice versa.

Ints are i64, nums are f64.

Built-in functions (for now):
  printi(int)
  printn(num)
  readi() -> int
  readn() -> num

*/

mod ast;
mod env;
mod err;
mod lex;
mod names;
mod parse;
mod tycheck;

use lex::Lex;
use names::NameTable;
use std::fs::File;

fn main() {
    // TODO: get the program name from a command line argument.

    let f = File::open("fib.pl0").unwrap();
    let mut names = NameTable::new();
    let mut prog = parse::parse(Lex::new(&mut names, f)).unwrap();

    tycheck::check_program(&mut names, &mut prog).unwrap();

    println!("{:?}", prog);
}
