/* My Second Rust Program */

/* A compiler for a toy language.  This builds trees and implements
 * some basic optimizations to get a sense for how Rust handles graph
 * structures and traversal and text generation.  The output should 
 * perhaps be wasm, suitable for a web browser (but in that case,
 * register allocation and saving of live values across calls are
 * trivial, so it's an oversimplification).
 *
 * We use threads for concurrency in the compiler when we can.
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

operator precedence and associativity high to low

! -              right
* / %            left
+ -              left
< <= > >=        left
== !=            left
&&               left
||               left
=                right


Semantics.

- Distinguished function 'main' without arguments.
- Ints double as booleans, but nums do not.
- Scoping for globals is whole-program.
- Scoping for parameters is from point-of-declaration to end of body.
- Scoping for locals is from point-of-declaration to end of block.
- Functions can be overloaded in argument lists. [Not implemented]
- No auto coercion from int to num or vice versa.
- Ints are i64, nums are f64.


Library.

Built-in functions (for now):

  printi(int)
  printn(num)
  readi() -> int
  readn() -> num


Future.

We should add read and write statements:

stmt ::= ...
       | write (stringlit | expr | ",")+ ";"
       | read id ("," id)* ";"

where a comma in the write format emits a space, a la awk, but commas
are optional.


We should add simple one-dimensional arrays.


We should add some implicit coercions, maybe int->num, since they
will (usually) require updating the ast during type checking by
inserting coercion operations.  Or something.


We should have a notion of "pub" functions that are exported to
some ecosystem (in the wasm case).  In this case, "main" is
merely an initialization function, and other pub functions can be
called after main has returned.


Global variables should be initializable with constant expressions,
and local variables with arbitrary expressions.
*/

mod ast;
mod env;
mod err;
mod ir;
mod irgen;
mod lex;
mod names;
mod parse;
mod tycheck;

use lex::Lex;
use names::NameTable;

use std::fs::File;

// TODO: The type checker forks off threads to check all functions,
// but then it waits for all those to complete before it goes on to IR
// generation.  This is not ideal, because IR generation can also be
// parallelized, as can per-function optimization and code generation.
// (Inlining is somewhat dependent on type checking completing first
// but might not require a centralized "we are done" point.)
//
// We might split this up so that type checking is exposed in two
// levels, global and per-function, and then we could fork off threads
// here to do type checking, ir generation, optimization, and code
// generation per function.  We could also manage worker pools etc
// here.
//
// If we do that, though, an error in one type checking thread should
// cause other threads to stop computing fairly quickly (eg, after
// type checking or asap thereafter).

fn main() {
    // TODO: get the program name from a command line argument.

    let f = File::open("fib.pl0").unwrap();
    let mut names = NameTable::new();
    let mut prog = parse::parse(Lex::new(&mut names, f)).unwrap();

    let mut env = tycheck::new_env();
    tycheck::check_program(&mut names, &mut prog, &mut env).unwrap();
    tycheck::check_functions(&mut prog, &env).unwrap();

    println!("{:?}", prog);

    let ir = irgen::program(&mut prog);

    println!("{:?}", ir);
}
