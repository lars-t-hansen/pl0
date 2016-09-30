// Type checking:
//
// Per-function:
//
// Since vars and functions at the global level are statically
// typed (no type computation is needed after parsing) we can type
// check functions in parallel eg using a queue of functions,
// once the entire program has been parsed.

use ast::*;

#[derive(Debug)]
pub struct TypeErr
{
    pub msg: String
}

pub fn tycheck(p:&Program) -> Result<(), TypeErr> {
    Typecheck::new().program(p)
}

struct Typecheck
{
    globals: HashMap<String, GlobalDefn>
}

enum GlobalDefn
{
    Fn,
    Var
}

impl Typecheck
{
    fn new() -> Typecheck {
        Typecheck {
            globals: HashMap::<String, GlobalDefn>::new()
        }
    }
    
    fn program(&mut self, p:&Program) -> Result<(), TypeErr> {
        // for each global, check for it in table, error if there
        // otherwise create a GlobalDefn for it and insert it into the hash table

        // Then for each fn, check the function
    }

    fn function(&mut self, f:&FunDefn) -> Result<(), TypeErr> {
        // Check that parameters have distrinct names
        
        // For each expression, compute types
        // - no duplicate parameter names
        // - returned values match return types
        // - tests for while and if must be integer types
        // - compute types for expressions
        // - calls must match formals in the fn
        // - scopes of values
        // - specialize operators in node or at IR generation?
    }
}
