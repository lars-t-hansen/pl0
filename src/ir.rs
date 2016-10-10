use std::cell::Cell;
use std::rc::Rc;

use names::Name;

/*
 * IR nodes are structs: { op, prev, next }, allocated from a pool.
 * The `op` is the operation in the node.  The `prev` and `next` are
 * indices in that pool, used to represent blocks (see below).  We
 * keep nodes as structs so that we can add data to nodes later
 * without having to update all the ops, which are tagged unions.
 * 
 * Functions are graphs of basic blocks.  A basic block is named by
 * its label, which holds an index of the first node in the block.
 * There are distinguished first and last blocks in each function.
 *
 * A basic block is a sequence (through the `next` field) of nodes
 * starting with a Block node and ending with a control transfer
 * (ReturnVal, ReturnVoid, Jcc, Jump).  Jcc has two outgoing edges,
 * one for taken, the other for untaken.  Jump has one, to the target
 * block.  Return has one, to the designated end block.  The edges
 * from are Label values.
 *
 * A value (`Val`) is represented as a reference to the node that
 * generated the value.  Thus this is like a label, but it can never
 * be unbound and is always intra-block, while labels are always
 * inter-block.
 */

#[derive(Debug)]
pub struct Program
{
    pub fns: Vec<Fun>
}

impl Program
{
    pub fn new() -> Program {
        Program {
            fns: Vec::new()
        }
    }

    pub fn add(&mut self, f:Fun) {
        self.fns.push(f);
    }
}

#[derive(Debug)]
pub struct Fun
{
    // Here, we probably want a map from local number to a Name, for
    // both int and num.  In this map, temps generated for whatever
    // purpose will appear as Name::T(k).
    pub entry_block: Label,
    pub exit_block: Label,
    pub ir: Vec<IR>
}

impl Fun {
    pub fn new(entry_block: Label, exit_block: Label, ir: Vec<IR>) -> Fun {
        Fun {
            entry_block: entry_block,
            exit_block: exit_block,
            ir: ir
        }
    }
}

#[derive(Debug)]
pub struct IR {
    pub op: Op,
    pub prev: Val,
    pub next: Val
}

pub const NULL : u32 = 911911;  // A `prev` or `next` link without a target (visually distinct)

pub type Val = u32;             // A Val is a reference to an IR node in the IR node heap

// Op nodes are probably fairly large, not sure how much of an issue
// that is.  Interesting to consider optimizing it.

#[derive(Debug)]
pub enum Op {
    Block(Option<BlockInfo>),
    Nop,
    Notreached,
    Literal(Lit),               // Has result
    Incoming(Arg),              // Has result
    Outgoing(Arg, Val),
    GetLocal(Local),            // Has result
    SetLocal(Local, Val),
    GetGlobal(Global),          // Has result
    SetGlobal(Global, Val),
    Op2(O2, Val, Val),          // Has result
    Op1(O1, Val),               // Has result
    Call(Global, Vec<Val>),     // Has optional result.  The Global indicates the return type.
    Jcc(OB, Val, Label, Label),
    Jump(Label),
    ReturnVal(Val, Label),
    ReturnVoid(Label)
}

#[derive(Debug)]
pub struct BlockInfo {
    // Will have predecessor nodes, probably
}

// A label is a clonable reference to an updatable container with
// information about an IR location.  The information is either
// the token UNBOUND or a nonnegative integer.

pub type Label = Rc<Cell<u32>>;

pub const UNBOUND : u32 = 0xFFFFFFFF;

pub fn label_bound(n:u32) -> Label {
    Rc::new(Cell::new(n))
}

pub fn label_unbound() -> Label {
    Rc::new(Cell::new(UNBOUND))
}

#[derive(Debug)]
pub enum Lit {
    I(i64),
    N(f64)
}

#[derive(Debug)]
pub enum Arg {
    I(u32),
    N(u32)
}

#[derive(Debug, Clone)]
pub enum Local {
    I(u32),
    N(u32)
}

#[derive(Debug, Clone)]
pub enum Global {
    I(Name),
    N(Name),
    V(Name)                 // Void
}

// The annotation I or N is the result type.  Some operators, like LessEq,
// are polymorphic; if so, then the type of the arguments is given by a
// second letter.

#[derive(Debug)]
pub enum O2 {
    AddI,
    AddN,
    SubI,
    SubN,
    MulI,
    MulN,
    DivI,
    DivN,
    ModI,
    ModN,
    LessII,
    LessIN,
    LessEqII,
    LessEqIN,
    EqII,
    EqIN,
    NotEqII,
    NotEqIN,
    GreaterEqII,
    GreaterEqIN,
    GreaterII,
    GreaterIN
}

#[derive(Debug)]
pub enum O1 {
    NotI,
    NegI,
    NegN,
    ToIntIN,
    ToNumNI
}

#[derive(Debug)]
pub enum OB {
    False,
    True,
}
