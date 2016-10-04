use ast::*;
use env::{GlobalEnv, LocalEnv};

/*
 * Nodes are structs: { op, prev, next }, allocated from a pool.  Prev and next are
 * indices in that pool.  Values (`Val`) are indices of nodes in that pool.  We
 * keep these as structs so that we can add data to nodes later without having to
 * update all the ops, which are tagged unions.
 * 
 * Functions are graphs of basic blocks.  A basic block is a sequence of nodes starting
 * with a Block node and ending with a control transfer (Return, Jcc, Jump).  Jcc has
 * two outgoing edges, one for taken, the other for untaken.  Return has none.  Jump has
 * one.  The outgoing edges are indices of nodes that must all be Block nodes.
 *
 * Labels must be data with identity probably?
 */

//
// Operations are structs: { prev, next, op } where prev and next are references
// to operations and the op is an enum.
//
// Operations are named by integer indices into a vector that serves as a heap
// for all operations.  This doesn't feel great.  Maybe macros or helper functions
// will make it feel OK.
//
// Operations are collected in basic blocks.  No phi nodes are needed at this stage
// because we have updatable locals, not "values" for the locals.  This will change.

/*
Name(0) => fib

// Possibly better, types are implicit
// Here next and prev are not shown but are really there
// Block and EndBlock can carry more information

// Here we number intra-block instead of linearly but this is just notation

0/0: { op: Block(0) }
0/1: { op: GetLocal(Local::I(0)) }
0/2: { op: Literal(Lit::I(2)) }
0/3: { op: Op2(Opr2::GreaterEqI, 0/1, 0/2) }
0/4: { op: Jcc(0/3, 2/0, 1/0)

1/0: { op: Block(1) }
1/1: { op: GetLocal(Local::I(0)) }
1/2: { op: SetLocal(Local::I(1), 1/1) }
1/3: { op: Jump(3/0) }

2/0: { op: Block(2) }
2/1: { op: GetLocal(Local::I(0)) }
2/2: { op: Literal(Lit::I(1)) }
2/3: { op: Op2(Op2::SubI, 2/1, 2/2) }
2/4: { op: CallI(Global::I(0), vec![13]) }
2/5: { op: GetLocal(Local::I(0)) }
2/6: { op: Literal(Lit::I(2)) }
2/7: { op: Op2(Op2::SubI, 2/5, 2/6) }
2/8: { op: CallI(Global::I(0), vec![2/7]) }
2/9: { op: Op2(Op2::AddI, 2/4, 2/8) }
2/10:{ op: SetLocal(Local::I(1), 2/9) }
2/11:{ op: Jump(3/0) }

3/0: { op: Block(3) }
3/1: { op: GetLocal(Local::I(1)) }
3/2: { op: Return(3/1) }

*/

type Val = u32;                 // A Val is a ref to an IR node in the IR node heap

enum Op {
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

enum Label {
    L(u32)
}

enum Lit {
    I(i64),
    N(f64)
}

enum Arg {
    I(u32),
    N(u32)
}

enum Local {
    I(u32),
    N(u32)
}

enum Global {
    I(GlobName),
    N(GlobName),
    V(GlobName)                 // Void
}

// The annotation I or N is the result type.  Some operators, like LessEq,
// are polymorphic; if so, then the type of the arguments is given by a
// second letter.

enum O2 {
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
    LessI,
    LessIN,
    LessEqII,
    LessEqIN,
    EqII,
    EqIN,
    NeII,
    NeIN,
    GreaterEqII,
    GreaterEqIN,
    GreaterII,
    GreaterIN
}

enum O1 {
    NotI,
    NegI,
    NegN,
    ToIntIN,
    ToNumNI
}

enum OB {
    False,
    True,
}

struct IR { op: Op, prev_: Val, next_: Val }

struct BlockInfo {
    // Will have predecessor nodes, probably
}

// Env maps names to pseudo-registers
// Two types of registers, Int and Num
// For each function the context has next register number to use

fn typed_op(t:TypeName, iop:Op, nop:Op) -> Op {
    if t == TypeName::Int { iop } else { nop }
}

struct GenFun
{
    ints: u32,
    nums: u32,
    ir: Vec<IR>,
    scope: ... // Share this with tycheck maybe?
}

// Env maps name to local number within a function.  For globals,
// we just reuse the name.  So we need to distinguish the two kinds.

impl GenFun
{
    fn next_int(&mut self) -> u32 {
        let x = self.ints;
        self.ints += 1;
        return x;
    }

    fn next_num(&mut self) -> u32 {
        let x = self.nums;
        self.nums += 1;
        return x;
    }

    fn jcc(&mut self, cond:OB, v:Val, if_true:Label, if_false:Label) {
        self.add_instr(Op::Jcc(cond, v, if_true, if_false));
        self.new_block();
    }

    fn bind_label() {
        // we must be at the beginning of a block (ie self.last must be a Block op)
    }
    
    fn nop(&mut self) {
        self.add_instr(Op::Nop);
    }

    fn notreached(&mut self) {
        self.add_instr(Op::Notreached);
    }

    fn incoming(&mut self, arg: Arg) -> Val {
        return self.add_instr(Op::Incoming(Arg));
    }
    
    fn outgoing(&mut self, arg: Arg, v: Val) {
        self.add_instr(Op::Outgoing(Arg));
    }

    // Not obviously correct...  next and prev can be cells.

    fn op2(&mut self, op:O2, rs1:Val, rs2:Val) -> Val {
        let idx = self.ir.len();
        self.ir.push(IR { op: op, prev_: 0, next_: -1 });
        let node = &mut self.ir[idx];
        node.prev_ = self.last_;
        let last = &mut self.ir[self.last_];
        last.next_ = idx;
    }
           
    fn gen_function(&mut self, fn:&FunDefn) {
        let first_block = self.new_block();

        let mut k = 0;
        for vd in &fn.formals {
            match vd.ty {
                TypeName::Int => {
                    let idx = self.next_int();
                    self.push_local(&vd.name, idx);
                    let v = self.incoming(Arg::I(k));
                    self.setlocal(Local::I(idx), v);
                }
                TypeName::Num => {
                    let idx = self.next_num();
                    self.push_local(&vd.name, idx);
                    let v = self.incoming(Arg::N(k));
                    self.setlocal(Local::N(idx), v);
                }
            }
        }

        self.last_block = self.new_label();

        self.gen_stmt(&fn.body);

        if fn.ret == TypeName::Void {
            self.returnvoid(self.last_block, self.last_block);
        }

        self.bind_label(self.last_block);
    }

    fn gen_stmt(&mut self, stmt:&Stmt) {
        match stmt {
            &Stmt::Block(ref s) => self.gen_block(s),
            &Stmt::Expr(ref s) => self.gen_expr(&s.expr),
            &Stmt::If(ref s) => self.gen_if(s),
            &Stmt::Return(ref s) => self.gen_return(s),
            &Stmt::While(ref s) => self.gen_while(s),
            &Stmt::Var(ref s) => self.define_local(s)
        }
    }

    fn gen_block(&mut self, stmt:&Box<BlockStmt>) {
        self.push_scope();
        for s in &stmt.phrases {
            self.gen_stmt(&s);
        }
        self.pop_scope();
    }

    fn gen_if(&mut self, stmt: &Box<IfStmt>) {
        // FIXME: alternate is optional
        let false_stmt = self.new_label();
        let true_stmt = self.new_label();
        let done = self.new_label();
        let e = self.gen_expr(&stmt.test);
        self.jcc(OB::False, e, false_stmt, true_stmt);
        self.bind_label(true_stmt);
        self.gen_stmt(&self.consequent);
        self.jump(done);
        self.bind_label(false_stmt);
        self.gen_stmt(&self.alternate);
        self.bind_label(done);
    }

    fn gen_while(&mut self, stmt: &Box<WhileStmt>) {
        let body = self.new_label();
        let done = self.new_label();
        let again = self.new_label();
        self.bind_label(again);
        let e = self.gen_expr(&stmt.test);
        self.jcc(OB::False, e, done, body);
        self.bind_label(body);
        self.gen_stmt(&self.body);
        self.jump(again);
        self.bind_label(done);
    }

    fn gen_return(&self, stmt:&Box<ReturnStmt>) {
        match &stmt.expr {
            &Some(ref e) => {
                let val = self.gen_expr(e);
                self.returnval(val, self.last_block);
            }
            &None => {
                self.returnvoid(void, self.last_block)
            }
        }
    }

    fn gen_expr(&self, env:&Env, e:&Expr, out:&mut IR) -> Val {
        match *e {
            Expr::Unary(ref u) => {
                let e0 self.gen_expr(env, &u.expr);
                let t = u.ty.get();
                match u.op {
                    Unop::Negate => out.op1(typed_op(t, Op::NegI, Op::NegN), e0),
                    Unop::Not => out.op1(Op::NotI, e0),
                    Unop::ToInt => out.op1(Op::ToIntN, e0),
                    Unop::ToNum => out.op1(Op::ToNumI, e0)
                }
            }
            Expr::Binary(ref b) => {
                let el = self.gen_expr(env, &b.lhs);
                // We don't have phis yet, we must use a synthetic local to carry the
                // value through control flow.
                if b.op == Binop::And || b.op == Binop::Or {
                    let taken = out.new_label();
                    let not_taken = out.new_label();
                    let var = out.new_local();
                    out.set_local(var, el);
                    out.jcc(if b.op == Binop::And { OB::False } else { OB::True }, el, taken, not_taken);
                    out.bind_label(not_taken);
                    let er = self.gen_expr(env, &b.rhs);
                    out.set_local(var, er);
                    out.bind_label(taken);
                    out.get_local(var)
                }
                let er = self.gen_expr(env, &b.rhs);
                let t = b.ty.get();
                match b.op {
                    Binop::Add => out.op2(typed_op(t, O2::AddI, O2::AddN), el, er),
                    Binop::Subtract => out.op2(typed_op(t, O2::SubI, O2::SubN), el, er),
                    Binop::Multiply => out.op2(typed_op(t, O2::MulI, O2::MulN), el, er),
                    Binop::Divide => out.op2(typed_op(t, O2::DivI, O2::DivN), el, er),
                    Binop::Modulo => out.op2(typed_op(t, O2::ModI, O2::ModN), el, er),
                    Binop::Equal => out.op2(typed_op(t, O2::EqII, O2::EqIN), el, er),
                    Binop::NotEqual => out.op2(typed_op(t, O2::NotEqII, O2::NotEqIN), el, er),
                    Binop::Less => out.op2(typed_op(t, O2::LessII, O2::LessIN), el, er),
                    Binop::LessOrEqual => out.op2(typed_op(t, O2::LessEqII, O2::LessEqIN), el, er),
                    Binop::Greater => out.op2(typed_op(t, O2::GreaterII, O2::GreaterIN), el, er),
                    Binop::GreaterOrEqual => out.op2(typed_op(t, O2::GreaterEqII, O2::GreaterEqIN), el, er),
                    _ => unreachable!()
                }
            }
            Expr::Assign(ref a) => {
                let er = self.gen_expr(env, &b.rhs);
                let k = self.lookup_var(env, &a.name);
                if k == -1 {
                    out.set_global(a.name.clone(), er);
                } else {
                    out.set_local(k, er); // Oh really?  What about our phis?
                }
                return er;
            }
            Expr::Call(ref c) => {
                // Generate outgoing() nodes just before the call;
                // call just carries the number of arguments
                let sign = try!(self.lookup_fn(scope, &c.name));
                if sign.formals.len() != c.args.len() {
                    return Err(self.error("Wrong number of arguments"));
                }
                for k in 0..c.args.len() {
                    let ta = try!(self.check_expr(scope, &c.args[k]));
                    try!(self.check_same(ta, sign.formals[k]));
                }
                c.ty.set(sign.ret);
                Ok(sign.ret)
            }
            Expr::IntLit(i) => {
                out.literal(Lit::I(i))
            }
            Expr::NumLit(n) => {
                out.literal(Lit::N(n))
            }
            Expr::Id(ref v) => {
                let t = try!(self.lookup_var(scope, &v.name));
                v.ty.set(t);
                Ok(t)
            }
        }
    }
}
