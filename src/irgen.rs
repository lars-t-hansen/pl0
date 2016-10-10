use std::cell::RefCell;
use std::rc::Rc;

use ast::*;
use env::{GlobalEnv, LocalEnv};
use names::{NameTable, Name};
use ir;
use ir::{Label, Op, O1, O2, OB, Val, Arg, IR, Local, Global, Lit};

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

pub fn program(program:&mut Program) -> ir::Program {
    let mut irp = ir::Program::new();
    for f in &program.fns {
        let mut fg = FunGen::new();
        fg.gen_function(&f);
        irp.add(ir::Fun::new(fg.first_block, fg.last_block, fg.ir));
    }
    return irp;
}

fn typed_o2(t:TypeName, iop:O2, nop:O2) -> O2 {
    if t == TypeName::INT { iop } else { nop }
}

fn typed_o1(t:TypeName, iop:O1, nop:O1) -> O1 {
    if t == TypeName::INT { iop } else { nop }
}

struct FunGen
{
    ints: u32,
    nums: u32,
    temp_ints: u32,
    ir: Vec<IR>,
    last: usize,
    first_block: ir::Label,
    last_block: ir::Label,
    locals: LocalEnv<Local>
}

impl FunGen
{
    fn new() -> FunGen {
        FunGen {
            ints: 0,
            nums: 0,
            temp_ints: 0,
            ir: Vec::new(),
            last: 0,
            first_block: ir::label_unbound(),
            last_block: ir::label_unbound(),
            locals: LocalEnv::new()
        }
    }
    
    fn gen_function(&mut self, fun:&FunDefn) {
        self.init_ir();

        let first_block = self.first_block.clone();
        let last_block = self.last_block.clone();

        self.new_block();
        self.bind_label(first_block);

        let mut k = 0;
        for vd in &fun.formals {
            match vd.ty {
                TypeName::INT => {
                    let local = self.new_local_int(&vd.name);
                    let v = self.incoming(Arg::I(k));
                    self.setlocal(local, v);
                }
                TypeName::NUM => {
                    let local = self.new_local_num(&vd.name);
                    let v = self.incoming(Arg::N(k));
                    self.setlocal(local, v);
                }
                _ => unreachable!()
            }
            k += 1;
        }

        self.gen_stmt(&fun.body);

        if fun.ret == TypeName::VOID {
            self.returnvoid(last_block.clone());
        }

        self.bind_label(last_block.clone());
        self.jump(last_block);
    }

    fn gen_stmt(&mut self, stmt:&Stmt) {
        match stmt {
            &Stmt::Block(ref s) => self.gen_block(s),
            &Stmt::Expr(ref s) => { self.gen_expr(&s.expr); }
            &Stmt::If(ref s) => self.gen_if(s),
            &Stmt::Return(ref s) => self.gen_return(s),
            &Stmt::While(ref s) => self.gen_while(s),
            &Stmt::Var(ref s) => self.gen_vardefn(s)
        }
    }

    fn gen_block(&mut self, stmt:&Box<BlockStmt>) {
        self.locals.push();
        for s in &stmt.phrases {
            self.gen_stmt(&s);
        }
        self.locals.pop();
    }

    fn gen_if(&mut self, stmt: &Box<IfStmt>) {
        let true_stmt = self.new_label();
        let done = self.new_label();
        let e = self.gen_expr(&stmt.test);
        match stmt.alternate {
            Some(ref alternate) => {
                let false_stmt = self.new_label();
                self.jcc(OB::False, e, false_stmt.clone(), true_stmt.clone());
                self.bind_label(true_stmt);
                self.gen_stmt(&stmt.consequent);
                self.jump(done.clone());
                self.bind_label(false_stmt);
                self.gen_stmt(alternate);
            }
            None => {
                self.jcc(OB::False, e, done.clone(), true_stmt.clone());
                self.bind_label(true_stmt);
                self.gen_stmt(&stmt.consequent);
            }
        }
        self.bind_label(done);
    }

    fn gen_while(&mut self, stmt: &Box<WhileStmt>) {
        let body = self.new_label();
        let done = self.new_label();
        let again = self.new_label();
        self.bind_label(again.clone());
        let e = self.gen_expr(&stmt.test);
        self.jcc(OB::False, e, done.clone(), body.clone());
        self.bind_label(body);
        self.gen_stmt(&stmt.body);
        self.jump(again);
        self.bind_label(done);
    }

    fn gen_return(&mut self, stmt:&Box<ReturnStmt>) {
        let last_block = self.last_block.clone();
        match &stmt.expr {
            &Some(ref e) => {
                let val = self.gen_expr(e);
                self.returnval(val, last_block);
            }
            &None => {
                self.returnvoid(last_block)
            }
        }
    }

    fn gen_vardefn(&mut self, stmt:&Box<VarDefnStmt>) {
        // FIXME
    }
    
    fn gen_expr(&mut self, e:&Expr) -> Val {
        match *e {
            Expr::Unary(ref u) => {
                let e0 = self.gen_expr(&u.expr);
                let t = u.ty.get();
                match u.op {
                    Unop::Negate => self.op1(typed_o1(t, O1::NegI, O1::NegN), e0),
                    Unop::Not => self.op1(O1::NotI, e0),
                    Unop::ToInt => self.op1(O1::ToIntIN, e0),
                    Unop::ToNum => self.op1(O1::ToNumNI, e0)
                }
            }
            Expr::Binary(ref b) => {
                let el = self.gen_expr(&b.lhs);
                if b.op == Binop::And || b.op == Binop::Or {
                    let taken = self.new_label();
                    let not_taken = self.new_label();
                    let var = self.new_temp_int();
                    self.setlocal(var, el);
                    self.jcc(if b.op == Binop::And { OB::False } else { OB::True }, el, taken.clone(), not_taken.clone());
                    self.bind_label(not_taken);
                    let er = self.gen_expr(&b.rhs);
                    self.setlocal(var, er);
                    self.bind_label(taken);
                    return self.getlocal(var);
                }
                let er = self.gen_expr(&b.rhs);
                let t = b.ty.get();
                match b.op {
                    Binop::Add => self.op2(typed_o2(t, O2::AddI, O2::AddN), el, er),
                    Binop::Subtract => self.op2(typed_o2(t, O2::SubI, O2::SubN), el, er),
                    Binop::Multiply => self.op2(typed_o2(t, O2::MulI, O2::MulN), el, er),
                    Binop::Divide => self.op2(typed_o2(t, O2::DivI, O2::DivN), el, er),
                    Binop::Modulo => self.op2(typed_o2(t, O2::ModI, O2::ModN), el, er),
                    Binop::Equal => self.op2(typed_o2(t, O2::EqII, O2::EqIN), el, er),
                    Binop::NotEqual => self.op2(typed_o2(t, O2::NotEqII, O2::NotEqIN), el, er),
                    Binop::Less => self.op2(typed_o2(t, O2::LessII, O2::LessIN), el, er),
                    Binop::LessOrEqual => self.op2(typed_o2(t, O2::LessEqII, O2::LessEqIN), el, er),
                    Binop::Greater => self.op2(typed_o2(t, O2::GreaterII, O2::GreaterIN), el, er),
                    Binop::GreaterOrEqual => self.op2(typed_o2(t, O2::GreaterEqII, O2::GreaterEqIN), el, er),
                    _ => unreachable!()
                }
            }
            Expr::Assign(ref a) => {
                let er = self.gen_expr(&a.expr);
                match self.locals.lookup(&a.name) {
                    Some(l) => {
                        self.setlocal(l, er);
                    }
                    None => {
                        // FIXME: type matters here, where do we get that from?
                        self.setglobal(a.name.clone(), er);
                    }
                }
                return er;
            }
            Expr::Call(ref c) => {
                // Generate outgoing() nodes just before the call;
                // call just carries the number of arguments
                //
                // Coercions are all explicit here.  Return type should
                // be readable from the call node, we don't need the
                // signature.

                // for k in 0..c.args.len() {
                //     let ta = try!(self.check_expr(&c.args[k]));
                //     try!(self.check_same(ta, sign.formals[k]));
                // }
                // c.ty.set(sign.ret);
                // Ok(sign.ret)
                0           // FIXME
            }
            Expr::IntLit(i) => {
                self.literal(Lit::I(i.n))
            }
            Expr::NumLit(n) => {
                self.literal(Lit::N(n.n))
            }
            Expr::Id(ref v) => {
                let t = self.local.lookup(&v.name);
                //v.ty.set(t);
                //Ok(t)
                0 // FIXME
            }
        }
    }
    
    // These add the binding in the innermost scope (where it must not
    // exist), using a new variable number for the appropriate type.

    fn new_local_int(&mut self, name:&Name) -> Local {
        Local::I(0) // FIXME
    }

    fn new_local_num(&mut self, name:&Name) -> Local {
        Local::N(0) // FIXME
    }

    fn new_temp_int(&mut self) -> Local {
        let t = self.next_int();
        return Local::I(t);
    }

    fn getlocal(&mut self, l:Local) -> Val {
        return self.add_instr(Op::GetLocal(l));
    }

    fn setlocal(&mut self, l:Local, v:Val) {
        self.add_instr(Op::SetLocal(l, v));
    }

    fn getglobal(&mut self, g:Global) -> Val {
        //return self.add_instr(Op::GetGlobal(l));
        0 // FIXME
    }

    fn setglobal(&mut self, g:Global, v:Val) {
        // FIXME
        //self.add_instr(Op::SetGlobal(l, v));
    }

    fn jcc(&mut self, cond:OB, v:Val, if_true:Label, if_false:Label) {
        self.add_instr(Op::Jcc(cond, v, if_true, if_false));
        self.new_block();
    }

    fn jump(&mut self, target:Label) {
        self.add_instr(Op::Jump(target));
        self.new_block();
    }

    fn returnval(&mut self, v:Val, next:Label) {
        self.add_instr(Op::ReturnVal(v, next));
        self.new_block();
    }
                       
    fn returnvoid(&mut self, next:Label) {
        self.add_instr(Op::ReturnVoid(next));
        self.new_block();
    }
                       
    fn nop(&mut self) {
        self.add_instr(Op::Nop);
    }

    fn notreached(&mut self) {
        self.add_instr(Op::Notreached);
    }

    fn literal(&mut self, l:Lit) -> Val {
        return self.add_instr(Op::Literal(l));
    }
    
    fn incoming(&mut self, arg: Arg) -> Val {
        return self.add_instr(Op::Incoming(arg));
    }
    
    fn outgoing(&mut self, arg: Arg, v: Val) {
        self.add_instr(Op::Outgoing(arg, v));
    }

    fn op1(&mut self, op:O1, rs:Val) -> Val {
        return self.add_instr(Op::Op1(op, rs));
    }
           
    fn op2(&mut self, op:O2, rs1:Val, rs2:Val) -> Val {
        return self.add_instr(Op::Op2(op, rs1, rs2));
    }
           
    fn new_label(&mut self) -> Label {
        return ir::label_unbound();
    }
    
    fn bind_label(&mut self, l:Label) {
        debug_assert!(match &self.ir[self.last].op {
            &Op::Block(_) => true,
            _ => false
        });

        l.set((self.ir.len() - 1) as u32);
    }
    
    fn new_block(&mut self) {
        debug_assert!(match &self.ir[self.last].op {
            &Op::Jump(_) | &Op::Jcc(_,_,_,_) | &Op::ReturnVal(_,_) | &Op::ReturnVoid(_) => true,
            _ => false
        });

        let idx = self.ir.len();
        self.ir.push(IR { op: Op::Block(None), prev: ir::NULL, next: ir::NULL });
        self.last = idx;
    }
    

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

    // Invariant: every block is terminated by a CTI.  For the
    // synthetic instruction before the first block this is a jump to
    // itself.  For the synthetic instruction at the end of the last
    // block this should also be a jump to itself.

    fn init_ir(&mut self) {
        debug_assert!(self.ir.len() == 0);
        self.ir.push(IR { op: Op::Jump(ir::label_bound(0)), prev: ir::NULL, next: ir::NULL });
        self.last = 0;
    }

    // Invariant: The last instruction must be a CTI.

    fn add_instr(&mut self, op:Op) -> Val {
        let idx = self.ir.len() as u32;
        self.ir.push(IR { op: op, prev: self.last as u32, next: ir::NULL });
        self.ir[self.last].next = idx;
        self.last = idx as usize;
        return idx;
    }
}
