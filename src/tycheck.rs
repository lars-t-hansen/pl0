use ast::*;
use env::{GlobalEnv, LocalEnv};
use err::TypeErr;
use names::{Name, NameTable};

use std::sync::Arc;
use std::thread;

#[derive(Clone)]
struct Signature
{
    formals: Vec<TypeName>,
    ret: TypeName
}

#[derive(Clone)]
enum Binding
{
    Var(TypeName),
    Fn(Signature)
}

pub type Res = Result<(), TypeErr>;

type EnvArc = Arc<Box<GlobalEnv<Binding>>>;
pub struct Env(EnvArc);

pub fn new_env() -> Env {
    Env(Arc::new(Box::new(GlobalEnv::new())))
}

pub fn check_program(names:&mut NameTable, p:&mut Program, e:&mut Env) -> Res {
    let &mut Env(ref mut envrc) = e;
    let env = Arc::get_mut(envrc).unwrap();

    for fd in &p.fns {
        let sign = Signature {
            ret: fd.ret,
            formals: fd.formals.iter().map(|x| x.ty).collect()
        };
        if !env.add_global(&fd.name, Binding::Fn(sign)) {
            return Err(error("Duplicate global name"));
        }
    }
    for vd in &p.vars {
        if !env.add_global(&vd.name, Binding::Var(vd.ty)) {
            return Err(error("Duplicate global name"));
        }
    }

    env.add_extern(&names.add(&"printi".to_string()),
                   Binding::Fn(Signature { ret: TypeName::VOID, formals: vec![TypeName::INT] }));
    env.add_extern(&names.add(&"printn".to_string()),
                   Binding::Fn(Signature { ret: TypeName::VOID, formals: vec![TypeName::NUM] }));

    Ok(())
}

// For expository purposes we check each function individually in a
// separate thread with a shared global environment for all the
// threads.

pub fn check_functions(p:&mut Program, e:&Env) -> Res {
    let &Env(ref env) = e;
    let mut threads = Vec::new();

    for f in p.fns.drain(..) {
        let mut fun = FnCheck::new(env.clone(), f.ret);
        threads.push(thread::spawn(move || -> Result<FunDefn, TypeErr> {
            fun.check_function(f)
        }));
    }

    let mut errors = 0;
    for t in threads.drain(..) {
        match t.join() {
            Err(e) => {
                println!("internal error: {:?}", e);
                errors += 1;
            }
            Ok(Err(e)) => {
                println!("type error: {}", e.msg);
                errors += 1;
            }
            Ok(Ok(f)) => {
                p.fns.push(f);
            }
        }
    }

    if errors > 0 { Err(error("Type checking failed")) } else { Ok(()) }
}

fn error(msg: &str) -> TypeErr {
    TypeErr { msg: msg.to_string() }
}
    
struct FnCheck
{
    ret:    TypeName,
    env:    EnvArc,
    locals: LocalEnv<Binding>
}

impl FnCheck
{
    fn new(env:EnvArc, ret:TypeName) -> FnCheck {
        FnCheck {
            ret: ret,
            env: env.clone(),
            locals: LocalEnv::new()
        }
    }
    
    fn check_function(&mut self, f:FunDefn) -> Result<FunDefn, TypeErr> {
        self.locals.push();
        for vd in &f.formals {
            if !self.add_var(&vd.name, vd.ty) {
                return Err(error("Function parameter names must be distinct"));
            }
        }
        
        let returns = try!(self.check_stmt(&f.body));
        if f.ret != TypeName::VOID && !returns {
            return Err(error("Function must return a value"));
        }
        self.locals.pop();

        Ok(f)
    }

    // The statement checkers return true if the statement always executes
    // a return statement.

    fn check_stmt(&mut self, stmt:&Stmt) -> Result<bool, TypeErr> {
        match *stmt {
            Stmt::Block(ref s) => self.check_block(s),
            Stmt::Expr(ref s) => {
                try!(self.check_expr(&s.expr));
                Ok(false)
            }
            Stmt::If(ref s) => self.check_if(s),
            Stmt::Return(ref s) => self.check_return(s),
            Stmt::While(ref s) => self.check_while(s),
            Stmt::Var(ref s) => {
                if !self.add_var(&s.defn.name, s.defn.ty) {
                    return Err(error("Binding already exists in this scope"));
                }                    
                Ok(false)
            }
        }
    }

    fn check_block(&mut self, stmt:&Box<BlockStmt>) -> Result<bool, TypeErr> {
        self.locals.push();
        let mut res = false;
        for s in &stmt.phrases {
            // Once a statement returns for sure, we're going to return for sure.
            // We check the rest but they don't matter.
            //
            // TODO: Warn about dead code?
            res = res || try!(self.check_stmt(&s));
        }
        self.locals.pop();
        Ok(res)
    }

    fn check_if(&mut self, stmt:&Box<IfStmt>) -> Result<bool, TypeErr> {
        let t = try!(self.check_expr(&stmt.test));
        try!(self.check_int(t));
        match &stmt.alternate {
            &None => {
                try!(self.check_stmt(&stmt.consequent));
                Ok(false)
            }
            &Some(ref alternate) => {
                let a = try!(self.check_stmt(&stmt.consequent));
                let b = try!(self.check_stmt(alternate));
                Ok(a && b)
            }
        }
    }
    
    fn check_while(&mut self, stmt:&Box<WhileStmt>) -> Result<bool, TypeErr> {
        let t = try!(self.check_expr(&stmt.test));
        try!(self.check_int(t));
        try!(self.check_stmt(&stmt.body));
        Ok(false)
    }

    fn check_return(&mut self, stmt:&Box<ReturnStmt>) -> Result<bool, TypeErr> {
        let t = 
            match stmt.expr {
                Some(ref e) => try!(self.check_expr(e)),
                None => TypeName::VOID
            };
        try!(self.check_same(self.ret, t));
        stmt.ty.set(t);
        Ok(true)
    }

    // check_expr may store the expression type in a node-specific way, to use
    // for ir generation, and always return the type.
    
    fn check_expr(&mut self, e:&Expr) -> Result<TypeName, TypeErr> {
        match *e {
            Expr::Unary(ref u) => {
                let t = try!(self.check_expr(&u.expr));
                match u.op {
                    Unop::Negate => {
                        try!(self.check_int_or_num(t));
                        u.ty.set(t);
                    }
                    Unop::Not => {
                        try!(self.check_int(t));
                        u.ty.set(TypeName::INT);
                    }
                    Unop::ToInt => {
                        try!(self.check_num(t));
                        u.ty.set(TypeName::INT);
                    }
                    Unop::ToNum => {
                        try!(self.check_int(t));
                        u.ty.set(TypeName::NUM);
                    }
                }
                Ok(u.ty.get())
            }
            Expr::Binary(ref b) => {
                let tl = try!(self.check_expr(&b.lhs));
                let tr = try!(self.check_expr(&b.rhs));
                match b.op {
                    Binop::Add | Binop::Subtract | Binop::Multiply | Binop::Divide | Binop::Modulo => {
                        try!(self.check_int_or_num(tl));
                        try!(self.check_int_or_num(tr));
                        try!(self.check_same(tl, tr));
                        b.ty.set(tl);
                    }
                    Binop::Equal | Binop::NotEqual |
                    Binop::Less | Binop::LessOrEqual | Binop::Greater | Binop::GreaterOrEqual => {
                        try!(self.check_int_or_num(tl));
                        try!(self.check_int_or_num(tr));
                        try!(self.check_same(tl, tr));
                        b.ty.set(TypeName::INT);
                    }
                    Binop::And | Binop::Or => {
                        try!(self.check_int(tl));
                        try!(self.check_int(tr));
                        b.ty.set(TypeName::INT);
                    }
                }
                Ok(b.ty.get())
            }
            Expr::Assign(ref a) => {
                let tl = try!(self.lookup_var(&a.name));
                let tr = try!(self.check_expr(&a.expr));
                try!(self.check_same(tl, tr));
                a.ty.set(tl);
                Ok(tl)
            }
            Expr::Call(ref c) => {
                // FIXME: allow overloaded functions
                let sign = try!(self.lookup_fn(&c.name));
                if sign.formals.len() != c.args.len() {
                    return Err(error("Wrong number of arguments"));
                }
                for k in 0..c.args.len() {
                    let ta = try!(self.check_expr(&c.args[k]));
                    try!(self.check_same(ta, sign.formals[k]));
                }
                c.ty.set(sign.ret);
                Ok(sign.ret)
            }
            Expr::IntLit(_) => {
                Ok(TypeName::INT)
            }
            Expr::NumLit(_) => {
                Ok(TypeName::NUM)
            }
            Expr::Id(ref v) => {
                let t = try!(self.lookup_var(&v.name));
                v.ty.set(t);
                Ok(t)
            }
        }
    }

    fn add_var(&mut self, name:&Name, ty:TypeName) -> bool {
        self.locals.add(name, Binding::Var(ty))
    }

    fn lookup_var(&self, name:&Name) -> Result<TypeName, TypeErr> {
        match self.locals.lookup(name) {
            Some(Binding::Var(t)) => Ok(t),
            Some(_) => Err(error("Name must reference variable")),
            None => match self.env.lookup(name) {
                Some(Binding::Var(t)) => Ok(t),
                Some(_) | None => Err(error("Name must reference variable")),
            }
        }
    }

    fn lookup_fn(&self, name:&Name) -> Result<Signature, TypeErr> {
        match self.locals.lookup(name) {
            Some(_) => Err(error("Name must reference function")),
            None => match self.env.lookup(name) {
                Some(Binding::Fn(ref s)) => Ok(s.clone()),
                Some(_) | None => Err(error("Name must reference function")),
            }
        }
    }

    fn check_int_or_num(&self, t:TypeName) -> Res {
        match t {
            TypeName::INT | TypeName::NUM => Ok(()),
            _ => Err(error("Int or Num type required"))
        }
    }

    fn check_int(&self, t:TypeName) -> Res {
        match t {
            TypeName::INT => Ok(()),
            _ => Err(error("Int type required"))
        }
    }

    fn check_num(&self, t:TypeName) -> Res {
        match t {
            TypeName::NUM => Ok(()),
            _ => Err(error("Num type required"))
        }
    }

    fn check_same(&self, tl:TypeName, tr:TypeName) -> Res {
        if tl == tr { Ok(()) } else { Err(error("Types must be equal")) }
    }
}
