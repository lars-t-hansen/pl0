use std::collections::HashMap;

use ast::*;

#[derive(Debug)]
pub struct TypeErr
{
    pub msg: String
}

pub type Res = Result<(), TypeErr>;

pub fn check_program(p:&Program) -> Res {
    let mut check = TyCheck::new();
    try!(check.program(p));
    try!(check.functions(p));
    Ok(())
}

// We can split into local/global binding here to avoid
// problems with cloning function signatures

#[derive(Clone, Debug)]         // TODO: Bad, really - how to avoid?
struct Signature
{
    formals: Vec<TypeName>,
    ret: TypeName
}

#[derive(Clone)]                // Clone necessary for lookup()
enum Binding
{
    Unbound,
    Var(TypeName),
    Fn(Signature)
}

// "Scope" is misleading, this is really a more general function
// checking context.
//
// `locals` could be a vector of hash tables, and for large local
// scopes that will be important.  It could also be a hash table of
// vectors, but that's more complicated.

struct Scope
{
    ret:    TypeName,
    locals: Vec<Vec<(String, Binding)>>
}

impl Scope
{
    fn new(ret:TypeName) -> Scope {
        Scope {
            ret: ret,
            locals: Vec::<Vec<(String, Binding)>>::new(),
        }
    }
    
    // Returns the binding and the relative level.  Iff unbound then level is -1.

    fn lookup(&self, name:&String) -> (Binding, i32) {
        let mut r = self.locals.len()-1;
        let mut level = 0;
        while r > 0 {
            let rib = &self.locals[r-1];
            let mut i = 0;
            while i < rib.len() {
                let (ref bname, ref binding) = rib[i];
                if bname == name {
                    return (binding.clone(), level);
                }
                i = i+1;
            }
            r = r-1;
            level = level + 1;
        }
        return (Binding::Unbound, -1);
    }

    fn add_var(&mut self, name:&String, ty:TypeName) {
        self.locals[0].push((name.clone(), Binding::Var(ty)));
    }

    fn push(&mut self) {
        self.locals.push(Vec::<(String,Binding)>::new());
    }

    fn pop(&mut self) {
        self.locals.pop();
    }        
}

// TyCheck is populated during the initial phase but is stable thereafter
// and can be shared among mutliple threads, we hope.

struct TyCheck
{
    globals: HashMap<String, Binding>,
    externs: HashMap<String, Binding>
}

impl TyCheck
{
    fn new() -> TyCheck {
        TyCheck {
            globals: HashMap::<String, Binding>::new(),
            externs: HashMap::<String, Binding>::new()
        }
    }
    
    fn program(&mut self, p:&Program) -> Res {
        for fd in &p.fns {
            match self.globals.get(&fd.name) {
                None => {
                    let formals = fd.formals.iter().map(|x| x.ty).collect();
                    let sign = Signature { ret: fd.ret, formals: formals };
                    self.globals.insert(fd.name.clone(), Binding::Fn(sign));
                }
                Some(_) => {
                    return Err(self.error("Duplicate global definition"));
                }
            }
        }
        for vd in &p.vars {
            match self.globals.get(&vd.name) {
                None => {
                    self.globals.insert(vd.name.clone(), Binding::Var(vd.ty));
                }
                Some(_) => {
                    return Err(self.error("Duplicate global definition"));
                }
            }
        }

        self.externs.insert(String::from("printi"),
                            Binding::Fn(Signature { ret: TypeName::VOID, formals: vec![TypeName::INT] }));
        self.externs.insert(String::from("printn"),
                            Binding::Fn(Signature { ret: TypeName::VOID, formals: vec![TypeName::NUM] }));

        Ok(())
    }

    fn functions(&self, p:&Program) -> Res {
        for f in &p.fns {
            try!(self.check_function(&f));
        }
        Ok(())
    }
    
    fn check_function(&self, f:&FunDefn) -> Res {
        let mut scope = Scope::new(f.ret);

        scope.push();
        for vd in &f.formals {
            let (_, level) = scope.lookup(&vd.name);
            if level != -1 {
                return Err(self.error("Function parameter names must be distinct"));
            }
            scope.add_var(&vd.name, vd.ty);
        }
        
        let returns = try!(self.check_stmt(&mut scope, &f.body));
        if f.ret != TypeName::VOID && !returns {
            return Err(self.error("Function must return a value"));
        }

        Ok(())
    }

    // The statement checkers return true if the statement always executes
    // a return statement.

    fn check_stmt(&self, scope:&mut Scope, stmt:&Stmt) -> Result<bool, TypeErr> {
        match *stmt {
            Stmt::Block(ref s) => self.check_block(scope, s),
            Stmt::Expr(ref s) => {
                try!(self.check_expr(scope, &s.expr));
                Ok(false)
            }
            Stmt::If(ref s) => self.check_if(scope, s),
            Stmt::Return(ref s) => self.check_return(scope, s),
            Stmt::While(ref s) => self.check_while(scope, s),
            Stmt::Var(ref s) => {
                // Don't need to consider global scopes here.
                let (_, level) = scope.lookup(&s.defn.name);
                if level == 0 {
                    return Err(self.error("Binding already exists"));
                }
                scope.add_var(&s.defn.name, s.defn.ty);
                Ok(false)
            }
        }
    }

    fn check_block(&self, scope:&mut Scope, stmt:&Box<BlockStmt>) -> Result<bool, TypeErr> {
        scope.push();
        let mut res = false;
        for s in &stmt.phrases {
            // Once a statement returns for sure, we're going to return for sure.
            // We check the rest but they don't matter.
            //
            // TODO: Warn about dead code?
            res = res || try!(self.check_stmt(scope, &s));
        }
        scope.pop();
        Ok(res)
    }

    fn check_if(&self, scope:&mut Scope, stmt:&Box<IfStmt>) -> Result<bool, TypeErr> {
        let t = try!(self.check_expr(scope, &stmt.test));
        try!(self.check_int(t));
        match &stmt.alternate {
            &None => {
                try!(self.check_stmt(scope, &stmt.consequent));
                Ok(false)
            }
            &Some(ref alternate) => {
                let a = try!(self.check_stmt(scope, &stmt.consequent));
                let b = try!(self.check_stmt(scope, alternate));
                Ok(a && b)
            }
        }
    }
    
    fn check_while(&self, scope:&mut Scope, stmt:&Box<WhileStmt>) -> Result<bool, TypeErr> {
        let t = try!(self.check_expr(scope, &stmt.test));
        try!(self.check_int(t));
        try!(self.check_stmt(scope, &stmt.body));
        Ok(false)
    }

    fn check_return(&self, scope:&mut Scope, stmt:&Box<ReturnStmt>) -> Result<bool, TypeErr> {
        let t = 
            match stmt.expr {
                Some(ref e) => try!(self.check_expr(scope, e)),
                None => TypeName::VOID
            };
        try!(self.check_same(scope.ret, t));
        stmt.ty.set(t);
        Ok(true)
    }

    // check_expr may store the expression type in a node-specific way, to use
    // for ir generation, and always return the type.
    
    fn check_expr(&self, scope:&mut Scope, e:&Expr) -> Result<TypeName, TypeErr> {
        match *e {
            Expr::Unary(ref u) => {
                let t = try!(self.check_expr(scope, &u.expr));
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
                let tl = try!(self.check_expr(scope, &b.lhs));
                let tr = try!(self.check_expr(scope, &b.rhs));
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
                let tl = try!(self.lookup_var(scope, &a.name));
                let tr = try!(self.check_expr(scope, &a.expr));
                try!(self.check_same(tl, tr));
                a.ty.set(tl);
                Ok(tl)
            }
            Expr::Call(ref c) => {
                // FIXME: allow overloaded functions
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
            Expr::IntLit(_) => {
                Ok(TypeName::INT)
            }
            Expr::NumLit(_) => {
                Ok(TypeName::NUM)
            }
            Expr::Id(ref v) => {
                let t = try!(self.lookup_var(scope, &v.name));
                v.ty.set(t);
                Ok(t)
            }
        }
    }

    fn lookup_var(&self, scope:&Scope, name:&String) -> Result<TypeName, TypeErr> {
        let (probe, _) = scope.lookup(name);
        match probe {
            Binding::Var(t) => Ok(t),
            Binding::Fn(_) => Err(self.error("Name must reference variable")),
            Binding::Unbound => {
                match self.globals.get(name) {
                    Some(&Binding::Var(t)) => Ok(t),
                    Some(_) => Err(self.error("Name must reference variable")),
                    None => {
                        match self.externs.get(name) {
                            Some(&Binding::Var(t)) => Ok(t),
                            Some(_) => Err(self.error("Name must reference variable")),
                            None => Err(self.error("Unbound variable"))
                        }
                    }
                }
            }
        }
    }

    fn lookup_fn(&self, scope:&Scope, name:&String) -> Result<Signature, TypeErr> {
        let (probe, _) = scope.lookup(name);
        match probe {
            Binding::Fn(s) => Ok(s),
            Binding::Var(_) => Err(self.error("Name must reference function")),
            Binding::Unbound => {
                match self.globals.get(name) {
                    Some(&Binding::Fn(ref s)) => Ok(s.clone()),
                    Some(_) => Err(self.error("Name must reference function")),
                    None => {
                        match self.externs.get(name) {
                            Some(&Binding::Fn(ref s)) => Ok(s.clone()),
                            Some(_) => Err(self.error("Name must reference function")),
                            None => Err(self.error("Unbound function"))
                        }
                    }
                }
            }
        }
    }

    fn check_int_or_num(&self, t:TypeName) -> Res {
        match t {
            TypeName::INT | TypeName::NUM => Ok(()),
            _ => Err(self.error("Int or Num type required"))
        }
    }

    fn check_int(&self, t:TypeName) -> Res {
        match t {
            TypeName::INT => Ok(()),
            _ => Err(self.error("Int type required"))
        }
    }

    fn check_num(&self, t:TypeName) -> Res {
        match t {
            TypeName::NUM => Ok(()),
            _ => Err(self.error("Num type required"))
        }
    }

    fn check_same(&self, tl:TypeName, tr:TypeName) -> Res {
        if tl == tr { Ok(()) } else { Err(self.error("Types must be equal")) }
    }
    
    fn error(&self, msg: &str) -> TypeErr {
        TypeErr { msg: String::from(msg) }
    }
}
