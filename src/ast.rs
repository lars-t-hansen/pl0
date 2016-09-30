use std::cell::Cell;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TypeName
{
    VOID,                       // For functions + uncomputed types
    INT,
    NUM
}

#[derive(Debug)]
pub struct Program
{
    pub fns: Vec<FunDefn>,
    pub vars: Vec<VarDefn>
}

impl Program
{
    pub fn new(fns: Vec<FunDefn>, vars: Vec<VarDefn>) -> Program {
        Program { fns: fns, vars: vars }
    }
}

#[derive(Debug)]
pub struct FunDefn
{
    pub name: String,
    pub formals: Vec<VarDefn>,
    pub ret: TypeName,
    pub body: Stmt              // Always a BlockStmt
}

impl FunDefn
{
    pub fn new(name: String, formals: Vec<VarDefn>, ret: TypeName, body: Stmt) -> FunDefn {
        match body {
            Stmt::Block(_) => (),
            _              => assert!(false)
        }
        FunDefn { name: name, formals: formals, ret: ret, body: body }
    }
}

#[derive(Debug)]
pub struct VarDefn
{
    pub name: String,
    pub ty: TypeName
}

impl VarDefn
{
    pub fn new(name: String, ty: TypeName) -> VarDefn {
        VarDefn { name: name, ty: ty }
    }
}

#[derive(Debug)]
pub enum Stmt
{
    Block(Box<BlockStmt>),
    Expr(Box<ExprStmt>),
    If(Box<IfStmt>),
    Return(Box<ReturnStmt>),
    While(Box<WhileStmt>),
    Var(Box<VarDefnStmt>)           // Inside functions only
}

#[derive(Debug)]
pub struct VarDefnStmt
{
    pub defn: VarDefn
}

impl VarDefnStmt
{
    pub fn new(defn: VarDefn) -> Stmt {
        Stmt::Var(Box::new(VarDefnStmt{ defn: defn }))
    }
}

#[derive(Debug)]
pub struct BlockStmt
{
    pub phrases: Vec<Stmt>
}

impl BlockStmt
{
    pub fn new(phrases: Vec<Stmt>) -> Stmt {
        Stmt::Block(Box::new( BlockStmt { phrases: phrases } ))
    }
}

#[derive(Debug)]
pub struct ExprStmt
{
    pub expr: Expr
}

impl ExprStmt
{
    pub fn new(expr: Expr) -> Stmt {
        Stmt::Expr(Box::new( ExprStmt { expr: expr } ))
    }
}

#[derive(Debug)]
pub struct IfStmt
{
    pub test: Expr,
    pub consequent: Stmt,
    pub alternate: Option<Stmt>
}

impl IfStmt
{
    pub fn new(test: Expr, consequent: Stmt, alternate: Option<Stmt>) -> Stmt {
        Stmt::If(Box::new(IfStmt { test: test, consequent: consequent, alternate: alternate }))
    }
}

#[derive(Debug)]
pub struct WhileStmt
{
    pub test: Expr,
    pub body: Stmt
}

impl WhileStmt
{
    pub fn new(test: Expr, body: Stmt) -> Stmt {
        Stmt::While(Box::new(WhileStmt { test: test, body: body }))
    }
}

#[derive(Debug)]
pub struct ReturnStmt
{
    pub expr: Option<Expr>,
    pub ty: Cell<TypeName>
}

impl ReturnStmt
{
    pub fn new(expr: Option<Expr>) -> Stmt {
        Stmt::Return(Box::new(ReturnStmt { expr: expr, ty: Cell::new(TypeName::VOID) }))
    }
}

#[derive(Debug)]
pub enum Expr
{
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Call(Box<CallExpr>),
    IntLit(Box<IntLitExpr>),
    NumLit(Box<NumLitExpr>),
    Id(Box<IdExpr>),
    Assign(Box<AssignExpr>)
}
    
#[derive(Debug)]
pub enum Unop
{
    Negate,
    Not,
    ToInt,
    ToNum
}

#[derive(Debug)]
pub struct UnaryExpr
{
    pub op: Unop,
    pub expr: Expr,
    pub ty: Cell<TypeName>
}

impl UnaryExpr
{
    pub fn new(op: Unop, expr: Expr) -> Expr {
        Expr::Unary(Box::new( UnaryExpr { op: op, expr: expr, ty: Cell::new(TypeName::VOID) } ))
    }
}

#[derive(Debug)]
pub enum Binop
{
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    And,
    Or
}

#[derive(Debug)]
pub struct BinaryExpr
{
    pub op: Binop,
    pub lhs: Expr,
    pub rhs: Expr,
    pub ty: Cell<TypeName>
}

impl BinaryExpr
{
    pub fn new(op: Binop, lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(Box::new( BinaryExpr { op: op, lhs: lhs, rhs: rhs, ty: Cell::new(TypeName::VOID) } ))
    }
}

#[derive(Debug)]
pub struct CallExpr
{
    pub name: String,
    pub args: Vec<Expr>,
    pub ty: Cell<TypeName>
}

impl CallExpr
{
    pub fn new(name: String, args: Vec<Expr>) -> Expr {
        Expr::Call(Box::new(CallExpr { name: name, args: args, ty: Cell::new(TypeName::VOID) }))
    }
}

#[derive(Debug)]
pub struct AssignExpr
{
    pub name: String,
    pub expr: Expr,
    pub ty: Cell<TypeName>
}

impl AssignExpr
{
    pub fn new(name: String, expr: Expr) -> Expr {
        Expr::Assign(Box::new(AssignExpr { name: name, expr: expr, ty: Cell::new(TypeName::VOID) }))
    }
}

#[derive(Debug)]
pub struct IntLitExpr
{
    pub n: i64
}

impl IntLitExpr
{
    pub fn new(n: i64) -> Expr {
        Expr::IntLit(Box::new(IntLitExpr { n: n }))
    }
}

#[derive(Debug)]
pub struct NumLitExpr
{
    pub n: f64
}

impl NumLitExpr
{
    pub fn new(n: f64) -> Expr {
        Expr::NumLit(Box::new(NumLitExpr { n: n }))
    }
}

#[derive(Debug)]
pub struct IdExpr
{
    pub name: String,
    pub ty: Cell<TypeName>
}

impl IdExpr
{
    pub fn new(name: String) -> Expr {
        Expr::Id(Box::new(IdExpr { name: name, ty: Cell::new(TypeName::VOID) }))
    }
}
