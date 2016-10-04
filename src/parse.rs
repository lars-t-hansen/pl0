use ast::*;
use err::ParseErr;
use lex::Lex;
use lex::Token;

pub fn parse(l:Lex) -> Result<Program, ParseErr> {
    Parse::new(l).parse()
}

struct Parse
{
    l: Lex,
    ungotten: bool,
    ungot: Token
}

macro_rules! binary_to_op {
    (AND) => (Binop::And);
    (OR) => (Binop::Or);
    (EQUALS) => (Binop::Equal);
    (NOTEQUALS) => (Binop::NotEqual);
    (LESS) => (Binop::Less);
    (LESSEQ) => (Binop::LessOrEqual);
    (GREATER) => (Binop::Greater);
    (GREATEREQ) => (Binop::GreaterOrEqual);
    (PLUS) => (Binop::Add);
    (MINUS) => (Binop::Subtract);
    (TIMES) => (Binop::Multiply);
    (DIVIDE) => (Binop::Divide);
    (REMAINDER) => (Binop::Modulo);
}

macro_rules! binop_left {
    ($name:ident, $next:ident, $($tok:ident),*)
        =>
    {
        fn $name(&mut self) -> Result<Expr, ParseErr> {
            let mut lhs = try!(self.$next());
            loop {
                let op;
                if false { break; }
                $( else if self.eat_token(Token::$tok) { op = binary_to_op!($tok); } )*
                else { break; }
                let rhs = try!(self.$next());
                let new_lhs = BinaryExpr::new(op, lhs, rhs);
                lhs = new_lhs;
            }
            return Ok(lhs);
        }
    }
}

impl Parse
{
    fn new(l: Lex) -> Parse {
        Parse {
            l: l,
            ungotten: false,
            ungot: Token::EOI
        }
    }

    fn parse(&mut self) -> Result<Program, ParseErr> {
        let mut fns = Vec::<FunDefn>::new();
        let mut vars = Vec::<VarDefn>::new();
        loop {
            let t = self.next();
            match t {
                Token::EOI => {
                    return Ok(Program::new(fns, vars));
                }
                Token::INT | Token::NUM => {
                    vars.push(try!(self.vardefn(t)));
                }
                Token::FN => {
                    fns.push(try!(self.fundefn()));
                }
                _ => {
                    return Err(self.error("Expected function or variable definition"));
                }
            }
        }
    }

    // Definitions

    // Type name consumed, is 't'
    fn vardefn(&mut self, t:Token) -> Result<VarDefn, ParseErr> {
        let ty = try!(self.token_to_type(t));
        let name = try!(self.match_ident());
        try!(self.match_token(Token::SEMI));
        Ok(VarDefn::new(name, ty))
    }

    // FN consumed
    fn fundefn(&mut self) -> Result<FunDefn, ParseErr> {
        let name = try!(self.match_ident());
        try!(self.match_token(Token::LPAREN));
        let mut formals = Vec::<VarDefn>::new();
        while self.peek() == Token::INT || self.peek() == Token::NUM {
            let t = self.next();
            let ty = try!(self.token_to_type(t));
            let name = try!(self.match_ident());
            formals.push(VarDefn::new(name, ty));
            if !self.eat_token(Token::COMMA) {
                break;
            }
        }
        try!(self.match_token(Token::RPAREN));
        let ty = 
            if self.eat_token(Token::ARROW) {
                let t = self.next();
                try!(self.token_to_type(t))
            } else {
                TypeName::VOID
            };
        try!(self.match_token(Token::LBRACE));
        let body = try!(self.block_stmt());
        Ok(FunDefn::new(name, formals, ty, body))
    }

    // Statements

    fn stmt(&mut self) -> Result<Stmt, ParseErr> {
        let t = self.next();
        match t {
            Token::LBRACE => self.block_stmt(),
            Token::IF     => self.if_stmt(),
            Token::RETURN => self.return_stmt(),
            Token::WHILE  => self.while_stmt(),
            _ => {
                self.unget(t);
                let e = try!(self.expr());
                try!(self.match_token(Token::SEMI));
                Ok(ExprStmt::new(e))
            }
        }
    }

    // LBRACE consumed
    fn block_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let mut phrases = Vec::<Stmt>::new();
        loop {
            let t = self.next();
            match t {
                Token::RBRACE => {
                    return Ok(BlockStmt::new(phrases));
                }
                Token::INT | Token::NUM => {
                    // FIXME: only if the next token is not "(" because in that
                    // case it's a redundant cast as part of an expression
                    // statement.   But we can't pushback two tokens yet.
                    phrases.push(VarDefnStmt::new(try!(self.vardefn(t))))
                }
                _ => {
                    self.unget(t);
                    phrases.push(try!(self.stmt()));
                }
            }
        }
    }
    
    // IF consumed
    fn if_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let e = try!(self.paren_expr());
        let s1 = try!(self.stmt());
        let s2 =
            if self.eat_token(Token::ELSE) {
                Some(try!(self.stmt()))
            } else {
                None
            };
        Ok(IfStmt::new(e, s1, s2))
    }
    
    // RETURN consumed
    fn return_stmt(&mut self) -> Result<Stmt, ParseErr> {
        if self.eat_token(Token::SEMI) {
            Ok(ReturnStmt::new(None))
        } else {
            let e = try!(self.expr());
            try!(self.match_token(Token::SEMI));
            Ok(ReturnStmt::new(Some(e)))
        }
    }
    
    // WHILE consumed
    fn while_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let e = try!(self.paren_expr());
        let s = try!(self.stmt());
        Ok(WhileStmt::new(e, s))
    }

    // Expressions

    fn paren_expr(&mut self) -> Result<Expr, ParseErr> {
        try!(self.match_token(Token::LPAREN));
        let e = try!(self.expr());
        try!(self.match_token(Token::RPAREN));
        Ok(e)
    }
    
    fn expr(&mut self) -> Result<Expr, ParseErr> {
        self.assign_expr()
    }

    fn assign_expr(&mut self) -> Result<Expr, ParseErr> {
        // FIXME: This is wrong, because expressions like "(hi) = 5" will work,
        // but the lhs should never be allowed to be other than a plain ident.
        let mut lhs = try!(self.or_expr());
        while self.eat_token(Token::ASSIGN) {
            let rhs = try!(self.assign_expr());
            let new_lhs =
                match lhs {
                    Expr::Id(ref vref) => {
                        AssignExpr::new(vref.name.clone(), rhs)
                    }
                    _ => {
                        return Err(self.error("Identifier required in assignment"));
                    }
                };
            lhs = new_lhs;
        }
        return Ok(lhs);
    }

    binop_left! { or_expr, and_expr, OR }
    binop_left! { and_expr, eq_expr, AND }
    binop_left! { eq_expr, cmp_expr, EQUALS, NOTEQUALS }
    binop_left! { cmp_expr, add_expr, LESS, LESSEQ, GREATER, GREATEREQ }
    binop_left! { add_expr, mul_expr, PLUS, MINUS }
    binop_left! { mul_expr, un_expr, TIMES, DIVIDE, REMAINDER }

    fn un_expr(&mut self) -> Result<Expr, ParseErr> {
        if self.eat_token(Token::MINUS) {
            Ok(UnaryExpr::new(Unop::Negate, try!(self.un_expr())))
        } else if self.eat_token(Token::NOT) {
            Ok(UnaryExpr::new(Unop::Not, try!(self.un_expr())))
        } else {
            self.primary_expr()
        }
    }

    fn primary_expr(&mut self) -> Result<Expr, ParseErr> {
        match self.next() {
            Token::IDENT(id) => {
                if self.eat_token(Token::LPAREN) {
                    let mut args = Vec::<Expr>::new();
                    if self.peek() != Token::RPAREN {
                        loop {
                            let e = try!(self.expr());
                            args.push(e);
                            if !self.eat_token(Token::COMMA) {
                                break;
                            }
                        }
                    }
                    try!(self.match_token(Token::RPAREN));
                    Ok(CallExpr::new(id, args))
                } else {
                    Ok(IdExpr::new(id))
                }
            }
            Token::INT => {
                Ok(UnaryExpr::new(Unop::ToInt, try!(self.paren_expr())))
            }
            Token::NUM => {
                Ok(UnaryExpr::new(Unop::ToNum, try!(self.paren_expr())))
            }
            Token::INTLIT(n) => {
                Ok(IntLitExpr::new(n))
            }
            Token::NUMLIT(n) => {
                Ok(NumLitExpr::new(n))
            }
            Token::LPAREN => {
                let expr = try!(self.expr());
                try!(self.match_token(Token::RPAREN));
                Ok(expr)
            }
            _ => Err(self.error("Invalid expression"))
        }
    }

    // Token stream

    fn match_ident(&mut self) -> Result<String, ParseErr> {
        let t = self.next();
        match t {
            Token::IDENT(name) => Ok(name),
            _                  => Err(self.error("Expected identifier"))
        }
    }

    fn match_token(&mut self, q:Token) -> Result<bool, ParseErr> {
        let t = self.next();
        if t == q {
            Ok(true)
        } else {
            Err(self.error(&format!("Expected {:?}", q)))
        }
    }

    fn eat_token(&mut self, q:Token) -> bool {
        let t = self.next();
        if t == q {
            true
        } else {
            self.unget(t);
            false
        }
    }

    // TODO: The need to clone the token suggests a weakness in the token
    // definition - the String in IDENT should be replaced by something
    // copyable.

    fn peek(&mut self) -> Token {
        if self.ungotten {
            return self.ungot.clone();
        }
        let t = self.next();
        self.unget(t.clone());
        return t;
    }

    fn next(&mut self) -> Token {
        if self.ungotten {
            let res = self.ungot.clone();
            self.ungotten = false;
            return res;
        }
        return self.l.next();
    }

    fn unget(&mut self, t:Token) {
        self.ungotten = true;
        self.ungot = t;
    }

    // Misc

    fn token_to_type(&self, t:Token) -> Result<TypeName, ParseErr> {
        match t {
            Token::INT => Ok(TypeName::INT),
            Token::NUM => Ok(TypeName::NUM),
            _          => Err(self.error("expected type name"))
        }
    }

    fn error(&self, s:&str) -> ParseErr {
        ParseErr{ msg: format!("{}: {}", self.l.line(), s) }
    }
}
