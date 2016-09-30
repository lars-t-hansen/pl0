use std::char;
use std::fs::File;
use std::io::{Read, Bytes};

pub struct Lex
{
    lineno: u32,
    ungotten: bool,
    ungot: char,
    input: Bytes<File>
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token
{
    SEMI,
    COMMA,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    ARROW,
    ASSIGN,
    PLUS,
    MINUS,
    TIMES,
    DIVIDE,
    REMAINDER,
    EQUALS,
    NOTEQUALS,
    LESS,
    LESSEQ,
    GREATER,
    GREATEREQ,
    AND,
    OR,
    NOT,
    INTLIT(i64),
    NUMLIT(f64),
    IDENT(String),
    ELSE,
    FN,
    INT,
    IF,
    NUM,
    RETURN,
    WHILE,

    ERROR(u32),                 // Carries line number
    EOI
}

const EOICHAR : char = '\0';

impl Lex
{
    pub fn new(input: File) -> Lex {
        Lex {
            lineno: 1,
            ungotten: false,
            ungot: ' ',
            input: input.bytes()
        }
    }

    pub fn line(&self) -> u32 {
        self.lineno
    }

    pub fn next(&mut self) -> Token {
        let c = self.get();
        match c {
            EOICHAR => Token::EOI,
            ' ' | '\t' | '\r' => self.next(),
            '\n' => { self.lineno += 1; self.next() }
            ';' => Token::SEMI,
            ',' => Token::COMMA,
            '(' => Token::LPAREN,
            '{' => Token::LBRACE,
            ')' => Token::RPAREN,
            '}' => Token::RBRACE,
            '+' => Token::PLUS,
            '-' => { if self.eat('>') { Token::ARROW } else { Token::MINUS } }
            '*' => Token::TIMES,
            '/' => { if self.eat('/') { self.comment(); self.next() } else { Token::DIVIDE } }
            '%' => Token::REMAINDER,
            '=' => { if self.eat('=') { Token::EQUALS } else { Token::ASSIGN } }
            '<' => { if self.eat('=') { Token::LESSEQ } else { Token::LESS } }
            '>' => { if self.eat('=') { Token::GREATEREQ } else { Token::GREATER } }
            '&' => { if self.eat('&') { Token::AND } else { self.bad() } }
            '|' => { if self.eat('|') { Token::OR } else { self.bad() } }
            '!' => { if self.eat('=') { Token::NOTEQUALS } else { Token::NOT } }
            '0' | '1' | '2' | '3' | '4' |
            '5' | '6' | '7' | '8' | '9' | '.' => self.num_literal(c),
            _ => {
                if is_initial(c) {
                    let s = self.name(c);
                    match s.as_str() {
                        "else"   => Token::ELSE,
                        "fn"     => Token::FN,
                        "int"    => Token::INT,
                        "if"     => Token::IF,
                        "num"    => Token::NUM,
                        "return" => Token::RETURN,
                        "while"  => Token::WHILE,
                        _        => Token::IDENT(s)
                    }
                } else {
                    self.bad()
                }
            }
        }
    }

    fn num_literal(&mut self, mut c: char) -> Token {
        let mut is_num = false;
        let mut s = String::new();
        while c >= '0' && c <= '9' {
            s.push(c);
            c = self.get();
        }
        if c == '.' {
            is_num = true;
            s.push(c);
            c = self.get();
            while c >= '0' && c <= '9' {
                s.push(c);
                c = self.get();
            }
        }
        if c == 'e' || c == 'E' {
            is_num = true;
            s.push(c);
            c = self.get();
            if c == '+' || c == '-' {
                s.push(c);
                c = self.get();
            }
            while c >= '0' && c <= '9' {
                s.push(c);
                c = self.get();
            }
        }
        self.unget(c);
        // FIXME: handle the errors
        if is_num {
            Token::NUMLIT(s.parse::<f64>().unwrap())
        } else {
            Token::INTLIT(s.parse::<i64>().unwrap())
        }
    }

    fn name(&mut self, c: char) -> String {
        let mut s = String::from("");
        s.push(c);
        while is_subsequent(self.peek()) {
            s.push(self.get());
        }
        return s;
    }

    fn comment(&mut self) {
        loop {
            match self.peek() {
                EOICHAR | '\n' => { break; }
                _              => { self.get(); }
            }
        }
    }

    fn bad(&mut self) -> Token {
        Token::ERROR(self.lineno)
    }
    
    fn eat(&mut self, c: char) -> bool {
        if self.peek() == c {
            self.get();
            true
        } else {
            false
        }
    }

    fn unget(&mut self, c: char) {
        self.ungotten = true;
        self.ungot = c;
    }

    fn peek(&mut self) -> char {
        if self.ungotten {
            self.ungot
        } else {
            let c = self.get();
            self.unget(c);
            c
        }
    }

    fn get(&mut self) -> char {
        if self.ungotten {
            self.ungotten = false;
            self.ungot
        } else {
            match self.input.next() {
                None => EOICHAR,
                Some(Ok(c)) => char::from_u32(c as u32).unwrap(),
                Some(Err(_)) => panic!("Input error")
            }
        }
    }
}

fn is_initial(c: char) -> bool {
    c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_'
}

fn is_subsequent(c: char) -> bool {
    is_initial(c) || c >= '0' && c <= '9'
}

// TODO: test code

    // loop {
    //     let t = l.next();
    //     println!("Token: {:?}", t);
    //     if t == Token::EOI {
    //         break;
    //     }
    // }

