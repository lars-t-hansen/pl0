use std::char;
use std::fs::File;
use std::io::{Read, Bytes};

use names::{Name, NameTable};

pub struct Lex<'a>
{
    lineno: u32,
    ungotten: bool,
    ungot: char,
    input: Bytes<File>,
    names: &'a mut NameTable
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Token
{
    Semi,
    Comma,
    LParen,
    RParen,
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
    IDENT(Name),
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

impl<'a> Lex<'a>
{
    pub fn new(names:&'a mut NameTable, input: File) -> Lex<'a> {
        Lex {
            lineno: 1,
            ungotten: false,
            ungot: ' ',
            input: input.bytes(),
            names: names
        }
    }

    pub fn line(&self) -> u32 {
        self.lineno
    }

    pub fn next(&mut self) -> Token {
        match self.get() {
            EOICHAR => Token::EOI,
            ' ' | '\t' | '\r' => self.next(),
            '\n' => { self.lineno += 1; self.next() }
            ';' => Token::Semi,
            ',' => Token::Comma,
            '(' => Token::LParen,
            '{' => Token::LBRACE,
            ')' => Token::RParen,
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
            c @ '0' ... '9' | c @ '.' => self.num_literal(c),
            c @ _ => {
                if is_initial(c) {
                    // OPTIMIZEME: here we should use a common buffer that's held in
                    // the lexer for accumulating the string's chars.  Then we should probe
                    // the name table to see if the name is already known.  Only when
                    // the name is already known should we clone the string and pass
                    // ownership of it to the name table.
                    let s = self.name(c);
                    match s.as_str() {
                        "else"   => Token::ELSE,
                        "fn"     => Token::FN,
                        "int"    => Token::INT,
                        "if"     => Token::IF,
                        "num"    => Token::NUM,
                        "return" => Token::RETURN,
                        "while"  => Token::WHILE,
                        _        => Token::IDENT(self.names.add(&s))
                    }
                } else {
                    self.bad()
                }
            }
        }
    }

    fn num_literal(&mut self, mut c: char) -> Token {
        let mut is_num = false;
        // OPTIMIZEME: here we should use a common buffer that's held in the lexer
        // for accumulating the number's chars.
        let mut s = String::new();
        while is_digit(c) {
            s.push(c);
            c = self.get();
        }
        if c == '.' {
            is_num = true;
            s.push(c);
            c = self.get();
            while is_digit(c) {
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
            while is_digit(c) {
                s.push(c);
                c = self.get();
            }
        }
        self.unget(c);
        if is_num {
            if let Ok(x) = s.parse::<f64>() { Token::NUMLIT(x) } else { self.bad() }
        } else {
            if let Ok(x) = s.parse::<i64>() { Token::INTLIT(x) } else { self.bad() }
        }
    }

    fn name(&mut self, c: char) -> String {
        let mut s = String::new();
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
    match c {
        'a' ... 'z' | 'A' ... 'Z' | '_' => true,
        _ => false
    }
}

fn is_digit(c: char) -> bool {
    match c {
        '0' ... '9' => true,
        _ => false
    }
}

fn is_subsequent(c: char) -> bool {
    match c {
        'a' ... 'z' | 'A' ... 'Z' | '_' | '0' ... '9' => true,
        _ => false
    }
}

// TODO: test code

    // loop {
    //     let t = l.next();
    //     println!("Token: {:?}", t);
    //     if t == Token::EOI {
    //         break;
    //     }
    // }

