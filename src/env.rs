use std::collections::HashMap;

use names::Name;

// An environment maps a name to its denotation,
// The form of the denotation is context-dependent, hence a parameter.

// A GlobalEnv is used for globals and externs.

pub struct GlobalEnv<Denotation>
{
    globals: HashMap<Name, Denotation>,
    externs: HashMap<Name, Denotation>
}

impl<Denotation: Clone> GlobalEnv<Denotation>
{
    pub fn new() -> GlobalEnv<Denotation> {
        GlobalEnv {
            globals: HashMap::new(),
            externs: HashMap::new()
        }
    }
    
    pub fn lookup(&self, name:&Name) -> Option<Denotation> {
        match self.globals.get(name) {
            Some(d) => Some(d.clone()),
            None => match self.externs.get(name) {
                Some(d) => Some(d.clone()),
                None => None
            }
        }
    }

    pub fn add_extern(&mut self, name:&Name, b:Denotation) {
        self.externs.insert(*name, b);
    }

    pub fn add_global(&mut self, name:&Name, b:Denotation) -> bool {
        match self.globals.get(name) {
            Some(_) => {
                false
            }
            None => {
                self.globals.insert(*name, b);
                true
            }
        }
    }
}


// A LocalEnv is used for formals and locals within blocks.

pub struct LocalEnv<Denotation>
{
    locals: Vec<Vec<(Name, Denotation)>>
}

impl<Denotation: Clone> LocalEnv<Denotation>
{
    pub fn new() -> LocalEnv<Denotation> {
        LocalEnv {
            locals: Vec::<Vec<(Name, Denotation)>>::new(),
        }
    }
    
    pub fn lookup(&self, name:&Name) -> Option<Denotation> {
        match self.lookup_(name) {
            Some((denotation, _)) => Some(denotation),
            None => None
        }
    }

    pub fn add(&mut self, name:&Name, denotation:Denotation) -> bool {
        match self.lookup_(name) {
            Some((_, level)) => {
                if level == 0 {
                    return false;
                }
            }
            None => {}
        }
        self.locals[0].push((*name, denotation));
        return true;
    }

    fn lookup_(&self, name:&Name) -> Option<(Denotation, i32)> {
        let mut r = self.locals.len()-1;
        let mut level = 0;
        while r > 0 {
            let rib = &self.locals[r-1];
            let mut i = 0;
            while i < rib.len() {
                let (ref denotation_name, ref denotation) = rib[i];
                if denotation_name == name {
                    return Some((denotation.clone(), level));
                }
                i = i+1;
            }
            r = r-1;
            level = level + 1;
        }
        return None;
    }

    pub fn push(&mut self) {
        self.locals.push(Vec::<(Name,Denotation)>::new());
    }

    pub fn pop(&mut self) {
        self.locals.pop();
    }        
}

