use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Name
{
    N(u32),                     // Program name
    T(u32)                      // Temporary
}
        
pub struct NameTable
{
    mapping: HashMap<String, usize>,
    reverse: Vec<String>,
    counter: usize
}

impl NameTable
{
    pub fn new() -> NameTable {
        NameTable {
            mapping: HashMap::new(),
            reverse: Vec::new(),
            counter: 0
        }
    }

    pub fn gensym(&mut self, prefix:&String) -> Name {
        let mut name = String::from(".");
        name.push_str(prefix);
        name.push('_');
        let n = self.counter;
        self.counter += n;
        let counter = n.to_string();
        name.push_str(&counter);
        debug_assert!(!self.mapping.get(&name).is_some()); // FIXME: presumably a direct call for this
        return self.add(&name);
    }

    pub fn add(&mut self, name:&String) -> Name {
        // TODO:
        // debug_assert!(name[0] != '.');

        // Rust #fail - this should have been a simple match operation.

        {
            let probe = self.mapping.get(name);
            if probe.is_some() {
                return Name::N(*probe.unwrap() as u32);
            }
        }

        let x = self.reverse.len();
        self.reverse.push(name.clone());
        self.mapping.insert(name.clone(), x);
        return Name::N(x as u32);
    }

    pub fn lookup(&self, id:Name) -> String {
        match id {
            Name::N(id) => self.reverse[id as usize].clone(),
            Name::T(id) => {
                let mut name = String::from(".TMP_");
                name.push_str(&id.to_string());
                name
            }
        }
    }
}
