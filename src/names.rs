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
    reverse: Vec<String>
}

impl NameTable
{
    pub fn new() -> NameTable {
        NameTable {
            mapping: HashMap::new(),
            reverse: Vec::new()
        }
    }

    pub fn add(&mut self, name:&String) -> Name {
        if let Some(x) = self.mapping.get(name) {
            return Name::N(*x as u32);
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
                let mut name = ".TMP_".to_string();
                name.push_str(&id.to_string());
                name
            }
        }
    }
}
