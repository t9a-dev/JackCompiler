use anyhow::{anyhow, Result};
use strum_macros::{AsRefStr, EnumString};

#[derive(Debug, Clone, Copy, PartialEq, EnumString, AsRefStr)]
#[strum(ascii_case_insensitive)]
pub enum Kind {
    Static,
    Field,
    Arg,
    Var,
}

#[derive(Debug)]
pub struct SymbolTable {
    pub entries: Vec<Entrie>,
}

#[derive(Debug, PartialEq)]
pub struct Entrie {
    pub name: String,
    pub r#type: String,
    pub kind: Kind,
    pub index: u16,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn reset(&mut self) {
        self.entries = Vec::new();
    }

    pub fn define(&mut self, name: &str, r#type: &str, kind: Kind) {
        self.entries.push(Entrie {
            name: name.to_string(),
            r#type: r#type.to_string(),
            kind,
            index: self.var_count(kind),
        });
    }

    fn var_count(&self, kind: Kind) -> u16 {
        self.entries
            .iter()
            .filter(|entrie| entrie.kind == kind)
            .count()
            .try_into()
            .unwrap()
    }

    pub fn kind_of(&self, name: &str) -> Option<Kind> {
        match self.entries.iter().find(|entrie| entrie.name == name) {
            Some(entrie) => Some(entrie.kind),
            None => None,
        }
    }

    pub fn type_of(&self, name: &str) -> Result<String> {
        match self.entries.iter().find(|entrie| entrie.name == name) {
            Some(entrie) => Ok(entrie.r#type.to_string()),
            None => Err(anyhow!("not found entrie name: {}", name)),
        }
    }

    pub fn index_of(&self, name: &str) -> Result<u16> {
        match self.entries.iter().find(|entrie| entrie.name == name) {
            Some(entrie) => Ok(entrie.index),
            None => Err(anyhow!("not found entrie name: {}", name)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_define() -> Result<()> {
        let mut symbol_table = SymbolTable::new();
        symbol_table.define("a", "int", Kind::Var);
        symbol_table.define("a", "int", Kind::Var);
        symbol_table.define("b", "int", Kind::Field);
        symbol_table.define("a", "int", Kind::Arg);

        assert_eq!(
            symbol_table.entries.get(0).unwrap(),
            &Entrie {
                name: "a".to_string(),
                r#type: "int".to_string(),
                kind: Kind::Var,
                index: 0
            }
        );
        assert_eq!(
            symbol_table.entries.get(1).unwrap(),
            &Entrie {
                name: "a".to_string(),
                r#type: "int".to_string(),
                kind: Kind::Var,
                index: 1
            }
        );
        assert_eq!(
            symbol_table.entries.get(2).unwrap(),
            &Entrie {
                name: "b".to_string(),
                r#type: "int".to_string(),
                kind: Kind::Field,
                index: 0
            }
        );
        assert_eq!(
            symbol_table.entries.get(3).unwrap(),
            &Entrie {
                name: "a".to_string(),
                r#type: "int".to_string(),
                kind: Kind::Arg,
                index: 0
            }
        );
        Ok(())
    }

    #[test]
    fn test_symbol_table_kind_of() -> Result<()> {
        let mut symbol_table = SymbolTable::new();
        symbol_table.define("a", "int", Kind::Var);
        symbol_table.define("a", "int", Kind::Var);
        symbol_table.define("b", "int", Kind::Field);
        symbol_table.define("a", "int", Kind::Arg);

        assert_eq!(symbol_table.kind_of("a"), Some(Kind::Var));
        assert_eq!(symbol_table.kind_of("b"), Some(Kind::Field));
        Ok(())
    }

    #[test]
    fn test_symbol_table_type_of() -> Result<()> {
        let mut symbol_table = SymbolTable::new();
        symbol_table.define("a", "int", Kind::Var);
        symbol_table.define("a", "int", Kind::Var);
        symbol_table.define("b", "char", Kind::Field);
        symbol_table.define("a", "int", Kind::Arg);

        assert_eq!(symbol_table.type_of("a")?, "int".to_string());
        assert_eq!(symbol_table.type_of("b")?, "char".to_string());
        assert_eq!(
            symbol_table.type_of("miu").unwrap_err().to_string(),
            "not found entrie name: miu".to_string()
        );
        Ok(())
    }

    #[test]
    fn test_symbol_table_index_of() -> Result<()> {
        let mut symbol_table = SymbolTable::new();
        symbol_table.define("a", "int", Kind::Var);
        symbol_table.define("a", "int", Kind::Var);
        symbol_table.define("b", "char", Kind::Field);
        symbol_table.define("a", "int", Kind::Arg);

        assert_eq!(symbol_table.index_of("a")?, 0);
        assert_eq!(symbol_table.index_of("b")?, 0);
        assert_eq!(
            symbol_table.index_of("miu").unwrap_err().to_string(),
            "not found entrie name: miu".to_string()
        );
        Ok(())
    }
}
