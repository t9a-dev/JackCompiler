use anyhow::Result;
use std::{
    io::Write,
    sync::{Arc, Mutex},
};
use strum_macros::{AsRefStr, EnumString};
use symbol_table::Kind;

#[derive(Debug, Clone, PartialEq, AsRefStr, EnumString)]
#[strum(ascii_case_insensitive)]
pub enum Segment {
    Constant,
    Argument,
    Local,
    Static,
    This,
    That,
    Pointer,
    Temp,
}

impl From<Kind> for Segment {
    fn from(value: Kind) -> Self {
        match value {
            symbol_table::Kind::Static => Self::Static,
            symbol_table::Kind::Field => Self::This,
            symbol_table::Kind::Arg => Self::Argument,
            symbol_table::Kind::Var => Self::Local,
        }
    }
}

#[derive(Debug, Clone, PartialEq, AsRefStr, EnumString)]
#[strum(ascii_case_insensitive)]
pub enum ArithmeticCommand {
    #[strum(serialize = "+", to_string = "add")]
    Add,
    #[strum(serialize = "-", to_string = "sub")]
    Sub,
    #[strum(serialize = "-", to_string = "neg")]
    Neg,
    #[strum(serialize = "=", to_string = "eq")]
    Eq,
    #[strum(serialize = ">", to_string = "gt")]
    Gt,
    #[strum(serialize = "<", to_string = "lt")]
    Lt,
    #[strum(serialize = "&", to_string = "and")]
    And,
    #[strum(serialize = "|", to_string = "or")]
    Or,
    #[strum(serialize = "~", to_string = "not")]
    Not,
    #[strum(serialize = "*", to_string = "call Math.multiply 2")]
    Multiply,
}

pub struct VMWriter {
    writer: Arc<Mutex<dyn Write>>,
}

impl VMWriter {
    pub fn new(writer: Arc<Mutex<dyn Write>>) -> Self {
        Self { writer }
    }

    pub fn write_push(&mut self, segment: Segment, index: i16) -> Result<()> {
        let segment = segment.as_ref().to_lowercase();
        self.write_code(&format!("push {segment} {index}\n"))?;
        Ok(())
    }

    pub fn write_pop(&mut self, segment: Segment, index: i16) -> Result<()> {
        let segment = segment.as_ref().to_lowercase();
        self.write_code(&format!("pop {segment} {index}\n"))?;
        Ok(())
    }

    pub fn write_arithmetic(&mut self, command: ArithmeticCommand) -> Result<()> {
        let command = command.as_ref();
        self.write_code(&format!("{command}\n"))?;
        Ok(())
    }

    pub fn write_label(&mut self, label: &str) -> Result<()> {
        self.write_code(&format!("label {label}\n"))?;
        Ok(())
    }

    pub fn write_goto(&mut self, label: &str) -> Result<()> {
        self.write_code(&format!("goto {label}\n"))?;
        Ok(())
    }

    pub fn write_if(&mut self, label: &str) -> Result<()> {
        self.write_code(&format!("if-goto {label}\n"))?;
        Ok(())
    }

    pub fn write_call(&mut self, name: &str, n_args: u16) -> Result<()> {
        self.write_code(&format!("call {name} {n_args}\n"))?;
        Ok(())
    }

    pub fn write_function(&mut self, name: &str, n_args: u16) -> Result<()> {
        self.write_code(&format!("function {name} {n_args}\n"))?;
        Ok(())
    }

    pub fn write_return(&mut self) -> Result<()> {
        self.write_code("return\n\n")?;
        Ok(())
    }

    pub fn close(&mut self) -> Result<()> {
        self.writer.lock().unwrap().flush()?;
        drop(self.writer.lock().unwrap());
        Ok(())
    }

    fn write_code(&mut self, content: &str) -> Result<()> {
        self.writer.lock().unwrap().write(content.as_bytes())?;
        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() -> Result<()> {
        Ok(())
    }
}
