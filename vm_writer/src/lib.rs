use anyhow::Result;
use std::{
    io::Write,
    sync::{Arc, Mutex},
};

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

pub enum Command {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}

pub struct VMWriter {
    writer: Arc<Mutex<dyn Write>>,
}

impl VMWriter {
    pub fn new(writer: Arc<Mutex<dyn Write>>) -> Self {
        Self { writer }
    }

    pub fn write_push(&mut self, segment: Segment, index: u16) -> Result<()> {
        Ok(())
    }

    pub fn write_pop(&mut self, segment: Segment, index: u16) -> Result<()> {
        Ok(())
    }

    pub fn write_arithmetic(&mut self, command: Command) -> Result<()> {
        Ok(())
    }

    pub fn write_label(&mut self, label: &str) -> Result<()> {
        Ok(())
    }

    pub fn write_goto(&mut self, label: &str) -> Result<()> {
        Ok(())
    }

    pub fn write_if(&mut self, label: &str) -> Result<()> {
        Ok(())
    }

    pub fn write_call(&mut self, name: &str, n_args: u16) -> Result<()> {
        Ok(())
    }

    pub fn write_function(&mut self, name: &str, n_args: u16) -> Result<()> {
        Ok(())
    }

    pub fn write_return(&mut self) -> Result<()> {
        Ok(())
    }

    pub fn close(&mut self) -> Result<()> {
        self.writer.lock().unwrap().flush()?;
        drop(self.writer.lock().unwrap());
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
