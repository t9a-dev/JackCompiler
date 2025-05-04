use anyhow::{anyhow, Ok, Result};
use jack_tokenizer::{JackTokenizer, KeyWord, TokenType};
use std::{
    io::Write,
    str::FromStr,
    sync::{Arc, Mutex},
};
use strum_macros::{AsRefStr, EnumString};
use symbol_table::{Kind, SymbolTable};
use vm_writer::{ArithmeticCommand, Segment, VMWriter};

#[derive(Debug, Clone, PartialEq, AsRefStr, EnumString)]
#[strum(ascii_case_insensitive)]
pub enum Category {
    Field,
    Static,
    Var,
    Arg,
    Class,
    Subroutine,
    IntConst,
    StringConst,
}

#[derive(Debug, Clone, PartialEq, AsRefStr)]
pub enum Usage {
    Declare,
    Use,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionNode {
    pub term: String,
    pub usage: Usage,
    pub category: Option<Category>,
    pub arithmetic_cmd: Option<ArithmeticCommand>,
}

impl ExpressionNode {
    pub fn new(
        term: &str,
        usage: Usage,
        category: Option<Category>,
        command: Option<ArithmeticCommand>,
    ) -> Self {
        Self {
            term: term.to_string(),
            usage,
            category,
            arithmetic_cmd: command,
        }
    }
}

pub struct CompilationEngine {
    tokenizer: JackTokenizer,
    xml_writer: Arc<Mutex<dyn Write>>,
    vm_writer: VMWriter,
    class_symbol_table: SymbolTable,
    subroutine_symbol_table: SymbolTable,
    pub expressions: Vec<ExpressionNode>,
}

impl CompilationEngine {
    pub fn new(
        tokenizer: JackTokenizer,
        xml_writer: Arc<Mutex<dyn Write>>,
        vm_writer: VMWriter,
        class_symbol_table: SymbolTable,
        subroutine_symbol_table: SymbolTable,
    ) -> Result<Self> {
        Ok(Self {
            tokenizer,
            xml_writer,
            vm_writer,
            class_symbol_table,
            subroutine_symbol_table,
            expressions: Vec::new(),
        })
    }

    pub fn compile_class(&mut self) -> Result<()> {
        let tag_name = "class";
        self.tokenizer.advance()?;
        self.write_start_xml_tag(tag_name)?;
        self.process_token("class")?;
        self.process_identifier(Category::Class, Usage::Declare)?;
        self.process_token("{")?;
        self.compile_class_var_dec()?;
        self.compile_subroutine()?;
        self.process_token("}")?;
        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_class_var_dec(&mut self) -> Result<()> {
        // "static"|"field"
        if self.tokenizer.token_type()? == TokenType::KeyWord
            && matches!(
                self.tokenizer
                    .keyword()?
                    .as_ref()
                    .to_string()
                    .to_lowercase()
                    .as_str(),
                "static" | "field"
            )
        {
            let tag_name = "classVarDec";
            self.write_start_xml_tag(tag_name)?;

            let symbol_entrie_kind = self
                .process_token("static")
                .or_else(|_| Ok(self.process_token("field")?))?;
            // type -> "int"|"char"|"boolean"|className
            let symbol_entrie_type = self.process_type()?;
            self.class_symbol_table.define(
                self.tokenizer.identifer()?.as_str(),
                &symbol_entrie_type,
                Kind::from_str(&symbol_entrie_kind)?,
            );
            self.process_identifier(Category::from_str(&symbol_entrie_kind)?, Usage::Declare)?;
            // 現在のトークンが","であれば複数varNameが存在するので対応する
            while self.tokenizer.token_type()? == TokenType::Symbol
                && self.tokenizer.symbol()? == ","
            {
                self.process_token(",")?;
                self.class_symbol_table.define(
                    self.tokenizer.identifer()?.as_str(),
                    &symbol_entrie_type,
                    Kind::from_str(&symbol_entrie_kind)?,
                );
                self.process_identifier(Category::from_str(&symbol_entrie_kind)?, Usage::Declare)?;
            }
            self.process_token(";")?;
            self.write_end_xml_tag(tag_name)?;
            // classVarDecが複数存在する場合
            if self.tokenizer.token_type()? == TokenType::KeyWord
                && matches!(
                    self.tokenizer
                        .keyword()?
                        .as_ref()
                        .to_string()
                        .to_lowercase()
                        .as_str(),
                    "static" | "field"
                )
            {
                self.compile_class_var_dec()?;
            }
        }

        Ok(())
    }

    pub fn compile_subroutine(&mut self) -> Result<()> {
        // "constructor"|"function"|"method"
        if self.tokenizer.token_type()? == TokenType::KeyWord
            && matches!(
                self.tokenizer.keyword()?.as_ref().to_lowercase().as_str(),
                "constructor" | "function" | "method"
            )
        {
            let tag_name = "subroutineDec";
            self.write_start_xml_tag(tag_name)?;

            let is_method = self.process_token("constructor").or_else(|_| {
                self.process_token("function")
                    .or_else(|_| Ok(self.process_token("method")?))
            })? == "method";
            // "void"|type
            self.process_token("void")
                .or_else(|_| Ok(self.process_type()?))?;
            let symbol_entrie_name =
                self.process_identifier(Category::Subroutine, Usage::Declare)?;
            self.subroutine_symbol_table.reset(); //　仕様によりサブルーチンコンパイル開始時に初期化する
                                                  // 仕様によりメソッドの場合はthisをシンボルテーブルに追加する
            if is_method {
                self.subroutine_symbol_table
                    .define("this", &symbol_entrie_name, Kind::Arg);
            }
            self.process_token("(")?;
            self.compile_parameter_list()?;
            self.process_token(")")?;
            self.compile_subroutine_body()?;
            self.write_end_xml_tag(tag_name)?;

            // subroutineDecが複数存在する場合
            if self.tokenizer.token_type()? == TokenType::KeyWord
                && matches!(
                    self.tokenizer.keyword()?.as_ref().to_lowercase().as_str(),
                    "constructor" | "function" | "method"
                )
            {
                self.compile_subroutine()?;
            }
        }

        Ok(())
    }

    pub fn compile_parameter_list(&mut self) -> Result<()> {
        let tag_name = "parameterList";
        self.write_start_xml_tag(tag_name)?;

        if matches!(
            self.tokenizer.token_type()?,
            TokenType::KeyWord | TokenType::Identifier
        ) && matches!(
            self.tokenizer.keyword()?.as_ref().to_lowercase().as_str(),
            "int" | "char" | "boolean"
        ) {
            let symbol_entrie_kind = Kind::Arg; // サブルーチンのパラメータリストコンパイルなのでArg固定
            let symbol_entrie_type = self.process_type()?;
            self.subroutine_symbol_table.define(
                self.tokenizer.identifer()?.as_str(),
                &symbol_entrie_type,
                symbol_entrie_kind,
            );
            self.process_identifier(Category::Arg, Usage::Declare)?;

            // 現在のトークン","であれば複数varNameが存在するので対応する
            while self.tokenizer.token_type()? == TokenType::Symbol
                && self.tokenizer.symbol()? == ","
            {
                self.process_token(",")?;
                let symbol_entrie_type = self.process_type()?;
                self.subroutine_symbol_table.define(
                    self.tokenizer.identifer()?.as_str(),
                    &symbol_entrie_type,
                    symbol_entrie_kind,
                );
                self.process_identifier(Category::Arg, Usage::Declare)?;
            }
        }

        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_subroutine_body(&mut self) -> Result<()> {
        let tag_name = "subroutineBody";
        self.write_start_xml_tag(tag_name)?;

        self.process_token("{")?;
        while self.tokenizer.token_type()? == TokenType::KeyWord
            && self.tokenizer.keyword()?.as_ref().to_lowercase().as_str() == "var"
        {
            self.compile_var_dec()?;
        }
        self.compile_statements()?;
        self.process_token("}")?;

        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_var_dec(&mut self) -> Result<()> {
        let tag_name = "varDec";
        self.write_start_xml_tag(tag_name)?;

        let symbol_entrie_kind = Kind::Var; // サブルーチンボディのコンパイルなのでVar固定
        self.process_token("var")?;
        let symbol_entrie_type = self.process_type()?;
        self.subroutine_symbol_table.define(
            self.tokenizer.identifer()?.as_str(),
            &symbol_entrie_type,
            symbol_entrie_kind,
        );
        self.process_identifier(Category::Var, Usage::Declare)?;
        // 現在のトークンが","であれば複数varNameが存在するので対応する
        while self.tokenizer.token_type()? == TokenType::Symbol && self.tokenizer.symbol()? == "," {
            let symbol_entrie_type = symbol_entrie_type.as_str();
            self.process_token(",")?;
            self.subroutine_symbol_table.define(
                self.tokenizer.identifer()?.as_str(),
                &symbol_entrie_type,
                symbol_entrie_kind,
            );
            self.process_identifier(Category::Var, Usage::Declare)?;
        }
        self.process_token(";")?;

        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_statements(&mut self) -> Result<()> {
        let tag_name = "statements";
        self.write_start_xml_tag(tag_name)?;
        if self.tokenizer.token_type()? == TokenType::KeyWord {
            while self.tokenizer.token_type()? == TokenType::KeyWord
                && matches!(
                    self.tokenizer.keyword()?.as_ref().to_lowercase().as_str(),
                    "let" | "if" | "while" | "do" | "return"
                )
            {
                match self.tokenizer.keyword()? {
                    jack_tokenizer::KeyWord::Let => self.compile_let()?,
                    jack_tokenizer::KeyWord::If => self.compile_if()?,
                    jack_tokenizer::KeyWord::While => self.compile_while()?,
                    jack_tokenizer::KeyWord::Do => self.compile_do()?,
                    jack_tokenizer::KeyWord::Return => self.compile_return()?,
                    _ => (),
                }
            }
        }
        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_let(&mut self) -> Result<()> {
        let tag_name = "letStatement";
        self.write_start_xml_tag(tag_name)?;

        self.process_token("let")?;
        self.process_identifier(Category::Var, Usage::Declare)?;
        if self.tokenizer.token_type()? == TokenType::Symbol && self.tokenizer.symbol()? == "[" {
            self.process_token("[")?;
            self.clear_expressions();
            self.compile_expression()?;
            self.process_token("]")?;
        }
        self.process_token("=")?;
        self.clear_expressions();
        self.compile_expression()?;
        self.process_token(";")?;

        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_if(&mut self) -> Result<()> {
        let tag_name = "ifStatement";
        self.write_start_xml_tag(tag_name)?;

        self.process_token("if")?;
        self.process_token("(")?;
        self.clear_expressions();
        self.compile_expression()?;
        self.process_token(")")?;
        self.process_token("{")?;
        self.compile_statements()?;
        self.process_token("}")?;
        if self.tokenizer.token_type()? == TokenType::KeyWord
            && self.tokenizer.keyword()? == KeyWord::Else
        {
            self.process_token("else")?;
            self.process_token("{")?;
            self.compile_statements()?;
            self.process_token("}")?;
        }

        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_while(&mut self) -> Result<()> {
        let tag_name = "whileStatement";
        self.write_start_xml_tag(tag_name)?;

        self.process_token("while")?;
        self.process_token("(")?;
        self.clear_expressions();
        self.compile_expression()?;
        self.process_token(")")?;
        self.process_token("{")?;
        self.compile_statements()?;
        self.process_token("}")?;

        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_do(&mut self) -> Result<()> {
        let tag_name = "doStatement";
        self.write_start_xml_tag(tag_name)?;

        self.process_token("do")?;
        // subroutine call
        self.clear_expressions();
        self.compile_expression()?;
        self.process_token(";")?;

        self.write_end_xml_tag(tag_name)?;
        self.write_vm_code()?;
        self.clear_expressions();
        Ok(())
    }

    pub fn compile_return(&mut self) -> Result<()> {
        let tag_name = "returnStatement";
        self.write_start_xml_tag(tag_name)?;

        self.process_token("return")?;
        // expression
        if self.has_expression()? {
            self.clear_expressions();
            self.compile_expression()?;
        }
        self.process_token(";")?;

        self.write_end_xml_tag(tag_name)?;
        self.vm_writer.write_return()?;
        self.clear_expressions();
        Ok(())
    }

    pub fn compile_expression(&mut self) -> Result<()> {
        let tag_name = "expression";
        self.write_start_xml_tag(tag_name)?;

        self.compile_term()?;
        while self.tokenizer.token_type()? == TokenType::Symbol
            && matches!(
                self.tokenizer.symbol()?.as_str(),
                "+" | "-" | "*" | "/" | "&" | "|" | "<" | ">" | "="
            )
        {
            let symbol = self.tokenizer.symbol()?;
            let term = self.process_token(&symbol)?;
            self.compile_term()?;
            let expression_node = ExpressionNode {
                term: term,
                usage: Usage::Use,
                category: None,
                arithmetic_cmd: Some(
                    ArithmeticCommand::from_str(&symbol)
                        .expect(&format!("strum from_str not found: {:?}", symbol)),
                ),
            };
            self.add_expression(expression_node)?;
        }

        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_term(&mut self) -> Result<()> {
        let tag_name = "term";
        self.write_start_xml_tag(tag_name)?;

        match self.tokenizer.token_type()? {
            TokenType::KeyWord => {
                self.process_token(self.tokenizer.keyword()?.as_ref().to_lowercase().as_str())?;
            }
            TokenType::Symbol => {
                let symbol = self.tokenizer.symbol()?;
                if symbol == "(" {
                    self.process_token("(")?;
                    self.compile_expression()?;
                    self.process_token(")")?;
                } else {
                    // Sub,Negはシンボルが"-"で重複しているので調整
                    let arithmetic_cmd = match symbol.as_str() {
                        "-" => ArithmeticCommand::Neg,
                        _ => ArithmeticCommand::from_str(&symbol)
                            .expect(&format!("strum from_str err: {:?}", symbol)),
                    };
                    let expression_node = ExpressionNode {
                        term: self.process_token(&symbol)?,
                        usage: Usage::Use,
                        category: None,
                        arithmetic_cmd: Some(arithmetic_cmd),
                    };
                    self.compile_term()?;
                    self.add_expression(expression_node)?;
                }
            }
            TokenType::Identifier => {
                // subroutine_symbol_tableにidentifierの登録がなければclassNameとして扱う
                let identifier_category = match self
                    .subroutine_symbol_table
                    .kind_of(self.tokenizer.identifer()?.as_str())
                {
                    Some(_) => Category::Arg,
                    None => {
                        match self
                            .class_symbol_table
                            .kind_of(self.tokenizer.identifer()?.as_str())
                        {
                            Some(kind) => Category::from_str(kind.as_ref())?,
                            None => Category::Class,
                        }
                    }
                };
                let identifier_name =
                    self.process_identifier(identifier_category.clone(), Usage::Use)?;
                if self.tokenizer.token_type()? == TokenType::Symbol {
                    match self.tokenizer.symbol()?.as_str() {
                        "[" => {
                            self.process_token("[")?;
                            self.compile_expression()?;
                            self.process_token("]")?;
                        }
                        "(" => {
                            self.process_token("(")?;
                            self.compile_expression_list()?;
                            self.process_token(")")?;
                        }
                        "." => {
                            self.process_token(".")?;
                            let identifier_name = identifier_name
                                + "."
                                + self
                                    .process_identifier(Category::Subroutine, Usage::Use)?
                                    .as_str();
                            self.process_token("(")?;
                            self.compile_expression_list()?;
                            self.process_token(")")?;
                            self.add_expression(ExpressionNode::new(
                                &identifier_name,
                                Usage::Use,
                                Some(identifier_category),
                                None,
                            ))?;
                        }
                        _ => {
                            self.add_expression(ExpressionNode::new(
                                &identifier_name,
                                Usage::Use,
                                Some(identifier_category),
                                None,
                            ))?;
                        }
                    }
                }
            }
            TokenType::IntConst => {
                let term = self.process_token(self.tokenizer.int_val()?.to_string().as_str())?;
                self.add_expression(ExpressionNode::new(
                    &term,
                    Usage::Use,
                    Some(Category::IntConst),
                    None,
                ))?;
            }
            TokenType::StringConst => {
                let term = self.process_token(self.tokenizer.string_val()?.as_str())?;
                self.add_expression(ExpressionNode::new(
                    &term,
                    Usage::Use,
                    Some(Category::StringConst),
                    None,
                ))?;
            }
        }

        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    pub fn compile_expression_list(&mut self) -> Result<()> {
        let tag_name = "expressionList";
        self.write_start_xml_tag(tag_name)?;
        if self.has_expression()? {
            self.compile_expression()?;
            while self.tokenizer.token_type()? == TokenType::Symbol
                && self.tokenizer.symbol()? == ","
            {
                self.process_token(",")?;
                self.compile_expression()?;
            }
        }
        self.write_end_xml_tag(tag_name)?;
        Ok(())
    }

    fn process_token(&mut self, token: &str) -> Result<String> {
        let current_token = match self.tokenizer.token_type()? {
            jack_tokenizer::TokenType::KeyWord => self
                .tokenizer
                .keyword()?
                .as_ref()
                .to_string()
                .as_str()
                .to_lowercase(),
            jack_tokenizer::TokenType::Symbol => self.tokenizer.symbol()?,
            jack_tokenizer::TokenType::Identifier => self.tokenizer.identifer()?,
            jack_tokenizer::TokenType::IntConst => self.tokenizer.int_val()?.to_string(),
            jack_tokenizer::TokenType::StringConst => self.tokenizer.string_val()?,
        };
        match self.tokenizer.token_type()? {
            TokenType::IntConst | TokenType::StringConst => {
                self.write_xml(
                    &self.tokenizer.token_type()?.as_ref().to_string(),
                    &current_token,
                )?;
            }
            _ => {
                if current_token == token.to_lowercase() {
                    self.write_xml(
                        &self
                            .tokenizer
                            .token_type()?
                            .as_ref()
                            .to_string()
                            .to_lowercase(),
                        &current_token,
                    )?;
                } else {
                    return Err(anyhow!(
                        "syntax error token: {:?}, current_token: {:?}",
                        token,
                        current_token
                    ));
                }
            }
        }

        self.tokenizer.advance()?;
        Ok(current_token.to_string())
    }

    fn process_identifier(&mut self, category: Category, usage: Usage) -> Result<String> {
        if self.tokenizer.token_type()? == TokenType::Identifier {
            let identifier_name = &self.tokenizer.identifer()?;
            self.write_identifier(&identifier_name, category, usage)?;
            self.tokenizer.advance()?;
            Ok(identifier_name.to_string())
        } else {
            return Err(anyhow!(
                "syntax error current token type is not identifier: {:?}, current token {}",
                self.tokenizer.token_type()?,
                self.tokenizer.current_token.clone().unwrap(),
            ));
        }
    }

    fn process_type(&mut self) -> Result<String> {
        Ok(self.process_token("int").or_else(|_| {
            self.process_token("char").or_else(|_| {
                self.process_token("boolean")
                    .or_else(|_| Ok(self.process_identifier(Category::Class, Usage::Use)?))
            })
        })?)
    }

    fn has_expression(&self) -> Result<bool> {
        match self.tokenizer.token_type()? {
            TokenType::KeyWord => Ok(matches!(
                self.tokenizer.keyword()?,
                KeyWord::True | KeyWord::False | KeyWord::Null | KeyWord::This
            )),
            TokenType::Symbol => Ok(matches!(self.tokenizer.symbol()?.as_str(), "(" | "~" | "-")),
            TokenType::Identifier => Ok(true),
            TokenType::IntConst => Ok(true),
            TokenType::StringConst => Ok(true),
        }
    }

    fn add_expression(&mut self, node: ExpressionNode) -> Result<()> {
        self.expressions.push(node);
        Ok(())
    }

    fn clear_expressions(&mut self) {
        self.expressions.clear();
    }

    fn write_start_xml_tag(&mut self, tag_name: &str) -> Result<()> {
        self.write(&format!("<{tag_name}>\n"))?;
        Ok(())
    }

    fn write_end_xml_tag(&mut self, tag_name: &str) -> Result<()> {
        self.write(&format!("</{tag_name}>\n"))?;
        Ok(())
    }

    fn write_xml(&mut self, tag_name: &str, content: &str) -> Result<()> {
        self.write(&format!("<{tag_name}>"))?;
        self.write(&format!(" {content} "))?;
        self.write(&format!("</{tag_name}>\n"))?;
        Ok(())
    }

    fn write_identifier(&mut self, name: &str, category: Category, usage: Usage) -> Result<()> {
        let tag_name = "identifier";
        let usage = usage.as_ref().to_string();
        match self
            .class_symbol_table
            .index_of(name)
            .or_else(|_| Ok(self.subroutine_symbol_table.index_of(name)?))
        {
            Result::Ok(index) => {
                self.write(&format!(
                    r#"<{tag_name} name="{name}" category="{}" index={index} usage="{usage}">"#,
                    category.as_ref().to_string()
                ))?;
            }
            Err(_) => self.write(&format!(
                r#"<{tag_name} name="{name}" category="{}" usage="{usage}">"#,
                category.as_ref().to_string()
            ))?,
        }

        self.write(&format!(" {name} "))?;
        self.write(&format!("</{tag_name}>\n"))?;
        Ok(())
    }

    fn write_vm_code(&mut self) -> Result<()> {
        let mut expressions = self.expressions.iter();
        while let Some(expression) = expressions.next() {
            match &expression.category {
                Some(category) => {
                    match category {
                        Category::IntConst => self
                            .vm_writer
                            .write_push(Segment::Constant, expression.term.parse::<u16>()?)?,
                        Category::Class | Category::Subroutine => {
                            // TODO: n_args use computed value
                            self.vm_writer.write_call(&expression.term, 1)?
                        }
                        Category::StringConst => todo!(),
                        _ => todo!(),
                    }
                }
                None => match &expression.arithmetic_cmd {
                    Some(cmd) => self.vm_writer.write_arithmetic(cmd.clone())?,
                    None => (),
                },
            }
        }
        Ok(())
    }

    fn write(&mut self, content: &str) -> Result<()> {
        self.xml_writer.lock().unwrap().write(content.as_bytes())?;
        self.xml_writer.lock().unwrap().flush()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::{
        io::Cursor,
        sync::{Arc, Mutex},
    };
    use symbol_table::SymbolTable;
    use vm_writer::VMWriter;

    use jack_tokenizer::JackTokenizer;

    use crate::CompilationEngine;
    use anyhow::Result;

    #[test]
    fn test_compilation_engine() -> Result<()> {
        let jack_code = Cursor::new("method");
        let output = Arc::new(Mutex::new(Cursor::new(Vec::new())));
        let mut tokenizer = JackTokenizer::new(jack_code)?;
        let class_symbol_table = SymbolTable::new();
        let subroutine_symbol_table = SymbolTable::new();
        tokenizer.advance()?;
        let mut compilation_engine = CompilationEngine::new(
            tokenizer,
            output.clone(),
            VMWriter::new(output.clone()),
            class_symbol_table,
            subroutine_symbol_table,
        )?;
        compilation_engine.process_token("method")?;
        let expect = "<keyword> method </keyword>\n";
        let output = output.lock().unwrap();
        let actual = String::from_utf8_lossy(output.get_ref());

        assert_eq!(expect, actual);
        Ok(())
    }
}
