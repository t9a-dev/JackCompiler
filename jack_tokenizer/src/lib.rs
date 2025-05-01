use anyhow::{anyhow, Result};
use regex::Regex;
use std::{
    io::{BufReader, Read},
    str::FromStr,
};
use strum::IntoEnumIterator;
use strum_macros::{AsRefStr, EnumIter, EnumString};

const SYMBOLS: [char; 19] = [
    '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~',
];

macro_rules! number_letter {
    () => {
        '0'..='9'
    };
}

macro_rules! alphabet_letter {
    () => {
        'a'..='z' | 'A'..='Z'
    };
}

#[derive(Debug, PartialEq, AsRefStr, EnumIter)]
pub enum TokenType {
    KeyWord,
    Symbol,
    Identifier,
    #[strum(to_string = "integerConstant")]
    IntConst,
    #[strum(to_string = "stringConstant")]
    StringConst,
}

#[derive(Debug, PartialEq, AsRefStr, EnumIter, EnumString)]
#[strum(ascii_case_insensitive)]
pub enum KeyWord {
    Class,
    Method,
    Function,
    Constructor,
    Int,
    Boolean,
    Char,
    Void,
    Var,
    Static,
    Field,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
    True,
    False,
    Null,
    This,
}

pub struct JackTokenizer {
    tokens: Vec<String>,
    current_token: Option<String>,
}

impl JackTokenizer {
    pub fn new<R: Read>(reader: R) -> Result<Self> {
        let mut buf = String::new();
        let mut jack_code = BufReader::new(reader);
        jack_code.read_to_string(&mut buf)?;

        Ok(Self {
            tokens: parse_tokens(&buf)?,
            current_token: None,
        })
    }

    pub fn has_more_tokens(&mut self) -> Result<bool> {
        Ok(self.tokens.iter().next().is_some())
    }

    pub fn advance(&mut self) -> Result<()> {
        if self.has_more_tokens()? {
            self.current_token = self.tokens.iter().next().cloned();
            let mut tokens = self.tokens.clone().into_iter();
            tokens.next();
            self.tokens = tokens.collect();
        }
        Ok(())
    }

    pub fn token_type(&self) -> Result<TokenType> {
        match &self.current_token {
            Some(t) if KeyWord::iter().any(|k| k.as_ref().to_lowercase() == *t) => {
                Ok(TokenType::KeyWord)
            }
            Some(t) if SYMBOLS.iter().any(|s| *s == t.chars().next().unwrap()) => {
                Ok(TokenType::Symbol)
            }
            Some(t) if matches!(t.chars().next().unwrap(), _c @(number_letter!())) => {
                Ok(TokenType::IntConst)
            }
            Some(t) if t.chars().next().unwrap() == '"' => Ok(TokenType::StringConst),
            Some(t) if matches!(t.chars().next().unwrap(), _c @('_' | alphabet_letter!())) => {
                Ok(TokenType::Identifier)
            }
            None => panic!("curret token is empty"),
            t => panic!("un supported token type: {:?}", t),
        }
    }

    pub fn keyword(&self) -> Result<KeyWord> {
        match &self.current_token {
            Some(token) => match KeyWord::from_str(&token) {
                Ok(keyword) => Ok(keyword),
                Err(e) => panic!(
                    "KeywordEnum parse error: {:?} token: {:?}",
                    e, self.current_token
                ),
            },
            None => panic!("current token is empty"),
        }
    }

    pub fn symbol(&self) -> Result<String> {
        Ok(self.current_token.clone().unwrap())
    }

    pub fn identifer(&self) -> Result<String> {
        Ok(self.current_token.clone().unwrap())
    }

    pub fn int_val(&self) -> Result<u16> {
        Ok(self
            .current_token
            .clone()
            .unwrap()
            .parse::<u16>()
            .expect("int_val parse failed"))
    }

    pub fn string_val(&self) -> Result<String> {
        Ok(self
            .current_token
            .clone()
            .unwrap()
            .as_str()
            .chars()
            .filter(|c| *c != '"')
            .collect())
    }
}

fn parse_tokens(input: &str) -> Result<Vec<String>> {
    let mut tokens: Vec<String> = Vec::new();

    let ignore_comment_regex = Regex::new(r"//.*(\n|$)|\n?/\*[\s\S]*?\*/\n?")?;
    let comment_ignored_input = ignore_comment_regex.replace_all(input, "").to_string();
    let mut input = comment_ignored_input.as_str();

    while input.chars().next().is_some() {
        match input.chars().next() {
            // whitespace
            Some(c) if c.is_whitespace() => {
                let mut chars = input.chars();
                chars.next();
                input = chars.as_str();
                Ok(())
            }
            // keyword,identifer
            Some(_c @ ('_' | alphabet_letter!())) => {
                let mut chars = input.chars();
                let mut token = chars.next().unwrap().to_string();
                input = chars.as_str();
                while matches!(
                    input.chars().next(),
                    Some(_c @ ('_' | alphabet_letter!() | number_letter!()))
                ) {
                    let mut chars = input.chars();
                    token += &chars.next().unwrap().to_string();
                    input = chars.as_str();
                }
                tokens.push(token);
                Ok(())
            }
            // symbol
            Some(c) if SYMBOLS.iter().any(|symbol| c == *symbol) => {
                let mut chars = input.chars();
                let token = chars.next().unwrap().to_string();
                input = chars.as_str();
                tokens.push(token);
                Ok(())
            }
            // integer
            Some(_c @ (number_letter!())) => {
                let mut chars = input.chars();
                let mut token = chars.next().unwrap().to_string();
                input = chars.as_str();
                while matches!(input.chars().next(), Some(_c @ (number_letter!()))) {
                    let mut chars = input.chars();
                    token += &chars.next().unwrap().to_string();
                    input = chars.as_str();
                }
                tokens.push(token);
                Ok(())
            }
            //stringConst
            Some(c) if c == '"' => {
                // '"'を見つけたら次に'"'を見つけるまでの文字をtokenとする
                let mut chars = input.chars();
                let mut token = chars.next().unwrap().to_string();
                input = chars.as_str();
                while input.chars().next() != Some('"') {
                    chars = input.chars();
                    token += &chars.next().unwrap().to_string();
                    input = chars.as_str();
                }
                chars = input.chars();
                token += &chars.next().unwrap().to_string();
                input = chars.as_str();
                tokens.push(token);
                Ok(())
            }
            None => Ok(()),
            c => Err(anyhow!("un supported token: {:?} input: {:?}", c, input)),
        }?
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;

    #[test]
    fn regex_playground() -> Result<()> {
        let re = Regex::new(r"//.*(\n|$)")?;
        let result = re.replace("// this comment", "");
        assert_eq!(result, "");

        let re = Regex::new(r"/\*[\s\S]*?\*/")?;
        let result = re.replace(
            "/**
         * comment 
         */",
            "",
        );
        assert_eq!(result, "");

        let re = Regex::new(r"/\*[\s\S]*?\*/")?;
        let result = re.replace(
            "/*
         * comment 
         */",
            "",
        );
        assert_eq!(result, "");

        let re = Regex::new(r"//.*(\n|$)|\n?/\*[\s\S]*?\*/\n?")?;
        let result = re.replace_all(
            "/*
* comment
*/
let a = \"hello\";
// comment 'abc'
/**
* comment
*/",
            "",
        );
        assert_eq!(result, "let a = \"hello\";\n");
        Ok(())
    }
    #[test]
    fn test_parse_token_when_string_const() {
        let input = r#""negative" "positive""#;
        let mut actual = vec!["\"positive\"", "\"negative\""];
        assert_eq!(parse_tokens(input).unwrap().sort(), actual.sort());
    }

    #[test]
    fn test_parse_token() {
        let input = r#"if (x < 0) {
    // comment
    let sign = "negative";
    let sign_2 = "positive";
}"#;
        let actual = vec![
            "if",
            "(",
            "x",
            "<",
            "0",
            ")",
            "{",
            "let",
            "sign",
            "=",
            "\"negative\"",
            ";",
            "let",
            "sign_2",
            "=",
            "\"positive\"",
            ";",
            "}",
        ];
        assert_eq!(parse_tokens(input).unwrap(), actual);
    }

    #[test]
    fn test_parse_token_when_multi_line_comment() {
        let input = r#"if (x < 0) {
    // single line comment
    /**
     * multi line comment '1'
     */
    /*
     multi line comment '2'
     */
    let sign = "negative";
    let sign_2 = "positive";
}"#;
        let actual = vec![
            "if",
            "(",
            "x",
            "<",
            "0",
            ")",
            "{",
            "let",
            "sign",
            "=",
            "\"negative\"",
            ";",
            "let",
            "sign_2",
            "=",
            "\"positive\"",
            ";",
            "}",
        ];
        assert_eq!(parse_tokens(input).unwrap(), actual);
    }

    #[test]
    fn test_jack_tokenizer() -> Result<()> {
        let file_content = std::io::Cursor::new(
            r#"if (x < 0) {
    // comment
    let sign = "negative";
    let sign_2 = "positive";
}"#
            .as_bytes(),
        );
        let mut tokenizer = JackTokenizer::new(file_content)?;

        assert_eq!(tokenizer.current_token.clone(), None);

        tokenizer.advance()?;
        assert_eq!(tokenizer.current_token.clone().unwrap(), "if".to_string());
        assert_eq!(tokenizer.token_type()?, TokenType::KeyWord);

        tokenizer.advance()?;
        assert_eq!(tokenizer.current_token.clone().unwrap(), "(".to_string());
        assert_eq!(tokenizer.token_type()?, TokenType::Symbol);

        tokenizer.advance()?;
        assert_eq!(tokenizer.current_token.clone().unwrap(), "x".to_string());
        assert_eq!(tokenizer.token_type()?, TokenType::Identifier);

        Ok(())
    }
}
