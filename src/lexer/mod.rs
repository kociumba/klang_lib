use std::iter::Peekable;
use std::str::Chars;
use std::string::String;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    // Keywords
    Type,
    Const,
    Var,
    Fun,
    Struct,
    Return,
    If,
    Else,
    For,
    While,

    // Types
    Int32,
    Int64,
    Float32,
    Float64,
    String,
    Bool,
    Void,
    Function,

    // Identifiers and literals
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(f64),

    // Operators and punctuation
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    NotEqual,
    PlusEqual,
    MinusEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Colon,
    Semicolon,
    Arrow,
    Or,
    BitwiseOr,
    And,
    BitwiseAnd,
    BitwiseXor,
    LeftShift,
    RightShift,
    Not,

    // misc symbols
    Dot,

    // End of file
    EOF,
}

#[derive(PartialEq, Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub start: (usize, usize), // (line, column)
    pub end: (usize, usize),
}

pub struct Lexer<'src> {
    source: &'src str,
    chars: Peekable<Chars<'src>>,
    line: usize,
    column: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Lexer {
            source,
            chars: source.chars().peekable(),
            line: 1,
            column: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.next();
        if let Some(c) = ch {
            self.column += 1;
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            }
        }
        ch
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        if let Some(&'/') = self.peek() {
            self.advance(); // consume the second slash
            while let Some(&c) = self.peek() {
                if c == '\n' {
                    break;
                }
                self.advance();
            }
        }
    }

    fn read_identifier(&mut self, first_char: char) -> String {
        let mut identifier = String::new();
        identifier.push(first_char);

        while let Some(&c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(c);
                self.advance();
            } else {
                break;
            }
        }

        identifier
    }

    fn read_number(&mut self, first_char: char) -> Result<f64, String> {
        let mut number = String::new();
        number.push(first_char);

        let mut has_decimal = false;

        while let Some(&c) = self.peek() {
            if c.is_digit(10) {
                number.push(c);
                self.advance();
            } else if c == '.' && !has_decimal {
                has_decimal = true;
                number.push(c);
                self.advance();
            } else {
                break;
            }
        }

        number
            .parse::<f64>()
            .map_err(|e| format!("Invalid number: {}", e))
    }

    fn read_string(&mut self) -> Result<String, String> {
        let mut string = String::new();

        while let Some(c) = self.advance() {
            if c == '"' {
                return Ok(string);
            } else if c == '\\' {
                if let Some(escape) = self.advance() {
                    match escape {
                        'n' => string.push('\n'),
                        't' => string.push('\t'),
                        'r' => string.push('\r'),
                        '\\' => string.push('\\'),
                        '"' => string.push('"'),
                        _ => return Err(format!("Invalid escape sequence: \\{}", escape)),
                    }
                } else {
                    return Err("Unterminated string literal".to_string());
                }
            } else {
                string.push(c);
            }
        }

        Err("Unterminated string literal".to_string())
    }

    pub fn next_token(&mut self) -> Result<SpannedToken, String> {
        loop {
            self.skip_whitespace();

            let start = (self.line, self.column);

            // Check for EOF
            let c = match self.advance() {
                Some(c) => c,
                None => {
                    let end = (self.line, self.column);
                    return Ok(SpannedToken {
                        token: Token::EOF,
                        start,
                        end,
                    });
                }
            };

            // Process the current character
            let token_result: Result<Token, String> = match c {
                '{' => Ok(Token::LeftBrace),
                '}' => Ok(Token::RightBrace),
                '(' => Ok(Token::LeftParen),
                ')' => Ok(Token::RightParen),
                '[' => Ok(Token::LeftBracket),
                ']' => Ok(Token::RightBracket),
                ',' => Ok(Token::Comma),
                ':' => Ok(Token::Colon),
                ';' => Ok(Token::Semicolon),
                '+' => {
                    if let Some(&'=') = self.peek() {
                        self.advance();
                        Ok(Token::PlusEqual)
                    } else {
                        Ok(Token::Plus)
                    }
                }
                '-' => {
                    // Check for -> arrow
                    if let Some(&'>') = self.peek() {
                        self.advance();
                        Ok(Token::Arrow)
                    } else if let Some(&'=') = self.peek() {
                        self.advance();
                        Ok(Token::MinusEqual)
                    } else {
                        Ok(Token::Minus)
                    }
                }
                '*' => Ok(Token::Star),
                '/' => {
                    if let Some(&'/') = self.peek() {
                        self.skip_comment();
                        continue;
                    } else {
                        Ok(Token::Slash)
                    }
                }
                '%' => Ok(Token::Percent),
                '=' => {
                    if let Some(&'=') = self.peek() {
                        self.advance();
                        Ok(Token::EqualEqual)
                    } else {
                        Ok(Token::Equal)
                    }
                }
                '!' => {
                    if let Some(&'=') = self.peek() {
                        self.advance();
                        Ok(Token::NotEqual)
                    } else {
                        Ok(Token::Not)
                        // Err(format!("Unexpected character: {}", c))
                    }
                }
                '>' => {
                    if let Some(&'=') = self.peek() {
                        self.advance();
                        Ok(Token::GreaterEqual)
                    } else if let Some(&'>') = self.peek() {
                        self.advance();
                        Ok(Token::RightShift)
                    } else {
                        Ok(Token::Greater)
                    }
                }
                '<' => {
                    if let Some(&'=') = self.peek() {
                        self.advance();
                        Ok(Token::LessEqual)
                    } else if let Some(&'<') = self.peek() {
                        self.advance();
                        Ok(Token::LeftShift)
                    } else {
                        Ok(Token::Less)
                    }
                }
                '|' => {
                    if let Some(&'|') = self.peek() {
                        self.advance();
                        Ok(Token::Or)
                    } else {
                        Ok(Token::BitwiseOr)
                    }
                }
                '&' => {
                    if let Some(&'&') = self.peek() {
                        self.advance();
                        Ok(Token::And)
                    } else {
                        Ok(Token::BitwiseAnd)
                    }
                }
                '^' => Ok(Token::BitwiseXor),
                '.' => Ok(Token::Dot),
                '"' => {
                    self.read_string().map(Token::StringLiteral)
                }
                '0'..='9' => {
                    self.read_number(c).map(Token::NumberLiteral)
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let identifier = self.read_identifier(c);
                    let token = match identifier.as_str() {
                        "type" => Token::Type,
                        "const" => Token::Const,
                        "var" => Token::Var,
                        "fun" => Token::Fun,
                        "struct" => Token::Struct,
                        "return" => Token::Return,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "for" => Token::For,
                        "while" => Token::While,
                        "int32" => Token::Int32,
                        "int64" => Token::Int64,
                        "int" => Token::Int64, // Default int is int64
                        "float32" => Token::Float32,
                        "float64" => Token::Float64,
                        "float" => Token::Float32, // Default float is float32
                        "string" => Token::String,
                        "bool" => Token::Bool,
                        "void" => Token::Void,
                        "function" => Token::Function,
                        _ => Token::Identifier(identifier),
                    };
                    Ok(token)
                }
                _ => Err(format!("Unexpected character: {}", c)),
            };

            let end = (self.line, self.column);
            return token_result.map(|token| SpannedToken { token, start, end });
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<SpannedToken>, String> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let is_eof = token.token == Token::EOF;
            tokens.push(token);

            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_GRAMMAR: &str = r#"
// comments should be ignored
type MyStruct: struct {
    MyField: int,
}

type MyType: string

fun isEven(arg1: int) -> bool {
    return arg1 % 2 == 0
}

fun main() -> int {
    run(5, "6")

    var x: int = 0

    while x < 2 {
        x = x + 1
        printf("%d\n", isEven(x))
    }
    return 0
}

fun run(arg1: int, arg2: string) -> int {
    return loop(arg1, arg2)
}

fun loop(arg1: int, arg2: string) -> int {
    var j: int = 0

    for i in range(0, arg1) {
        j = i + j
        if isEven(i) {
            printf("%d\n", j)
        } else {
            printf("%d\n", i)
        }
    }

    return j
}
"#;

    #[test]
    fn test_lexer() {
        let test_str = TEST_GRAMMAR;
        let mut lex = Lexer::new(test_str);
        // loop {
        //     let token = lex.next_token().unwrap();
        //     let is_eof = token == Token::EOF;
        //
        //     if is_eof {
        //         break;
        //     }
        // }

        println!("{:#?}", lex.tokenize())
    }
}
