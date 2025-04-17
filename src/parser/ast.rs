use std::collections::HashMap;

/// Represents a position in the source code
#[derive(Debug, Clone, Copy)]
pub struct SourcePosition {
    pub line: usize,
    pub column: usize,
}

/// Represents a span in the source code (start and end positions)
#[derive(Debug, Clone, Copy)]
pub struct SourceSpan {
    pub start: SourcePosition,
    pub end: SourcePosition,
}

/// The root of the AST, containing all top-level declarations
///
/// When returned by a function, in the c ffi interface, is not intended to be used directly, just passed to other functions expecting it
/// > **WARNING**: not c fii compatible
#[repr(C)]
#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
    pub symbol_table: SymbolTable,
}

/// tracks ast node types, in a single enum
pub enum AstNode<'a> {
    Program(&'a Program),
    Declaration(&'a Declaration),
    TypeDeclaration(&'a TypeDeclaration),
    FunctionDeclaration(&'a FunctionDeclaration),
    VariableDeclaration(&'a VariableDeclaration),
    Statement(&'a Statement),
    Expression(&'a Expression),
    Block(&'a Block),
    StructField(&'a StructField),
}

/// A symbol table to track declarations across the program
#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: HashMap<ScopeId, Scope>,
    next_scope_id: usize,
    global_scope_id: ScopeId,
}

/// impl for use in the analysis system
impl SymbolTable {
    pub fn new() -> Self {
        let mut table = Self {
            scopes: HashMap::new(),
            next_scope_id: 0,
            global_scope_id: ScopeId(0),
        };

        table.create_scope(None); // global scope
        table
    }

    pub fn global_scope(&mut self) -> ScopeId {
        self.global_scope_id
    }

    pub fn create_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;

        let scope = Scope {
            id,
            parent,
            symbols: HashMap::new(),
        };

        self.scopes.insert(id ,scope);
        id
    }

    pub fn get_parent_scope(&mut self, scope_id: ScopeId) -> Option<ScopeId> {
        self.scopes.get(&scope_id).and_then(|scope| scope.parent)
    }

    pub fn add_symbol(&mut self, scope_id: ScopeId, name: &str, symbol: Symbol) -> Result<(), String> {
        if let Some(scope) = self.scopes.get_mut(&scope_id) {
            if scope.symbols.contains_key(name) {
                return Err(format!("Symbol '{}' already declared in this scope", name))
            }
            scope.symbols.insert(name.to_string(), symbol);
            Ok(())
        } else {
            Err(format!("Scope '{:?}' not found", scope_id))
        }
    }

    pub fn lookup_symbol(&self, scope_id: ScopeId, name: &str) -> Option<&Symbol> {
        let mut current = Some(scope_id);

        while let Some(id) = current {
            if let Some(scope) = self.scopes.get(&id) {
                if let Some(symbol) = scope.symbols.get(name) {
                    return Some(symbol);
                }
                current = scope.parent;
            } else {
                return None;
            }
        }

        None
    }
}

/// unique id for each scope encountered during analysis, id = 0 is always global scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(usize);

/// a unique scope identified by it's `ScopeId`
#[derive(Debug, Clone)]
pub struct Scope {
    id: ScopeId,
    parent: Option<ScopeId>,
    symbols: HashMap<String, Symbol>,
}

/// Base trait for all symbols
#[derive(Debug, Clone)]
pub enum Symbol {
    Type(TypeSymbol),
    Function(FunctionSymbol),
    Variable(VariableSymbol),
}

/// Symbol representing a type definition
#[derive(Debug, Clone)]
pub struct TypeSymbol {
    pub name: String,
    pub definition: TypeDefinition,
    pub span: SourceSpan,
    pub is_resolved: bool,
}

/// Symbol representing a function declaration
#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeReference,
    pub span: SourceSpan,
    pub is_resolved: bool,
}

/// Symbol representing a variable declaration
#[derive(Debug, Clone)]
pub struct VariableSymbol {
    pub name: String,
    pub type_ref: TypeReference,
    pub is_mutable: bool,
    pub span: SourceSpan,
    pub is_resolved: bool,
}

/// Top-level declarations in the program
#[derive(Debug, Clone)]
pub enum Declaration {
    TypeDeclaration(TypeDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
}

/// A type declaration (type alias or struct)
#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub name: String,
    pub definition: TypeDefinition,
    pub span: SourceSpan,
    pub symbol_id: Option<String>, // Reference to the symbol table
}

/// Different kinds of type definitions
#[derive(Debug, Clone)]
pub enum TypeDefinition {
    Alias(TypeReference),
    Struct(StructDefinition),
}

/// Definition of a struct type
#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub fields: Vec<StructField>,
}

/// A field in a struct
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_ref: TypeReference,
    pub span: SourceSpan,
}

/// A function declaration
#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeReference,
    pub body: Block,
    pub span: SourceSpan,
    pub symbol_id: Option<String>, // Reference to the symbol table
}

/// A parameter in a function declaration
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub type_ref: TypeReference,
    pub span: SourceSpan,
}

/// A variable declaration
#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub type_ref: TypeReference,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
    pub span: SourceSpan,
    pub symbol_id: Option<String>, // Reference to the symbol table
}

/// A reference to a type, which may be resolved during analysis
#[derive(Debug, Clone, PartialEq)]
pub enum TypeReference {
    // Built-in types
    Int32,
    Int64,
    Float32,
    Float64,
    Bool,
    String,
    Void,

    // User-defined types
    Named(String),

    // More complex types
    Function {
        parameters: Vec<TypeReference>,
        return_type: Box<TypeReference>,
    },

    // Will be resolved during type checking
    Unresolved,
}

/// A block of statements with its own scope
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: SourceSpan,
    pub local_symbols: HashMap<String, Symbol>, // Local scope
}

/// Statements in a `Block`
#[derive(Debug, Clone)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Assignment(Assignment),
    ExpressionStatement(Expression),
    ReturnStatement(ReturnStatement),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    ForStatement(ForStatement),
    Block(Block),
}

/// An assignment statement
#[derive(Debug, Clone)]
pub struct Assignment {
    pub target: LValue,
    pub operator: AssignmentOperator,
    pub value: Expression,
    pub span: SourceSpan,
}

/// Left-hand side of an assignment
#[derive(Debug, Clone)]
pub enum LValue {
    Identifier(String),
    MemberAccess {
        object: Box<Expression>,
        member: String,
    },
    IndexAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
}

/// Assignment operators
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    Simple,     // =
    Add,        // +=
    Subtract,   // -=
    Multiply,   // *=
    Divide,     // /=
    Modulo,     // %=
    BitwiseAnd, // &=
    BitwiseOr,  // |=
    BitwiseXor, // ^=
    LeftShift,  // <<=
    RightShift, // >>=
}

/// A return statement
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub span: SourceSpan,
}

/// An if statement (with optional else)
#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Block,
    pub else_branch: Option<Box<Statement>>, // Either Block or another IfStatement (for else if)
    pub span: SourceSpan,
}

/// A while loop
#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Block,
    pub span: SourceSpan,
}

/// A for loop
#[derive(Debug, Clone)]
pub struct ForStatement {
    pub iterator: String,
    pub iterable: Expression, // Could be a range or a collection
    pub body: Block,
    pub span: SourceSpan,
}

/// An expression
#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    BinaryOperation {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
        span: SourceSpan,
        type_ref: TypeReference, // Filled during type checking
    },
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
        span: SourceSpan,
        type_ref: TypeReference, // Filled during type checking
    },
    FunctionCall {
        function: Box<Expression>, // Can be identifier or member access
        arguments: Vec<Expression>,
        span: SourceSpan,
        type_ref: TypeReference, // Return type, filled during type checking
    },
    MemberAccess {
        object: Box<Expression>,
        member: String,
        span: SourceSpan,
        type_ref: TypeReference, // Filled during type checking
    },
    IndexAccess {
        array: Box<Expression>,
        index: Box<Expression>,
        span: SourceSpan,
        type_ref: TypeReference, // Filled during type checking
    },
    Grouping {
        expression: Box<Expression>,
        span: SourceSpan,
        type_ref: TypeReference, // Same as inner expression, filled during type checking
    },
}

/// Literal values
#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    // Comparison
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    // Logical
    And,
    Or,

    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Positive,
    Negative,
    Not,
    BitwiseNot,
}
