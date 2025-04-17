mod rule;
mod diagnostic;
mod context;
mod rule_registry;
mod rules;
mod external_api;

use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::Diagnostic;
use crate::analysis::rule_registry::RuleRegistry;
use crate::parser::ast::*;
use crate::analysis::rules::duplicate_decl::DuplicateDeclarationRule;
use crate::analysis::rules::type_check::TypeCheckRule;
use crate::analysis::rules::undefined_val::UndefinedVariableRule;

pub struct SemanticAnalyzer {
    rule_registry: RuleRegistry,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut registry = RuleRegistry::new();

        // Register built-in rules
        registry.register(DuplicateDeclarationRule);
        registry.register(TypeCheckRule);
        registry.register(UndefinedVariableRule);
        // registry.register(AssignToImmutableRule);
        // registry.register(ReturnTypeCheckRule);
        // registry.register(ConditionTypeCheckRule);
        // registry.register(UnusedVariableRule);
        // Add more rules here...

        Self {
            rule_registry: registry,
        }
    }

    pub fn analyze(&self, program: &mut Program) -> Result<(), Vec<Diagnostic>> {
        let mut ctx = AnalysisContext::new(program);

        // Phase 1: Collect declarations
        self.collect_declarations(&mut ctx);

        // Phase 2: Apply all enabled rules
        self.apply_rules(&mut ctx);

        // Return results
        if ctx.diagnostics.has_errors() {
            Err(ctx.diagnostics.diagnostics().to_vec())
        } else {
            Ok(())
        }
    }

    fn collect_declarations(&self, ctx: &mut AnalysisContext) {
        // First pass to collect all declarations
        for decl in &ctx.program.declarations {
            match decl {
                Declaration::TypeDeclaration(type_decl) => {
                    // Add type to symbol table
                    let symbol = TypeSymbol {
                        name: type_decl.name.clone(),
                        definition: type_decl.definition.clone(),
                        span: type_decl.span,
                        is_resolved: false,
                    };

                    if let Err(msg) = ctx.symbol_table.add_symbol(
                        ctx.current_scope,
                        &type_decl.name,
                        Symbol::Type(symbol)
                    ) {
                        ctx.diagnostics.report_error(
                            "duplicate-declaration",
                            msg,
                            type_decl.span
                        );
                    }
                }

                Declaration::FunctionDeclaration(fun_decl) => {
                    // Add function to symbol table
                    let symbol = FunctionSymbol {
                        name: fun_decl.name.clone(),
                        parameters: fun_decl.parameters.clone(),
                        return_type: fun_decl.return_type.clone(),
                        span: fun_decl.span,
                        is_resolved: false,
                    };

                    if let Err(msg) = ctx.symbol_table.add_symbol(
                        ctx.current_scope,
                        &fun_decl.name,
                        Symbol::Function(symbol)
                    ) {
                        ctx.diagnostics.report_error(
                            "duplicate-declaration",
                            msg,
                            fun_decl.span
                        );
                    }
                }

                Declaration::VariableDeclaration(var_decl) => {
                    // Add variable to symbol table
                    let symbol = VariableSymbol {
                        name: var_decl.name.clone(),
                        type_ref: var_decl.type_ref.clone(),
                        is_mutable: var_decl.is_mutable,
                        span: var_decl.span,
                        is_resolved: false,
                    };

                    if let Err(msg) = ctx.symbol_table.add_symbol(
                        ctx.current_scope,
                        &var_decl.name,
                        Symbol::Variable(symbol)
                    ) {
                        ctx.diagnostics.report_error(
                            "duplicate-declaration",
                            msg,
                            var_decl.span
                        );
                    }
                }
            }
        }
    }

    fn apply_rules(&self, ctx: &mut AnalysisContext) {
        // Visit all AST nodes and apply rules
        self.visit_program(ctx);
    }

    fn visit_program(&self, ctx: &mut AnalysisContext) {
        // Apply top-level rules first
        for rule in self.rule_registry.get_all_rules() {
            if ctx.is_rule_enabled(rule.id()) {
                let program = &ctx.program.clone();
                let node = AstNode::Program(program);
                let _ = rule.check(ctx, &node);
            }
        }

        // Then recursively visit all declarations
        let declarations = &ctx.program.declarations.clone();
        for decl in declarations {
            self.visit_declaration(ctx, decl);
        }
    }

    fn visit_declaration(&self, ctx: &mut AnalysisContext, decl: &Declaration) {
        match decl {
            Declaration::TypeDeclaration(type_decl) => {
                // Apply type declaration rules
                for rule in self.rule_registry.get_all_rules() {
                    if ctx.is_rule_enabled(rule.id()) {
                        let node = AstNode::TypeDeclaration(type_decl);
                        let _ = rule.check(ctx, &node);
                    }
                }

                // Visit type definition
                self.visit_type_definition(ctx, &type_decl.definition);
            }

            Declaration::FunctionDeclaration(fun_decl) => {
                // Apply function declaration rules
                for rule in self.rule_registry.get_all_rules() {
                    if ctx.is_rule_enabled(rule.id()) {
                        let node = AstNode::FunctionDeclaration(fun_decl);
                        let _ = rule.check(ctx, &node);
                    }
                }

                // Set current function
                let prev_function = ctx.current_function.clone();
                ctx.current_function = Some(fun_decl.name.clone());

                // Create new scope for function body
                ctx.enter_scope();

                // Add parameters to scope
                for param in &fun_decl.parameters {
                    let symbol = VariableSymbol {
                        name: param.name.clone(),
                        type_ref: param.type_ref.clone(),
                        is_mutable: false,
                        span: param.span,
                        is_resolved: true,
                    };

                    let _ = ctx.symbol_table.add_symbol(
                        ctx.current_scope,
                        &param.name,
                        Symbol::Variable(symbol)
                    );
                }

                // Visit function body
                self.visit_block(ctx, &fun_decl.body);

                // Restore previous context
                ctx.exit_scope();
                ctx.current_function = prev_function;
            }

            Declaration::VariableDeclaration(var_decl) => {
                // Apply variable declaration rules
                for rule in self.rule_registry.get_all_rules() {
                    if ctx.is_rule_enabled(rule.id()) {
                        let node = AstNode::VariableDeclaration(var_decl);
                        let _ = rule.check(ctx, &node);
                    }
                }

                // Visit initializer if present
                if let Some(init) = &var_decl.initializer {
                    self.visit_expression(ctx, init);
                }
            }
        }
    }

    // Implement more visit methods for all AST node types...
    fn visit_block(&self, ctx: &mut AnalysisContext, block: &Block) {
        // Create new scope
        ctx.enter_scope();

        // Visit all statements
        for stmt in &block.statements {
            self.visit_statement(ctx, stmt);
        }

        // Exit scope
        ctx.exit_scope();
    }

    fn visit_statement(&self, ctx: &mut AnalysisContext, stmt: &Statement) {
        match stmt {
            Statement::VariableDeclaration(var_decl) => {
                // Similar to top-level variable declaration
                for rule in self.rule_registry.get_all_rules() {
                    if ctx.is_rule_enabled(rule.id()) {
                        let node = AstNode::VariableDeclaration(var_decl);
                        let _ = rule.check(ctx, &node);
                    }
                }

                // Visit initializer if present
                if let Some(init) = &var_decl.initializer {
                    self.visit_expression(ctx, init);
                }

                // Add variable to current scope
                let symbol = VariableSymbol {
                    name: var_decl.name.clone(),
                    type_ref: var_decl.type_ref.clone(),
                    is_mutable: var_decl.is_mutable,
                    span: var_decl.span,
                    is_resolved: false,
                };

                let _ = ctx.symbol_table.add_symbol(
                    ctx.current_scope,
                    &var_decl.name,
                    Symbol::Variable(symbol)
                );
            }

            // Implement other statement types...

            _ => {
                // Apply rules to this statement
                for rule in self.rule_registry.get_all_rules() {
                    if ctx.is_rule_enabled(rule.id()) {
                        let node = AstNode::Statement(stmt);
                        let _ = rule.check(ctx, &node);
                    }
                }
            }
        }
    }

    fn visit_expression(&self, ctx: &mut AnalysisContext, expr: &Expression) {
        // Apply expression rules
        for rule in self.rule_registry.get_all_rules() {
            if ctx.is_rule_enabled(rule.id()) {
                let node = AstNode::Expression(expr);
                let _ = rule.check(ctx, &node);
            }
        }

        // Recursively visit sub-expressions
        match expr {
            Expression::BinaryOperation { left, right, .. } => {
                self.visit_expression(ctx, left);
                self.visit_expression(ctx, right);
            }
            Expression::UnaryOperation { operand, .. } => {
                self.visit_expression(ctx, operand);
            }
            Expression::FunctionCall { function, arguments, .. } => {
                self.visit_expression(ctx, function);
                for arg in arguments {
                    self.visit_expression(ctx, arg);
                }
            }
            Expression::MemberAccess { object, .. } => {
                self.visit_expression(ctx, object);
            }
            Expression::IndexAccess { array, index, .. } => {
                self.visit_expression(ctx, array);
                self.visit_expression(ctx, index);
            }
            Expression::Grouping { expression, .. } => {
                self.visit_expression(ctx, expression);
            }
            // Other expression types...
            _ => {}
        }
    }

    fn visit_type_definition(&self, ctx: &mut AnalysisContext, type_def: &TypeDefinition) {
        todo!()
    }
    
    // TODO: this impl is not finished
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

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
    fn test_analysis() {
        let test_str = TEST_GRAMMAR;
        let mut lex = Lexer::new(test_str);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();
        let result = SemanticAnalyzer::new().analyze(&mut program.unwrap());
        print!("{:#?}", result);
        assert!(result.is_ok());
    }
}