use crate::{
    ast::{self, ASTVisitor, expr::ExprVisitor, stmt::StmtVisitor},
    resolver::var_map::ScopedVarMap,
    source_file::SourceFile,
    symbol::Symbol,
    type_checker::{
        r#type::Type,
        type_checker_error::{TypeCheckerError, TypeCheckerErrorType},
        typed_symbol_table::{TypedSymbolTable, TypedSymbolTableEntry},
    },
    with_token::WithToken,
};

pub mod r#type;
pub mod type_checker_error;
mod typed_symbol_table;

#[derive(Debug)]
pub struct TypeChecker<'a> {
    source_file: &'a SourceFile,
    symbol_table: &'a ScopedVarMap,
    type_table: TypedSymbolTable,
}

pub type TypeCheckerResult<T> = anyhow::Result<T, TypeCheckerError>;

impl<'a> TypeChecker<'a> {
    pub fn new(source_file: &'a SourceFile, symbol_table: &'a ScopedVarMap) -> Self {
        TypeChecker {
            source_file,
            symbol_table,
            type_table: TypedSymbolTable::new(),
        }
    }

    fn assert_int(&mut self, expr: &ast::expr::Expr) -> TypeCheckerResult<()> {
        let ty = self.visit_expr(expr)?;
        if ty != Type::Int {
            Err(self.make_error(
                TypeCheckerErrorType::TypeMismatch {
                    expected: Type::Int,
                    actual: ty,
                },
                &expr.token(),
            ))
        } else {
            Ok(())
        }
    }

    fn get_symbol(&self, symbol: Symbol) -> Option<&TypedSymbolTableEntry> {
        let name = self.get_symbol_name(symbol);
        self.type_table.get(name)
    }

    fn get_symbol_name(&self, symbol: Symbol) -> &str {
        self.symbol_table.resolve_assert(symbol)
    }

    fn get_symbol_type(&self, symbol: Symbol) -> Option<&Type> {
        self.get_symbol(symbol).map(|entry| &entry.ty)
    }

    fn make_error<T>(
        &self,
        error_type: TypeCheckerErrorType,
        token: &WithToken<T>,
    ) -> TypeCheckerError {
        let token = token.get_token(self.source_file.get_tokens_checked());
        TypeCheckerError {
            error_type,
            span: token.span(),
        }
    }
}

impl<'a> ExprVisitor<TypeCheckerResult<Type>> for TypeChecker<'a> {
    fn visit_assignment_expr(
        &mut self,
        expr: &crate::ast::expr::AssignExpr,
    ) -> TypeCheckerResult<Type> {
        let target_type = self.visit_expr(&expr.target)?;
        let rhs_type = self.visit_expr(&expr.right)?;

        if let Type::Function { .. } = target_type {
            Err(self.make_error(
                TypeCheckerErrorType::TypeCannotBeLValue(target_type),
                &expr.target.token(),
            ))
        } else if target_type != rhs_type {
            Err(self.make_error(
                TypeCheckerErrorType::TypeMismatch {
                    expected: target_type,
                    actual: rhs_type,
                },
                &expr.eq_token,
            ))
        } else {
            Ok(target_type)
        }
    }

    fn visit_binary_expr(
        &mut self,
        expr: &crate::ast::expr::BinaryExpr,
    ) -> TypeCheckerResult<Type> {
        self.assert_int(&expr.left)?;
        self.assert_int(&expr.right)?;
        Ok(Type::Int)
    }

    fn visit_conditional_expr(
        &mut self,
        expr: &ast::expr::ConditionalExpr,
    ) -> TypeCheckerResult<Type> {
        self.assert_int(&expr.condition)?;
        let then_type = self.visit_expr(&expr.then_expr)?;
        let else_type = self.visit_expr(&expr.else_expr)?;

        if then_type != else_type {
            Err(self.make_error(
                TypeCheckerErrorType::TypeMismatch {
                    expected: then_type,
                    actual: else_type,
                },
                &expr.else_expr.token(),
            ))
        } else {
            Ok(then_type)
        }
    }

    fn visit_constant(
        &mut self,
        expr: &WithToken<crate::lexer::token::Literal>,
    ) -> TypeCheckerResult<Type> {
        match expr.item {
            crate::lexer::token::Literal::Integer(_) => Ok(Type::Int),
            _ => unimplemented!("only integer literals are supported for now"),
        }
    }

    fn visit_function_call(&mut self, expr: &ast::expr::FunctionCall) -> TypeCheckerResult<Type> {
        let callee_type = self.visit_expr(&expr.callee_expr)?;
        let arg_types = expr
            .args
            .iter()
            .map(|arg| self.visit_expr(&arg))
            .collect::<Result<Vec<_>, _>>()?;

        if let Type::Function {
            param_types,
            return_type,
        } = callee_type
        {
            if param_types.len() != arg_types.len() {
                Err(self.make_error(
                    TypeCheckerErrorType::FunctionCallWrongArgsCount {
                        expected: param_types.len(),
                        actual: arg_types.len(),
                    },
                    &expr.callee_expr.token(),
                ))
            } else {
                param_types.iter().zip(arg_types).enumerate().try_for_each(
                    |(i, (param_ty, arg_ty))| {
                        if *param_ty != arg_ty {
                            Err(self.make_error(
                                TypeCheckerErrorType::IncorrectArgumentType {
                                    expected: param_ty.clone(),
                                    actual: arg_ty,
                                    arg_index: i,
                                },
                                &expr.args[i],
                            ))
                        } else {
                            Ok(())
                        }
                    },
                )?;
                Ok(*return_type)
            }
        } else {
            Err(self.make_error(
                TypeCheckerErrorType::TypeNotCallable(callee_type),
                &expr.callee_expr.token(),
            ))
        }
    }

    fn visit_unary_expr(&mut self, expr: &ast::expr::UnaryExpr) -> TypeCheckerResult<Type> {
        self.assert_int(&expr.operand)?;
        Ok(Type::Int)
    }

    fn visit_variable(
        &mut self,
        var: &WithToken<crate::symbol::Symbol>,
    ) -> TypeCheckerResult<Type> {
        let symbol = var.item;
        Ok(self
            .get_symbol_type(symbol)
            .expect("variable must be present in the symbol table before access")
            .clone())
    }
}

impl StmtVisitor<TypeCheckerResult<()>> for TypeChecker<'_> {
    fn visit_break(&mut self, _: &ast::stmt::BreakStmt) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_case(&mut self, _: &ast::stmt::CaseStmt) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_compound(&mut self, block: &ast::Block) -> TypeCheckerResult<()> {
        self.visit_block(block)
    }

    fn visit_continue(&mut self, _: &ast::stmt::ContinueStmt) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_default(&mut self, _: &ast::stmt::DefaultStmt) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_do_while_stmt(&mut self, stmt: &ast::stmt::DoWhileStmt) -> TypeCheckerResult<()> {
        self.visit_stmt(&stmt.body)?;
        self.assert_int(&stmt.condition)
    }

    fn visit_expr_stmt(&mut self, stmt: &ast::expr::Expr) -> TypeCheckerResult<()> {
        self.visit_expr(stmt)?;
        Ok(())
    }

    fn visit_for_stmt(&mut self, stmt: &ast::stmt::ForStmt) -> TypeCheckerResult<()> {
        if let Some(init) = &stmt.initializer.item {
            match init {
                ast::stmt::ForStmtInit::Declaration(decls) => {
                    for var_decl in decls {
                        self.visit_var_decl(var_decl)?;
                    }
                }
                ast::stmt::ForStmtInit::Expression(expr) => {
                    self.visit_expr(expr)?;
                }
            }
        }
        if let Some(cond) = &stmt.condition.item {
            self.assert_int(cond)?;
        }
        if let Some(incr) = &stmt.step.item {
            self.visit_expr(incr)?;
        }
        self.visit_stmt(&stmt.body)
    }

    fn visit_goto_stmt(&mut self, _: &WithToken<String>) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &ast::stmt::IfStmt) -> TypeCheckerResult<()> {
        self.assert_int(&stmt.condition)?;
        self.visit_stmt(&stmt.then_stmt)?;
        if let Some(else_stmt) = &stmt.else_stmt {
            self.visit_stmt(&else_stmt.item)?;
        }
        Ok(())
    }

    fn visit_label_stmt(&mut self, stmt: &ast::stmt::Label) -> TypeCheckerResult<()> {
        self.visit_stmt(&stmt.next_stmt)?;
        Ok(())
    }

    fn visit_null_stmt(&mut self) -> TypeCheckerResult<()> {
        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        stmt: &WithToken<Option<ast::expr::Expr>>,
    ) -> TypeCheckerResult<()> {
        if let Some(expr) = &stmt.item {
            self.visit_expr(expr)?;
        }
        Ok(())
    }

    fn visit_switch_stmt(&mut self, stmt: &ast::stmt::SwitchStmt) -> TypeCheckerResult<()> {
        self.assert_int(&stmt.condition)?;
        self.visit_stmt(&stmt.body)
    }

    fn visit_while_stmt(&mut self, stmt: &ast::stmt::WhileStmt) -> TypeCheckerResult<()> {
        self.assert_int(&stmt.condition)?;
        self.visit_stmt(&stmt.body)
    }
}

impl<'a> ASTVisitor for TypeChecker<'a> {
    type BlockItemResult = TypeCheckerResult<()>;
    type BlockResult = TypeCheckerResult<()>;
    type ExprResult = TypeCheckerResult<Type>;
    type FunctionDeclResult = TypeCheckerResult<()>;
    type ProgramResult = TypeCheckerResult<()>;
    type StmtResult = TypeCheckerResult<()>;
    type VarDeclResult = TypeCheckerResult<()>;

    fn visit_block(&mut self, block: &ast::Block) -> Self::BlockResult {
        for item in &block.body {
            self.visit_block_item(item)?;
        }
        Ok(())
    }

    fn visit_block_item(&mut self, item: &ast::BlockItem) -> Self::BlockItemResult {
        match item {
            ast::BlockItem::FunctionDecl(func_decl) => self.visit_function_def(func_decl),
            ast::BlockItem::VarDecl(decls) => {
                for decl in decls {
                    self.visit_var_decl(decl)?;
                }
                Ok(())
            }
            ast::BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_function_def(&mut self, func_decl: &ast::FunctionDecl) -> Self::FunctionDeclResult {
        let func_ty = Type::Function {
            param_types: func_decl.params.iter().map(|_| Type::Int).collect(),
            return_type: Box::new(Type::Int),
        };
        let name_str = self.get_symbol_name(func_decl.name.item).to_string();

        if let Some(old_decl) = self.get_symbol(func_decl.name.item) {
            if let (
                Type::Function {
                    param_types: old_params,
                    ..
                },
                Type::Function { param_types, .. },
            ) = (&old_decl.ty, &func_ty)
            {
                if old_params != param_types {
                    let prev_line_col = self
                        .source_file
                        .line_col_token_begin(old_decl.token.token_id);
                    return Err(self.make_error(
                        TypeCheckerErrorType::FunctionConflictingDeclarations {
                            new_type: func_ty,
                            prev_type: old_decl.ty.clone(),
                            prev_line_col,
                        },
                        &func_decl.name,
                    ));
                }
            } else {
                unreachable!(
                    "function symbol in the symbol table must have a function type; this is asserted during the resolve stage"
                );
            }
        } else {
            self.type_table.insert(
                name_str,
                TypedSymbolTableEntry {
                    ty: func_ty,
                    token: func_decl.name.with_value(()),
                },
            );
        }

        for param in &func_decl.params {
            if let Some(param_sym) = param.item {
                let param_ty = Type::Int;
                let param_name = self.get_symbol_name(param_sym).to_string();
                self.type_table.insert(
                    param_name,
                    TypedSymbolTableEntry {
                        ty: param_ty,
                        token: param.with_value(()),
                    },
                );
            }
        }

        if let Some(body) = &func_decl.body {
            self.visit_block(body)?;
        }

        Ok(())
    }

    fn visit_program(&mut self, program: &ast::Program) -> Self::ProgramResult {
        for func_decl in &program.function_defs {
            self.visit_function_def(func_decl)?;
        }
        Ok(())
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDeclaration) -> Self::VarDeclResult {
        let var_type = TypedSymbolTableEntry {
            ty: Type::Int,
            token: var_decl.name.with_value(()),
        };
        let name = self.get_symbol_name(var_decl.name.item).to_string();
        self.type_table.insert(name, var_type);

        if let Some(init) = &var_decl.initializer {
            self.assert_int(&init)?;
        }

        Ok(())
    }
}
