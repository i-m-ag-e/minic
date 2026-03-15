use std::mem;

use crate::{
    ast::{
        self, ASTRefVisitor,
        expr::{self, BinaryOp, ExprRefVisitor, UnaryOp},
        stmt::{Label, StmtRefVisitor},
    },
    lexer::token::{Literal, TokenID},
    resolver::var_map::VarMap,
    source_file::SourceFile,
    tacky::{self, Instruction, InstructionKind, Value},
    with_token::WithToken,
};

#[derive(Debug, Clone)]
pub struct TackyGen<'a> {
    pub var_count: usize,
    pub current_body: Vec<Instruction>,
    source_file: &'a SourceFile,
    symbol_table: &'a VarMap,
}

impl<'a> TackyGen<'a> {
    pub fn new(source_file: &'a SourceFile, symbol_table: &'a VarMap) -> Self {
        Self {
            var_count: 0,
            current_body: Vec::new(),
            source_file,
            symbol_table,
        }
    }

    fn make_var(&mut self) -> Value {
        let var_id = self.var_count;
        self.var_count += 1;
        Value::Var(format!("pv.{}", var_id))
    }

    fn make_logic_label(&self, op: &str, stage: &str, token_id: TokenID) -> String {
        let line_col = self.source_file.line_col_token_begin(token_id);
        format!("{}_{}.{}_{}", op, stage, line_col.0, line_col.1)
    }

    fn emit_raw(&mut self, instruction: InstructionKind, token_id: TokenID, message: Option<&str>) {
        let debug_info = instruction.debug_info();
        let token_line_col = self.source_file.line_col_token_begin(token_id);
        self.current_body.push(Instruction {
            kind: instruction,
            debug_info: crate::debug_info::DebugInfo::new(
                token_line_col.0,
                token_line_col.1,
                message
                    .map(|msg| format!("({}) {}", msg, debug_info))
                    .unwrap_or(debug_info),
            ),
        });
    }

    fn emit(&mut self, instruction: InstructionKind, token_id: TokenID) {
        self.emit_raw(instruction, token_id, None);
    }

    fn emit_with_message(
        &mut self,
        instruction: InstructionKind,
        token_id: TokenID,
        message: &str,
    ) {
        self.emit_raw(instruction, token_id, Some(message));
    }

    fn emit_many<const N: usize>(
        &mut self,
        token_id: TokenID,
        instructions: [(InstructionKind, &str); N],
    ) {
        for (instruction, message) in instructions {
            if !message.is_empty() {
                self.emit_with_message(instruction, token_id, message);
            } else {
                self.emit(instruction, token_id);
            }
        }
    }
}

impl<'a> ExprRefVisitor<Value> for TackyGen<'a> {
    fn visit_assignment_expr(&mut self, expr: &expr::AssignExpr) -> Value {
        let dest = self.visit_expr(&expr.target);
        let src = self.visit_expr(&expr.right);
        self.emit(
            InstructionKind::Copy {
                dest: dest.clone(),
                src,
            },
            expr.eq_token.token_id,
        );

        dest
    }

    fn visit_binary_expr(&mut self, expr: &expr::BinaryExpr) -> Value {
        let left_val = self.visit_expr(&expr.left);
        let token_id = expr.operator.token_id;

        if let BinaryOp::And = expr.operator.item {
            let short_circuit_label = self.make_logic_label("and", "false", expr.operator.token_id);
            let end_label = self.make_logic_label("and", "end", expr.operator.token_id);

            self.emit_with_message(
                InstructionKind::JumpIfZero(left_val, short_circuit_label.clone()),
                token_id,
                "(&&: lhs)",
            );
            let right_val = self.visit_expr(&expr.right);
            let dest_var = self.make_var();

            self.emit_many(
                token_id,
                [
                    (
                        InstructionKind::JumpIfZero(right_val.clone(), short_circuit_label.clone()),
                        "(&&: rhs)",
                    ),
                    (
                        InstructionKind::Copy {
                            dest: dest_var.clone(),
                            src: Value::Constant(1),
                        },
                        "(&&) true",
                    ),
                    (InstructionKind::Jump(end_label.clone()), "(&&) end"),
                    (InstructionKind::Label(short_circuit_label), ""),
                    (
                        InstructionKind::Copy {
                            dest: dest_var.clone(),
                            src: Value::Constant(0),
                        },
                        "(&&) false",
                    ),
                    (InstructionKind::Label(end_label), ""),
                ],
            );

            dest_var
        } else if let BinaryOp::Or = expr.operator.item {
            let short_circuit_label = self.make_logic_label("or", "true", expr.operator.token_id);
            let end_label = self.make_logic_label("or", "end", expr.operator.token_id);

            self.emit_with_message(
                InstructionKind::JumpIfNotZero(left_val, short_circuit_label.clone()),
                token_id,
                "(||: lhs)",
            );
            let right_val = self.visit_expr(&expr.right);
            let dest_var = self.make_var();

            self.emit_many(
                token_id,
                [
                    (
                        InstructionKind::JumpIfNotZero(
                            right_val.clone(),
                            short_circuit_label.clone(),
                        ),
                        "(||: rhs)",
                    ),
                    (
                        InstructionKind::Copy {
                            dest: dest_var.clone(),
                            src: Value::Constant(0),
                        },
                        "(||) false",
                    ),
                    (InstructionKind::Jump(end_label.clone()), "(||) end"),
                    (InstructionKind::Label(short_circuit_label), ""),
                    (
                        InstructionKind::Copy {
                            dest: dest_var.clone(),
                            src: Value::Constant(1),
                        },
                        "(||) true",
                    ),
                    (InstructionKind::Label(end_label), ""),
                ],
            );

            dest_var
        } else {
            let right_val = self.visit_expr(&expr.right);
            let dest_var = self.make_var();
            self.emit(
                InstructionKind::Binary {
                    op: expr.operator.item,
                    dest: dest_var.clone(),
                    left: left_val,
                    right: right_val,
                },
                token_id,
            );
            dest_var
        }
    }

    fn visit_conditional_expr(&mut self, expr: &expr::ConditionalExpr) -> Value {
        let cond_val = self.visit_expr(&expr.condition);
        let dest_var = self.make_var();
        let else_label = self.make_logic_label("conditional", "else", expr.then_expr.token_id);
        let end_label = self.make_logic_label("conditional", "end", expr.else_expr.token_id);

        self.emit_with_message(
            InstructionKind::JumpIfZero(cond_val.clone(), else_label.clone()),
            expr.then_expr.token_id,
            "(?:) condition",
        );

        let then_val = self.visit_expr(&expr.then_expr);
        self.emit_with_message(
            InstructionKind::Copy {
                dest: dest_var.clone(),
                src: then_val,
            },
            expr.then_expr.token_id,
            "(?:) copy result of <then> into result of (?:)",
        );
        self.emit_with_message(
            InstructionKind::Jump(end_label.clone()),
            expr.then_expr.token_id,
            "(?:) jump to end after <then>",
        );

        self.emit(InstructionKind::Label(else_label), expr.else_expr.token_id);
        let else_val = self.visit_expr(&expr.else_expr);
        self.emit_with_message(
            InstructionKind::Copy {
                dest: dest_var.clone(),
                src: else_val,
            },
            expr.else_expr.token_id,
            "(?:) copy result of <else> into result of (?:)",
        );
        self.emit(InstructionKind::Label(end_label), expr.else_expr.token_id);
        dest_var
    }

    fn visit_constant(&mut self, expr: &WithToken<Literal>) -> Value {
        match expr.item {
            Literal::Integer(i) => Value::Constant(i),
            _ => unimplemented!(),
        }
    }

    fn visit_unary_expr(&mut self, expr: &expr::UnaryExpr) -> Value {
        let operand_val = self.visit_expr(&expr.operand);
        let dest_var = self.make_var();
        let postfix = expr.postfix;

        if let UnaryOp::Increment | UnaryOp::Decrement = *expr.operator {
            let (new_op, op_str, converted_op_str) = if let UnaryOp::Increment = *expr.operator {
                (BinaryOp::Add, "++", "+=")
            } else {
                (BinaryOp::Subtract, "--", "-=")
            };

            if postfix {
                self.emit_many(
                    expr.operator.token_id,
                    [
                        (
                            InstructionKind::Copy {
                                dest: dest_var.clone(),
                                src: operand_val.clone(),
                            },
                            &format!("(p{0}) copy final result of {0}", op_str),
                        ),
                        (
                            InstructionKind::Binary {
                                op: new_op,
                                dest: operand_val.clone(),
                                left: operand_val.clone(),
                                right: Value::Constant(1),
                            },
                            &format!("(p{}) {} {} 1", op_str, operand_val, converted_op_str),
                        ),
                    ],
                );
            } else {
                self.emit_many(
                    expr.operator.token_id,
                    [
                        (
                            InstructionKind::Binary {
                                op: new_op,
                                dest: operand_val.clone(),
                                left: operand_val.clone(),
                                right: Value::Constant(1),
                            },
                            &format!("({}p) {} {} 1", op_str, operand_val, converted_op_str),
                        ),
                        (
                            InstructionKind::Copy {
                                dest: dest_var.clone(),
                                src: operand_val,
                            },
                            &format!("({0}p) copy final result of {0}", op_str),
                        ),
                    ],
                );
            }
        } else {
            self.emit(
                InstructionKind::Unary {
                    op: expr.operator.item,
                    dest: dest_var.clone(),
                    src: operand_val,
                },
                expr.operator.token_id,
            );
        }
        dest_var
    }

    fn visit_variable(&mut self, var: &WithToken<crate::symbol::Symbol>) -> Value {
        let var_name = self.symbol_table.resolve_assert(**var);
        Value::Var(var_name.to_string())
    }
}

impl<'a> StmtRefVisitor<()> for TackyGen<'a> {
    fn visit_expr_stmt(&mut self, stmt: &expr::Expr) -> () {
        self.visit_expr(stmt);
    }

    fn visit_goto_stmt(&mut self, stmt: &WithToken<String>) -> () {
        let label = &stmt.item;
        self.emit(InstructionKind::Jump(label.to_string()), stmt.token_id);
    }

    fn visit_if_stmt(&mut self, stmt: &ast::stmt::IfStmt) -> () {
        let cond_val = self.visit_expr(&stmt.condition);
        let else_token_id = stmt
            .else_stmt
            .as_ref()
            .map(|else_stmt| else_stmt.token_id)
            .unwrap_or(stmt.condition.token_id);
        let else_label = self.make_logic_label("if", "_else", else_token_id);
        let end_label = self.make_logic_label("if", "end", stmt.condition.token_id);

        self.emit_with_message(
            InstructionKind::JumpIfZero(cond_val.clone(), else_label.clone()),
            stmt.condition.token_id,
            &format!("(if) jump to else if {} is false", cond_val),
        );

        self.visit_stmt(&stmt.then_stmt);
        self.emit_with_message(
            InstructionKind::Jump(end_label.clone()),
            stmt.condition.token_id,
            "(if) jump to end after then",
        );

        self.emit(InstructionKind::Label(else_label), stmt.condition.token_id);
        if let Some(else_stmt) = &stmt.else_stmt {
            self.visit_stmt(else_stmt);
        }

        self.emit(InstructionKind::Label(end_label), stmt.condition.token_id);
    }

    fn visit_label_stmt(&mut self, stmt: &Label) -> () {
        let label = &stmt.name.item;
        self.emit(
            InstructionKind::Label(label.to_string()),
            stmt.name.token_id,
        );
        self.visit_stmt(&stmt.next_stmt);
    }

    fn visit_null_stmt(&mut self) -> () {}

    fn visit_return_stmt(&mut self, stmt: &WithToken<Option<expr::Expr>>) -> () {
        if let Some(ret_expr) = &stmt.item {
            let ret_val = self.visit_expr(ret_expr);
            self.emit(InstructionKind::Return(ret_val), stmt.token_id);
        } else {
            unimplemented!()
        }
    }
}

impl<'a> ASTRefVisitor for TackyGen<'a> {
    type ProgramResult = tacky::Program;
    type FunctionDefResult = tacky::FunctionDef;
    type StmtResult = ();
    type ExprResult = Value;
    type BlockItemResult = ();
    type VarDeclResult = ();

    fn visit_function_def(&mut self, func_def: &ast::FunctionDef) -> Self::FunctionDefResult {
        assert!(self.current_body.is_empty());

        if func_def.body.is_none() {
            unimplemented!();
        }

        for item in func_def.body.as_ref().unwrap() {
            self.visit_block_item(item);
        }

        if func_def.name.item == "main" {
            self.emit(
                InstructionKind::Return(Value::Constant(0)),
                func_def.name.token_id,
            );
        }

        let line_col = self
            .source_file
            .line_col_token_begin(func_def.name.token_id);
        tacky::FunctionDef {
            name: func_def.name.item.clone(),
            body: mem::replace(&mut self.current_body, Vec::new()),
            pos: line_col,
        }
    }

    fn visit_program(&mut self, program: &ast::Program) -> Self::ProgramResult {
        assert!(self.current_body.is_empty());

        let mut function_defs = Vec::new();
        for func_def in &program.function_defs {
            let tacky_func_def = self.visit_function_def(func_def);
            function_defs.push(tacky_func_def);
        }

        // only use the first function for now
        tacky::Program(function_defs.remove(0))
    }

    fn visit_block_item(&mut self, item: &ast::BlockItem) -> Self::BlockItemResult {
        match &item {
            ast::BlockItem::Decl(var_decl) => self.visit_var_decl(var_decl),
            ast::BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDeclaration) -> Self::VarDeclResult {
        if let Some(initializer) = &var_decl.initializer {
            let src = self.visit_expr(&*initializer);
            let dst = self.visit_variable(&var_decl.name);
            self.emit_with_message(
                InstructionKind::Copy { dest: dst, src },
                var_decl.name.token_id,
                "(var_decl with initializer)",
            );
        }
    }
}
