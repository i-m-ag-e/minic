use std::mem;

use crate::{
    ast::{
        self, ASTVisitor,
        expr::{self, BinaryOp, ExprVisitor, UnaryOp},
        stmt::{BreakTarget, ForStmtInit, Label, LoopID, StmtVisitor},
    },
    lexer::token::{Literal, TokenID},
    resolver::var_map::ScopedVarMap,
    semantic_analyzer::SwitchMap,
    source_file::SourceFile,
    tacky::{self, Instruction, InstructionKind, Value},
    with_token::WithToken,
};

#[derive(Debug, Clone)]
pub struct TackyGen<'a> {
    pub var_count: usize,
    pub current_body: Vec<Instruction>,
    source_file: &'a SourceFile,
    switch_map: SwitchMap,
    switch_stack: Vec<LoopID>,
    symbol_table: &'a ScopedVarMap,
}

impl<'a> TackyGen<'a> {
    pub fn new(
        source_file: &'a SourceFile,
        switch_map: SwitchMap,
        symbol_table: &'a ScopedVarMap,
    ) -> Self {
        Self {
            var_count: 0,
            current_body: Vec::new(),
            source_file,
            switch_map,
            switch_stack: Vec::new(),
            symbol_table,
        }
    }

    fn make_var(&mut self) -> Value {
        let var_id = self.var_count;
        self.var_count += 1;
        Value::Var(format!("pv.{}", var_id))
    }

    fn make_case_label(&self, token_id: TokenID, switch_id: LoopID, default: bool) -> String {
        let line_col = self.source_file.line_col_token_begin(token_id);
        format!(
            "switch.{}_{}.{}_{}",
            switch_id,
            if default { "default" } else { "case" },
            line_col.0,
            line_col.1
        )
    }

    fn make_logic_label(&self, op: &str, stage: &str, token_id: TokenID) -> String {
        let line_col = self.source_file.line_col_token_begin(token_id);
        format!("{}_{}.{}_{}", op, stage, line_col.0, line_col.1)
    }

    fn make_loop_control_label(&self, op: &str, loop_id: LoopID) -> String {
        format!("{}_loop.{}", op, loop_id)
    }

    fn make_loop_label(&self, loop_type: &str, loop_id: LoopID, token_id: TokenID) -> String {
        let line_col = self.source_file.line_col_token_begin(token_id);
        format!(
            "{}.loop.{}_{}.{}",
            loop_type, loop_id, line_col.0, line_col.1
        )
    }

    fn make_switch_break_label(&self, switch_id: LoopID) -> String {
        format!("break_switch.{}", switch_id)
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
        message: &str,
        instruction: InstructionKind,
        token_id: TokenID,
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
                self.emit_with_message(message, instruction, token_id);
            } else {
                self.emit(instruction, token_id);
            }
        }
    }
}

impl<'a> ExprVisitor<Value> for TackyGen<'a> {
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
                "(&&: lhs)",
                InstructionKind::JumpIfZero(left_val, short_circuit_label.clone()),
                token_id,
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
                "(||: lhs)",
                InstructionKind::JumpIfNotZero(left_val, short_circuit_label.clone()),
                token_id,
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
            "(?:) condition",
            InstructionKind::JumpIfZero(cond_val.clone(), else_label.clone()),
            expr.then_expr.token_id,
        );

        let then_val = self.visit_expr(&expr.then_expr);
        self.emit_with_message(
            "(?:) copy result of <then> into result of (?:)",
            InstructionKind::Copy {
                dest: dest_var.clone(),
                src: then_val,
            },
            expr.then_expr.token_id,
        );
        self.emit_with_message(
            "(?:) jump to end after <then>",
            InstructionKind::Jump(end_label.clone()),
            expr.then_expr.token_id,
        );

        self.emit(InstructionKind::Label(else_label), expr.else_expr.token_id);
        let else_val = self.visit_expr(&expr.else_expr);
        self.emit_with_message(
            "(?:) copy result of <else> into result of (?:)",
            InstructionKind::Copy {
                dest: dest_var.clone(),
                src: else_val,
            },
            expr.else_expr.token_id,
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

impl<'a> StmtVisitor<()> for TackyGen<'a> {
    fn visit_break(&mut self, stmt: &ast::stmt::BreakStmt) -> () {
        match stmt.0.item {
            BreakTarget::Loop(id) => self.emit_with_message(
                &format!("break out of loop with id {}", id),
                InstructionKind::Jump(self.make_loop_control_label("break", id)),
                stmt.0.token_id,
            ),
            BreakTarget::Switch(id) => self.emit_with_message(
                &format!("break out of switch with id {}", id),
                InstructionKind::Jump(self.make_switch_break_label(id)),
                stmt.0.token_id,
            ),
        };
    }

    fn visit_case(&mut self, stmt: &ast::stmt::CaseStmt) -> () {
        let switch_id = *self
            .switch_stack
            .last()
            .expect("case outside a switch cannot exist at the TACKY gen stage");
        let case_label = self.make_case_label(stmt.value.token_id, switch_id, false);
        self.emit(InstructionKind::Label(case_label), stmt.value.token_id);

        self.visit_stmt(&stmt.next_stmt);
    }

    fn visit_compound(&mut self, block: &ast::Block) -> () {
        self.visit_block(block)
    }

    fn visit_continue(&mut self, stmt: &ast::stmt::ContinueStmt) -> () {
        self.emit_with_message(
            &format!("continue loop with id {}", stmt.0.item),
            InstructionKind::Jump(self.make_loop_control_label("continue", stmt.0.item)),
            stmt.0.token_id,
        );
    }

    fn visit_default(&mut self, stmt: &ast::stmt::DefaultStmt) -> () {
        let switch_id = *self
            .switch_stack
            .last()
            .expect("case outside a switch cannot exist at the TACKY gen stage");
        let default_label = self.make_case_label(stmt.default_token.token_id, switch_id, true);
        self.emit(
            InstructionKind::Label(default_label),
            stmt.default_token.token_id,
        );

        self.visit_stmt(&stmt.next_stmt);
    }

    fn visit_do_while_stmt(&mut self, stmt: &ast::stmt::DoWhileStmt) -> () {
        let loop_label = self.make_loop_label("do_while", stmt.loop_id, stmt.body.token_id);
        let break_label = self.make_loop_control_label("break", stmt.loop_id);
        let continue_label = self.make_loop_control_label("continue", stmt.loop_id);

        self.emit_with_message(
            &format!("loop id {}", stmt.loop_id),
            InstructionKind::Label(loop_label.clone()),
            stmt.body.token_id,
        );
        self.visit_stmt(&stmt.body);
        self.emit(InstructionKind::Label(continue_label), stmt.body.token_id);
        let condition_result = self.visit_expr(&stmt.condition);

        self.emit_with_message(
            &format!(
                "if {}, jump back to beginning of loop with id {}",
                condition_result, stmt.loop_id
            ),
            InstructionKind::JumpIfNotZero(condition_result, loop_label),
            stmt.condition.token_id,
        );
        self.emit(InstructionKind::Label(break_label), stmt.condition.token_id);
    }

    fn visit_expr_stmt(&mut self, stmt: &expr::Expr) -> () {
        self.visit_expr(stmt);
    }

    fn visit_for_stmt(&mut self, stmt: &ast::stmt::ForStmt) -> () {
        let loop_label = self.make_loop_label("for", stmt.loop_id, stmt.initializer.token_id);
        let break_label = self.make_loop_control_label("break", stmt.loop_id);
        let continue_label = self.make_loop_control_label("continue", stmt.loop_id);

        match &*stmt.initializer {
            Some(ForStmtInit::Declaration(decls)) => {
                decls.iter().for_each(|decl| self.visit_var_decl(decl));
            }
            Some(ForStmtInit::Expression(expr)) => {
                self.visit_expr(expr);
            }
            None => {}
        };

        self.emit_with_message(
            &format!("loop id {}", stmt.loop_id),
            InstructionKind::Label(loop_label.clone()),
            stmt.initializer.token_id,
        );

        let condition_result = stmt
            .condition
            .item
            .as_ref()
            .map(|cond| self.visit_expr(cond))
            .unwrap_or(Value::Constant(1));
        self.emit_with_message(
            &format!("if !{}, jump to end of loop", condition_result),
            InstructionKind::JumpIfZero(condition_result, break_label.clone()),
            stmt.condition.token_id,
        );

        self.visit_stmt(&stmt.body);

        self.emit(
            InstructionKind::Label(continue_label),
            stmt.initializer.token_id,
        );
        if let Some(step) = &stmt.step.item {
            self.visit_expr(step);
        }

        self.emit_with_message(
            &format!("jump to start of loop with id {}", stmt.loop_id),
            InstructionKind::Jump(loop_label),
            stmt.initializer.token_id,
        );
        self.emit(
            InstructionKind::Label(break_label),
            stmt.initializer.token_id,
        );
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
            &format!("(if) jump to else if {} is false", cond_val),
            InstructionKind::JumpIfZero(cond_val, else_label.clone()),
            stmt.condition.token_id,
        );

        self.visit_stmt(&stmt.then_stmt);
        self.emit_with_message(
            "(if) jump to end after then",
            InstructionKind::Jump(end_label.clone()),
            stmt.condition.token_id,
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

    fn visit_switch_stmt(&mut self, stmt: &ast::stmt::SwitchStmt) -> () {
        let condition_result = self.visit_expr(&stmt.condition);
        let break_label = self.make_switch_break_label(stmt.switch_id);
        let Some(switch_data) = self.switch_map.remove(&stmt.switch_id) else {
            unreachable!()
        };

        for &case in &switch_data.cases {
            let Literal::Integer(int) = case.item else {
                unimplemented!()
            };
            self.emit_with_message(
                &format!(
                    "case comparison `{}` == `{}` (switch id {})",
                    condition_result, int, stmt.switch_id
                ),
                InstructionKind::JumpIfEqual {
                    lhs: condition_result.clone(),
                    rhs: Value::Constant(int),
                    label: self.make_case_label(case.token_id, stmt.switch_id, false),
                },
                case.token_id,
            );
        }

        if let Some(default) = switch_data.default {
            let default_label = self.make_case_label(default.token_id, stmt.switch_id, true);
            self.emit_with_message(
                &format!("jmp to default case (switch id {})", stmt.switch_id),
                InstructionKind::Jump(default_label),
                stmt.condition.token_id,
            );
        } else {
            self.emit_with_message(
                &format!("no match with cases, jump to end of switch"),
                InstructionKind::Jump(break_label.clone()),
                stmt.condition.token_id,
            );
        }

        self.switch_stack.push(stmt.switch_id);
        self.visit_stmt(&stmt.body);
        self.switch_stack
            .pop()
            .expect("switch stack cannot be empty while popping");

        self.emit(InstructionKind::Label(break_label), stmt.condition.token_id);
    }

    fn visit_while_stmt(&mut self, stmt: &ast::stmt::WhileStmt) -> () {
        let loop_label = self.make_loop_label("while", stmt.loop_id, stmt.condition.token_id);
        let break_label = self.make_loop_control_label("break", stmt.loop_id);
        let continue_label = self.make_loop_control_label("continue", stmt.loop_id);

        self.emit_with_message(
            &format!("loop id {}", stmt.loop_id),
            InstructionKind::Label(loop_label.clone()),
            stmt.condition.token_id,
        );
        self.emit(
            InstructionKind::Label(continue_label),
            stmt.condition.token_id,
        );

        let condition_result = self.visit_expr(&stmt.condition);
        self.emit_with_message(
            &format!("if !{}, jump to end of loop", condition_result),
            InstructionKind::JumpIfZero(condition_result, break_label.clone()),
            stmt.condition.token_id,
        );
        self.visit_stmt(&stmt.body);

        self.emit_with_message(
            &format!("jump back to start of loop with id {}", stmt.loop_id),
            InstructionKind::Jump(loop_label),
            stmt.condition.token_id,
        );
        self.emit(InstructionKind::Label(break_label), stmt.condition.token_id);
    }
}

impl<'a> ASTVisitor for TackyGen<'a> {
    type ProgramResult = tacky::Program;
    type FunctionDefResult = tacky::FunctionDef;
    type StmtResult = ();
    type ExprResult = Value;
    type BlockItemResult = ();
    type BlockResult = ();
    type VarDeclResult = ();

    fn visit_function_def(&mut self, func_def: &ast::FunctionDef) -> Self::FunctionDefResult {
        assert!(self.current_body.is_empty());

        if func_def.body.is_none() {
            unimplemented!();
        }

        self.visit_block(func_def.body.as_ref().unwrap());

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
            ast::BlockItem::Decl(var_decls) => var_decls
                .iter()
                .for_each(|var_decl| self.visit_var_decl(var_decl)),
            ast::BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_block(&mut self, block: &ast::Block) -> Self::BlockResult {
        block
            .body
            .iter()
            .for_each(|block_item| self.visit_block_item(block_item));
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDeclaration) -> Self::VarDeclResult {
        if let Some(initializer) = &var_decl.initializer {
            let src = self.visit_expr(&*initializer);
            let dst = self.visit_variable(&var_decl.name);
            self.emit_with_message(
                "(var_decl with initializer)",
                InstructionKind::Copy { dest: dst, src },
                var_decl.name.token_id,
            );
        }
    }
}
