use std::mem;

use crate::{
    ast::{
        self, ASTRefVisitor,
        expr::{self, BinaryOp, ExprRefVisitor},
        stmt::StmtRefVisitor,
    },
    lexer::token::{Literal, TokenID},
    source_file::SourceFile,
    tacky::{self, Instruction, InstructionKind, Value},
    with_token::WithToken,
};

#[derive(Debug, Clone)]
pub struct TackyGen<'a> {
    pub var_count: usize,
    pub current_body: Vec<Instruction>,
    source_file: &'a SourceFile,
    // current_line: usize,
}

impl<'a> TackyGen<'a> {
    pub fn new(source_file: &'a SourceFile) -> Self {
        Self {
            var_count: 0,
            current_body: Vec::new(),
            source_file,
            // current_line: 0,
        }
    }

    fn make_var(&mut self) -> Value {
        let var_id = self.var_count;
        self.var_count += 1;
        Value::Var(var_id)
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

    fn visit_constant(&mut self, expr: &WithToken<Literal>) -> Value {
        match expr.item {
            Literal::Integer(i) => Value::Constant(i),
            _ => unimplemented!(),
        }
    }

    fn visit_unary_expr(&mut self, expr: &expr::UnaryExpr) -> Value {
        let operand_val = self.visit_expr(&expr.operand);
        let dest_var = self.make_var();
        self.emit(
            InstructionKind::Unary {
                op: expr.operator.item,
                dest: dest_var.clone(),
                src: operand_val,
            },
            expr.operator.token_id,
        );
        dest_var
    }
}

impl<'a> StmtRefVisitor<()> for TackyGen<'a> {
    fn visit_expr_stmt(&mut self, stmt: &expr::Expr) -> () {
        self.visit_expr(stmt);
    }

    fn visit_return_stmt(&mut self, stmt: &Option<expr::Expr>) -> () {
        if let Some(ret_expr) = stmt {
            let ret_val = self.visit_expr(ret_expr);
            self.emit(InstructionKind::Return(ret_val), ret_expr.token());
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

    fn visit_function_def(&mut self, func_def: &ast::FunctionDef) -> Self::FunctionDefResult {
        assert!(self.current_body.is_empty());

        if func_def.body.is_none() {
            unimplemented!();
        }

        for stmt in func_def.body.as_ref().unwrap() {
            self.visit_stmt(stmt);
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
}
