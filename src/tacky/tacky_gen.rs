use std::mem;

use crate::{
    ast::{
        self, ASTRefVisitor,
        expr::{self, BinaryOp, ExprRefVisitor},
        stmt::StmtRefVisitor,
    },
    lexer::token::{Literal, TokenID},
    source_file::SourceFile,
    tacky::{self, Instruction, Value},
    with_token::WithToken,
};

#[derive(Debug, Clone)]
pub struct TackyGen<'a> {
    pub var_count: usize,
    pub current_body: Vec<Instruction>,
    source_file: &'a SourceFile,
}

impl<'a> TackyGen<'a> {
    pub fn new(source_file: &'a SourceFile) -> Self {
        Self {
            var_count: 0,
            current_body: Vec::new(),
            source_file,
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
}

impl<'a> ExprRefVisitor<Value> for TackyGen<'a> {
    fn visit_binary_expr(&mut self, expr: &expr::BinaryExpr) -> Value {
        let left_val = self.visit_expr(&expr.left);

        if let BinaryOp::And | BinaryOp::Or = expr.operator.item {
            type JumpType = fn(Value, String) -> Instruction;
            let (op_type, jump_type, short_circuit_ans, short_circuit_on) =
                if let BinaryOp::And = expr.operator.item {
                    ("and", Instruction::JumpIfZero as JumpType, false, "false")
                } else {
                    ("or", Instruction::JumpIfNotZero as JumpType, true, "true")
                };

            let short_circuit_label =
                self.make_logic_label(op_type, short_circuit_on, expr.operator.token_id);
            let end_label = self.make_logic_label(op_type, "end", expr.operator.token_id);

            self.current_body
                .push(jump_type(left_val, short_circuit_label.clone()));
            let right_val = self.visit_expr(&expr.right);
            let dest_var = self.make_var();

            self.current_body.extend_from_slice(&[
                jump_type(right_val.clone(), short_circuit_label.clone()),
                Instruction::Copy {
                    dest: dest_var.clone(),
                    src: Value::Constant(!short_circuit_ans as i64),
                },
                Instruction::Jump(end_label.clone()),
                Instruction::Label(short_circuit_label),
                Instruction::Copy {
                    dest: dest_var.clone(),
                    src: Value::Constant(short_circuit_ans as i64),
                },
                Instruction::Label(end_label),
            ]);
            dest_var
        } else {
            let right_val = self.visit_expr(&expr.right);
            let dest_var = self.make_var();
            self.current_body.push(Instruction::Binary {
                op: expr.operator.item,
                dest: dest_var.clone(),
                left: left_val,
                right: right_val,
            });
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
        self.current_body.push(Instruction::Unary {
            op: expr.operator.item,
            dest: dest_var.clone(),
            src: operand_val,
        });
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
            self.current_body.push(Instruction::Return(ret_val));
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

        tacky::FunctionDef {
            name: func_def.name.item.clone(),
            body: mem::replace(&mut self.current_body, Vec::new()),
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
