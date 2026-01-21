use crate::{
    ast::{
        self, ASTRefVisitor,
        expr::{BinaryExpr, Expr, ExprRefVisitor, UnaryExpr},
        stmt::StmtRefVisitor,
    },
    lexer::token::Literal,
    with_token::WithToken,
};

const INDENT: &str = "    ";

#[derive(Debug, Clone)]
pub struct Program {
    function_defs: Vec<FunctionDef>,
}

impl Program {
    fn new() -> Self {
        Self {
            function_defs: Vec::new(),
        }
    }

    pub fn to_string(&self) -> String {
        let mut asm_str = String::new();
        for func in &self.function_defs {
            asm_str.push_str(&func.to_string());
        }
        asm_str.push_str("\n    .section .note.GNU-stack,\"\",@progbits\n");
        asm_str
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    name: String,
    body: Option<Vec<Instruction>>,
}

impl FunctionDef {
    fn to_string(&self) -> String {
        let mut func_str = format!("{}.globl {}\n", INDENT, self.name);
        func_str.push_str(&format!("{}:\n", self.name));
        if let Some(body) = &self.body {
            for instr in body {
                func_str.push_str(&format!("{}{}\n", INDENT, instr.to_string()));
            }
        }
        func_str
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov { src: Operand, dest: Operand },
    Ret,
}

impl Instruction {
    fn to_string(&self) -> String {
        match self {
            Instruction::Mov { src, dest } => {
                format!("movq\t\t{}, {}", src.to_string(), dest.to_string())
            }
            Instruction::Ret => "ret".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i64),
    Register,
}

impl Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Imm(value) => format!("${}", value),
            Operand::Register => "%rax".to_string(),
        }
    }
}
pub struct AsmGen {
    pub current_body: Vec<Instruction>,
}

impl AsmGen {
    pub fn new() -> Self {
        Self {
            current_body: Vec::new(),
        }
    }

    pub fn generate(&mut self, program: &ast::Program) -> Program {
        self.visit_program(program)
    }

    fn add_instruction(&mut self, instr: Instruction) {
        self.current_body.push(instr);
    }
}

impl ExprRefVisitor<Operand> for AsmGen {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Operand {
        unimplemented!()
    }

    fn visit_group_expr(&mut self, expr: &WithToken<Expr>) -> Operand {
        unimplemented!()
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Operand {
        unimplemented!()
    }

    fn visit_constant(&mut self, expr: &WithToken<Literal>) -> Operand {
        let value = match expr.item {
            Literal::Integer(i) => i,
            _ => panic!("Unsupported literal for assembly generation"),
        };
        Operand::Imm(value)
    }
}

impl StmtRefVisitor<()> for AsmGen {
    fn visit_expr_stmt(&mut self, stmt: &Expr) -> () {
        unimplemented!()
    }

    fn visit_return_stmt(&mut self, stmt: &Option<Expr>) -> () {
        if let Some(expr) = stmt {
            let op = self.visit_expr(expr);
            self.add_instruction(Instruction::Mov {
                src: op,
                dest: Operand::Register,
            });
        }
        self.add_instruction(Instruction::Ret);
    }
}

impl ASTRefVisitor for AsmGen {
    type StmtResult = ();
    type ExprResult = Operand;
    type ProgramResult = Program;
    type FunctionDefResult = ();

    fn visit_program(&mut self, program: &ast::Program) -> Self::ProgramResult {
        for func_def in &program.function_defs {
            self.visit_function_def(func_def);
        }
        let mut prog = Program::new();
        prog.function_defs.push(FunctionDef {
            name: "main".to_string(),
            body: Some(self.current_body.clone()),
        });
        prog
    }

    fn visit_function_def(&mut self, func_def: &ast::FunctionDef) -> Self::StmtResult {
        self.current_body.clear();
        if let Some(body) = &func_def.body {
            for stmt in body {
                self.visit_stmt(stmt);
            }
        }
    }
}
