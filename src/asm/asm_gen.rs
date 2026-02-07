use super::{FunctionDef, Instruction, Operand, Program, Register};
use crate::{
    asm::Condition,
    ast::expr::{BinaryOp, UnaryOp},
    tacky,
};

macro_rules! replace_pseudo_operand {
    ($stack_offset: expr, $operand: expr, $pseudo_map: expr) => {
        if let Operand::Pseudo(var_id) = $operand {
            let var_id = *var_id;
            if let Some(offset) = $pseudo_map.get(&var_id) {
                *$operand = Operand::Stack(*offset);
            } else {
                $stack_offset += 8;
                $pseudo_map.insert(var_id, -$stack_offset);
                *$operand = Operand::Stack(-$stack_offset);
            }
        }
    };
}

macro_rules! replace_instruction {
    ( $arr: expr; [$index: expr] => $first_inst: expr, $( $rest: expr ),*) => {{
        $arr[$index] = $first_inst;

        $(
            $index += 1;
            $arr.insert($index, $rest);
        )*
    }};
}

pub fn tacky_to_asm(tacky_prog: &tacky::Program) -> Program {
    let mut asm_program = Program::new();

    let mut asm_func = FunctionDef {
        name: tacky_prog.0.name.clone(),
        stack_size: 0,
        body: vec![Instruction::AllocateStack(0)],
    };

    for instr in &tacky_prog.0.body {
        tacky_instr_to_asm(instr, &mut asm_func.body);
    }

    asm_program.function_defs.push(asm_func);

    resolve_pseudo_operands(&mut asm_program);
    alloc_stack_and_resolve_stack_ops(&mut asm_program);
    asm_program
}

fn binary_op_to_condition(op: BinaryOp) -> Condition {
    match op {
        BinaryOp::Greater => Condition::G,
        BinaryOp::GreaterEqual => Condition::GE,
        BinaryOp::LessThan => Condition::L,
        BinaryOp::LessEqual => Condition::LE,
        BinaryOp::Equal => Condition::E,
        BinaryOp::NotEqual => Condition::NE,
        _ => panic!("Unsupported binary operation for condition code"),
    }
}

fn shrink_register_operand(operand: Operand) -> Operand {
    if let Operand::Register(reg) = operand {
        match reg {
            Register::AX => Operand::Register(Register::AL),
            Register::CX => Operand::Register(Register::CL),
            Register::DX => Operand::Register(Register::DL),
            Register::R10 => Operand::Register(Register::R10B),
            Register::R11 => Operand::Register(Register::R11B),
            _ => operand,
        }
    } else {
        operand
    }
}

fn tacky_instr_to_asm(instr: &tacky::Instruction, body: &mut Vec<Instruction>) {
    match instr {
        tacky::Instruction::Binary {
            op:
                op @ (BinaryOp::Greater
                | BinaryOp::GreaterEqual
                | BinaryOp::LessEqual
                | BinaryOp::LessThan
                | BinaryOp::Equal
                | BinaryOp::NotEqual),
            dest,
            left,
            right,
        } => {
            let left = tacky_value_to_operand(left);
            let right = tacky_value_to_operand(right);
            let dest = tacky_value_to_operand(dest);

            body.push(Instruction::Cmp(right, left));
            body.push(Instruction::Mov {
                src: Operand::Imm(0),
                dest: dest.clone(),
            });
            body.push(Instruction::SetCC(
                binary_op_to_condition(*op),
                shrink_register_operand(dest),
            ));
        }
        tacky::Instruction::Binary {
            op,
            dest,
            left,
            right,
        } => {
            let left = tacky_value_to_operand(left);
            let right = tacky_value_to_operand(right);
            let dest = tacky_value_to_operand(dest);

            if let BinaryOp::Divide | BinaryOp::Modulus = op {
                body.push(Instruction::Mov {
                    src: left,
                    dest: Operand::Register(Register::AX),
                });
                body.push(Instruction::Cqo);
                body.push(Instruction::Idiv(right));
                body.push(Instruction::Mov {
                    src: match op {
                        BinaryOp::Divide => Operand::Register(Register::AX),
                        BinaryOp::Modulus => Operand::Register(Register::DX),
                        _ => unreachable!(),
                    },
                    dest,
                });
            } else {
                body.push(Instruction::Mov {
                    src: left,
                    dest: dest.clone(),
                });
                body.push(Instruction::Binary {
                    op: *op,
                    src: right,
                    dest: dest.clone(),
                });
            }
        }
        tacky::Instruction::Copy { dest, src } => {
            let src = tacky_value_to_operand(src);
            let dest = tacky_value_to_operand(dest);
            body.push(Instruction::Mov { src, dest });
        }
        tacky::Instruction::Jump(label) => {
            body.push(Instruction::Jmp(label.clone()));
        }
        tacky::Instruction::JumpIfNotZero(cond, label) => {
            let cond = tacky_value_to_operand(cond);
            body.push(Instruction::Cmp(cond, Operand::Imm(0)));
            body.push(Instruction::JmpCC(Condition::NE, label.clone()));
        }
        tacky::Instruction::JumpIfZero(cond, label) => {
            let cond = tacky_value_to_operand(cond);
            body.push(Instruction::Cmp(cond, Operand::Imm(0)));
            body.push(Instruction::JmpCC(Condition::E, label.clone()));
        }
        tacky::Instruction::Label(label) => {
            body.push(Instruction::Label(label.clone()));
        }
        tacky::Instruction::Return(value) => {
            let operand = tacky_value_to_operand(value);
            body.push(Instruction::Mov {
                src: operand,
                dest: Operand::Register(Register::AX),
            });
            body.push(Instruction::Ret);
        }
        tacky::Instruction::Unary {
            op: UnaryOp::Not,
            dest,
            src,
        } => {
            let src_operand = tacky_value_to_operand(src);
            let dest_operand = tacky_value_to_operand(dest);
            body.extend_from_slice(&[
                Instruction::Cmp(src_operand, Operand::Imm(0)),
                Instruction::Mov {
                    src: Operand::Imm(0),
                    dest: dest_operand.clone(),
                },
                Instruction::SetCC(Condition::E, dest_operand),
            ]);
        }
        tacky::Instruction::Unary { op, dest, src } => {
            let src_operand = tacky_value_to_operand(src);
            let dest_operand = tacky_value_to_operand(dest);
            body.push(Instruction::Mov {
                src: src_operand,
                dest: dest_operand.clone(),
            });
            body.push(Instruction::Unary(*op, dest_operand));
        }
    }
}

fn tacky_value_to_operand(value: &tacky::Value) -> Operand {
    match value {
        tacky::Value::Constant(c) => Operand::Imm(*c),
        tacky::Value::Var(var_id) => Operand::Pseudo(*var_id),
    }
}

fn resolve_pseudo_operands(prog: &mut Program) {
    for func in &mut prog.function_defs {
        let mut pseudo_map = std::collections::HashMap::new();
        let mut stack_offset: i32 = 0;

        for instr in &mut func.body {
            match instr {
                Instruction::AllocateStack(_)
                | Instruction::Cqo
                | Instruction::Label(_)
                | Instruction::Ret
                | Instruction::Jmp(_)
                | Instruction::JmpCC(_, _) => {}
                Instruction::Binary { op: _, src, dest } => {
                    replace_pseudo_operand!(stack_offset, src, pseudo_map);
                    replace_pseudo_operand!(stack_offset, dest, pseudo_map);
                }
                Instruction::Cmp(lhs, rhs) => {
                    replace_pseudo_operand!(stack_offset, lhs, pseudo_map);
                    replace_pseudo_operand!(stack_offset, rhs, pseudo_map);
                }
                Instruction::Idiv(operand) => {
                    replace_pseudo_operand!(stack_offset, operand, pseudo_map);
                }
                Instruction::Mov { src, dest } => {
                    replace_pseudo_operand!(stack_offset, src, pseudo_map);
                    replace_pseudo_operand!(stack_offset, dest, pseudo_map);
                }
                Instruction::SetCC(_, operand) => {
                    replace_pseudo_operand!(stack_offset, operand, pseudo_map);
                }
                Instruction::Unary(_, operand) => {
                    replace_pseudo_operand!(stack_offset, operand, pseudo_map);
                }
            }
        }

        func.stack_size = stack_offset;
    }
}

fn alloc_stack_and_resolve_stack_ops(prog: &mut Program) {
    for func in &mut prog.function_defs {
        func.body[0] = Instruction::AllocateStack(func.stack_size as usize);
        let mut index = 0;
        while index < func.body.len() {
            match func.body[index] {
                Instruction::Binary {
                    op: op @ (BinaryOp::LeftShift | BinaryOp::RightShift),
                    src: operand @ (Operand::Pseudo(_) | Operand::Register(_) | Operand::Stack(_)),
                    dest,
                } => {
                    replace_instruction!(func.body; [index] =>
                        Instruction::Mov {
                            src: operand,
                            dest: Operand::Register(Register::CX),
                        },
                        Instruction::Binary {
                            op,
                            src: Operand::Register(Register::CL),
                            dest: dest,
                        }
                    );
                }
                Instruction::Binary {
                    op:
                        op @ (BinaryOp::Add
                        | BinaryOp::Subtract
                        | BinaryOp::BitAnd
                        | BinaryOp::BitOr
                        | BinaryOp::BitXor),
                    src: Operand::Stack(src),
                    dest: Operand::Stack(dest),
                } => {
                    replace_instruction!(func.body; [index] =>
                        Instruction::Mov {
                            src: Operand::Stack(src),
                            dest: Operand::Register(Register::R10),
                        },
                        Instruction::Binary {
                            op: op,
                            src: Operand::Register(Register::R10),
                            dest: Operand::Stack(dest),
                        }
                    );
                }
                Instruction::Binary {
                    op: op @ BinaryOp::Multiply,
                    src,
                    dest: Operand::Stack(n),
                } => {
                    replace_instruction!(func.body; [index] =>
                        Instruction::Mov {
                            src: Operand::Stack(n),
                            dest: Operand::Register(Register::R11),
                        },
                        Instruction::Binary {
                            op: op,
                            src: src,
                            dest: Operand::Register(Register::R11),
                        },
                        Instruction::Mov {
                            src: Operand::Register(Register::R11),
                            dest: Operand::Stack(n),
                        }
                    )
                }
                Instruction::Cmp(lhs @ Operand::Stack(_), rhs @ Operand::Stack(_)) => {
                    replace_instruction!(func.body; [index] =>
                        Instruction::Mov {
                            src: lhs,
                            dest: Operand::Register(Register::R10),
                        },
                        Instruction::Cmp(Operand::Register(Register::R10), rhs)
                    );
                }
                Instruction::Cmp(lhs, rhs @ Operand::Imm(_)) => {
                    replace_instruction!(func.body; [index] =>
                        Instruction::Mov {
                            src: rhs,
                            dest: Operand::Register(Register::R11),
                        },
                        Instruction::Cmp(lhs, Operand::Register(Register::R11))
                    );
                }
                Instruction::Idiv(imm @ Operand::Imm(_)) => {
                    replace_instruction!(func.body; [index] =>
                        Instruction::Mov {
                            src: imm,
                            dest: Operand::Register(Register::R10),
                        },
                        Instruction::Idiv(Operand::Register(Register::R10))
                    );
                }
                Instruction::Mov {
                    src: Operand::Stack(src),
                    dest: Operand::Stack(dest),
                } => {
                    replace_instruction!(func.body; [index] =>
                        Instruction::Mov {
                            src: Operand::Stack(src),
                            dest: Operand::Register(Register::R10),
                        },
                        Instruction::Mov {
                            src: Operand::Register(Register::R10),
                            dest: Operand::Stack(dest),
                        }
                    );
                }
                _ => {}
            }
            index += 1;
        }
    }
}
