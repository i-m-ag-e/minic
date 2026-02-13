use super::{FunctionDef, Instruction, Operand, Program, Register};
use crate::{
    asm::{Condition, InstructionKind},
    ast::expr::{BinaryOp, UnaryOp},
    debug_info::DebugInfo,
    tacky,
};

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
    let func = &tacky_prog.0;

    let mut asm_func = FunctionDef {
        name: func.name.clone(),
        stack_size: 0,
        body: vec![Instruction {
            kind: InstructionKind::AllocateStack(0),
            debug_info: DebugInfo::new(func.pos.0, func.pos.1, String::new()),
        }],
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
    let tacky::Instruction { kind, debug_info } = instr;
    match kind {
        tacky::InstructionKind::Binary {
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

            body.push(Instruction {
                kind: InstructionKind::Cmp(right, left),
                debug_info: debug_info.clone(),
            });
            body.push(Instruction {
                kind: InstructionKind::Mov {
                    src: Operand::Imm(0),
                    dest: dest.clone(),
                },
                debug_info: debug_info.clone(),
            });
            body.push(Instruction {
                kind: InstructionKind::SetCC(
                    binary_op_to_condition(*op),
                    shrink_register_operand(dest),
                ),
                debug_info: debug_info.clone(),
            });
        }
        tacky::InstructionKind::Binary {
            op,
            dest,
            left,
            right,
        } => {
            let left = tacky_value_to_operand(left);
            let right = tacky_value_to_operand(right);
            let dest = tacky_value_to_operand(dest);

            if let BinaryOp::Divide | BinaryOp::Modulus = op {
                body.push(Instruction {
                    kind: InstructionKind::Mov {
                        src: left,
                        dest: Operand::Register(Register::AX),
                    },
                    debug_info: debug_info.clone(),
                });
                body.push(Instruction {
                    kind: InstructionKind::Cqo,
                    debug_info: debug_info.clone(),
                });
                body.push(Instruction {
                    kind: InstructionKind::Idiv(right),
                    debug_info: debug_info.clone(),
                });
                body.push(Instruction {
                    kind: InstructionKind::Mov {
                        src: match op {
                            BinaryOp::Divide => Operand::Register(Register::AX),
                            BinaryOp::Modulus => Operand::Register(Register::DX),
                            _ => unreachable!(),
                        },
                        dest,
                    },
                    debug_info: debug_info.clone(),
                });
            } else {
                body.push(Instruction {
                    kind: InstructionKind::Mov {
                        src: left,
                        dest: dest.clone(),
                    },
                    debug_info: debug_info.clone(),
                });
                body.push(Instruction {
                    kind: InstructionKind::Binary {
                        op: *op,
                        src: right,
                        dest: dest.clone(),
                    },
                    debug_info: debug_info.clone(),
                });
            }
        }
        tacky::InstructionKind::Copy { dest, src } => {
            let src = tacky_value_to_operand(src);
            let dest = tacky_value_to_operand(dest);
            body.push(Instruction {
                kind: InstructionKind::Mov { src, dest },
                debug_info: debug_info.clone(),
            });
        }
        tacky::InstructionKind::Jump(label) => {
            body.push(Instruction {
                kind: InstructionKind::Jmp(label.clone()),
                debug_info: debug_info.clone(),
            });
        }
        tacky::InstructionKind::JumpIfNotZero(cond, label) => {
            let cond = tacky_value_to_operand(cond);
            body.extend_from_slice(&[
                Instruction {
                    kind: InstructionKind::Cmp(cond, Operand::Imm(0)),
                    debug_info: debug_info.clone(),
                },
                Instruction {
                    kind: InstructionKind::JmpCC(Condition::NE, label.clone()),
                    debug_info: debug_info.clone(),
                },
            ]);
        }
        tacky::InstructionKind::JumpIfZero(cond, label) => {
            let cond = tacky_value_to_operand(cond);
            body.extend_from_slice(&[
                Instruction {
                    kind: InstructionKind::Cmp(cond, Operand::Imm(0)),
                    debug_info: debug_info.clone(),
                },
                Instruction {
                    kind: InstructionKind::JmpCC(Condition::E, label.clone()),
                    debug_info: debug_info.clone(),
                },
            ]);
        }
        tacky::InstructionKind::Label(label) => {
            body.push(Instruction {
                kind: InstructionKind::Label(label.clone()),
                debug_info: debug_info.clone(),
            });
        }
        tacky::InstructionKind::Return(value) => {
            let operand = tacky_value_to_operand(value);
            body.extend_from_slice(&[
                Instruction {
                    kind: InstructionKind::Mov {
                        src: operand,
                        dest: Operand::Register(Register::AX),
                    },
                    debug_info: debug_info.with_additional("return_value -> RAX".to_string()),
                },
                Instruction {
                    kind: InstructionKind::Ret,
                    debug_info: debug_info.clone(),
                },
            ]);
        }
        tacky::InstructionKind::Unary {
            op: UnaryOp::Not,
            dest,
            src,
        } => {
            let src_operand = tacky_value_to_operand(src);
            let dest_operand = tacky_value_to_operand(dest);
            body.extend_from_slice(&[
                Instruction {
                    kind: InstructionKind::Cmp(src_operand, Operand::Imm(0)),
                    debug_info: debug_info.clone(),
                },
                Instruction {
                    kind: InstructionKind::Mov {
                        src: Operand::Imm(0),
                        dest: dest_operand.clone(),
                    },
                    debug_info: debug_info.clone(),
                },
                Instruction {
                    kind: InstructionKind::SetCC(Condition::E, dest_operand),
                    debug_info: debug_info.clone(),
                },
            ]);
        }
        tacky::InstructionKind::Unary { op, dest, src } => {
            let src_operand = tacky_value_to_operand(src);
            let dest_operand = tacky_value_to_operand(dest);
            body.push(Instruction {
                kind: InstructionKind::Mov {
                    src: src_operand,
                    dest: dest_operand.clone(),
                },
                debug_info: debug_info.clone(),
            });
            body.push(Instruction {
                kind: InstructionKind::Unary(*op, dest_operand),
                debug_info: debug_info.clone(),
            });
        }
    }
}

fn tacky_value_to_operand(value: &tacky::Value) -> Operand {
    match value {
        tacky::Value::Constant(c) => Operand::Imm(*c),
        tacky::Value::Var(var_id) => Operand::Pseudo(*var_id),
    }
}

fn replace_pseudo_operand(
    stack_offset: i32,
    operand: &mut Operand,
    pseudo_map: &mut std::collections::HashMap<usize, i32>,
) -> i32 {
    if let Operand::Pseudo(var_id) = operand {
        let var_id = *var_id;
        if let Some(offset) = pseudo_map.get(&var_id) {
            *operand = Operand::Stack(*offset);
            stack_offset
        } else {
            let new_offset = stack_offset + 8;
            pseudo_map.insert(var_id, new_offset);
            *operand = Operand::Stack(new_offset);
            new_offset
        }
    } else {
        stack_offset
    }
}

fn resolve_pseudo_operands(prog: &mut Program) {
    for func in &mut prog.function_defs {
        let mut pseudo_map = std::collections::HashMap::new();
        let mut stack_offset: i32 = 0;

        for instr in &mut func.body {
            for operand in instr.kind.get_operands_mut() {
                stack_offset = replace_pseudo_operand(stack_offset, operand, &mut pseudo_map);
            }
        }

        func.stack_size = stack_offset;
    }
}

fn alloc_stack_and_resolve_stack_ops(prog: &mut Program) {
    for func in &mut prog.function_defs {
        func.body[0] = Instruction {
            kind: InstructionKind::AllocateStack(func.stack_size as usize),
            debug_info: func.body[0]
                .debug_info
                .with_additional("allocate stack".to_string()),
        };
        let mut index = 0;
        while index < func.body.len() {
            let Instruction { kind, debug_info } = std::mem::replace(
                &mut func.body[index],
                Instruction {
                    kind: InstructionKind::AllocateStack(0),
                    debug_info: DebugInfo::new(0, 0, String::new()),
                },
            ); // replace with a temporary instruction

            match kind {
                InstructionKind::Binary {
                    op: op @ (BinaryOp::LeftShift | BinaryOp::RightShift),
                    src: operand @ (Operand::Pseudo(_) | Operand::Register(_) | Operand::Stack(_)),
                    dest,
                } => {
                    replace_instruction!(func.body; [index] =>
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: operand,
                                dest: Operand::Register(Register::CX),
                            },
                            debug_info: debug_info.with_additional("shamt -> %cl (%rcx)".to_string()),
                        },
                        Instruction {
                            kind: InstructionKind::Binary {
                                op,
                                src: Operand::Register(Register::CL),
                                dest: dest,
                            },
                            debug_info: debug_info.clone(),
                        }
                    );
                }
                InstructionKind::Binary {
                    op:
                        op @ (BinaryOp::Add
                        | BinaryOp::Subtract
                        | BinaryOp::BitAnd
                        | BinaryOp::BitOr
                        | BinaryOp::BitXor),
                    src: src_op @ Operand::Stack(_),
                    dest: dest_op @ Operand::Stack(_),
                } => {
                    replace_instruction!(func.body; [index] =>
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: src_op,
                                dest: Operand::Register(Register::R10),
                            },
                            debug_info: debug_info.with_additional(format!("(stack binary {2} = {2} {0} {1}) {1} -> %r10", op, src_op, dest_op)),
                        },
                        Instruction {
                            kind: InstructionKind::Binary {
                                op: op,
                                src: Operand::Register(Register::R10),
                                dest: dest_op,
                            },
                            debug_info: debug_info.with_additional(format!("(stack binary {2} = {2} {0} {1}) %r10 -> {2}", op, src_op, dest_op)),
                        }
                    );
                }
                InstructionKind::Binary {
                    op: op @ BinaryOp::Multiply,
                    src,
                    dest: dest_op @ Operand::Stack(_),
                } => {
                    let prefix = format!("(multiply dest stack {1} = {1} * {0})", src, dest_op);
                    replace_instruction!(func.body; [index] =>
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: dest_op,
                                dest: Operand::Register(Register::R11),
                            },
                            debug_info: debug_info.with_additional(format!("{} {} -> %r11", prefix, src)),
                        },
                        Instruction {
                            kind: InstructionKind::Binary {
                                op: op,
                                src: src,
                                dest: Operand::Register(Register::R11),
                            },
                            debug_info: debug_info.with_additional(format!("{} {} * %r11 -> %r11", prefix, src)),
                        },
                        Instruction{
                            kind: InstructionKind::Mov {
                                src: Operand::Register(Register::R11),
                                dest: dest_op,
                            },
                            debug_info: debug_info.with_additional(format!("{} %r11 -> {}", prefix, dest_op)),
                        }
                    );
                }
                InstructionKind::Cmp(lhs @ Operand::Stack(_), rhs @ Operand::Stack(_)) => {
                    replace_instruction!(func.body; [index] =>
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: lhs,
                                dest: Operand::Register(Register::R10),
                            },
                            debug_info: debug_info.with_additional(format!("(cmp stack) {} -> %r10", lhs)),
                        },
                        Instruction {
                            kind: InstructionKind::Cmp(Operand::Register(Register::R10), rhs),
                            debug_info: debug_info.with_additional(format!("(cmp stack) %r10 -> {}", rhs)),
                        }
                    );
                }
                InstructionKind::Cmp(lhs, rhs @ Operand::Imm(_)) => {
                    replace_instruction!(func.body; [index] =>
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: rhs,
                                dest: Operand::Register(Register::R11),
                            },
                            debug_info: debug_info.with_additional(format!("(cmp imm) {} -> %r11", rhs)),
                        },
                        Instruction {
                            kind: InstructionKind::Cmp(lhs, Operand::Register(Register::R11)),
                            debug_info: debug_info.with_additional(format!("(cmp imm) cmp {}, {}", lhs, rhs)),
                        }
                    );
                }
                InstructionKind::Idiv(imm @ Operand::Imm(_)) => {
                    replace_instruction!(func.body; [index] =>
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: imm,
                                dest: Operand::Register(Register::R10),
                            },
                            debug_info: debug_info.with_additional(format!("(idiv imm) {} -> %r10", imm)),
                        },
                        Instruction {
                            kind: InstructionKind::Idiv(Operand::Register(Register::R10)),
                            debug_info: debug_info.with_additional(format!("(idiv imm) %r10 -> {}", imm)),
                        }
                    );
                }
                InstructionKind::Mov {
                    src: Operand::Stack(src),
                    dest: Operand::Stack(dest),
                } => {
                    replace_instruction!(func.body; [index] =>
                        Instruction{
                            kind: InstructionKind::Mov {
                                src: Operand::Stack(src),
                                dest: Operand::Register(Register::R10),
                            },
                            debug_info: debug_info.with_additional(format!("(mov stack) {} -> %r10", src))
                        },
                        Instruction{
                            kind: InstructionKind::Mov {
                                src: Operand::Register(Register::R10),
                                dest: Operand::Stack(dest),
                            },
                            debug_info: debug_info.with_additional(format!("(mov stack) %r10 -> {}", dest))
                        }
                    );
                }
                _ => {
                    // place the instruction back if there is no change
                    func.body[index] = Instruction { kind, debug_info };
                }
            }
            index += 1;
        }
    }
}
