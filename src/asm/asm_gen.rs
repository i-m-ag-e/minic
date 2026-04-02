use super::{FunctionDef, Instruction, Operand, Program, Register};
use crate::{
    asm::{Condition, InstructionKind},
    ast::expr::{BinaryOp, UnaryOp},
    debug_info::DebugInfo,
    tacky,
};

const ARG_REGISTERS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];

pub fn emit_asm_program(tacky_prog: tacky::Program) -> Program {
    let mut asm_program = Program::new();

    for tacky_func in tacky_prog.0 {
        let asm_func = emit_asm_function(tacky_func);
        asm_program.function_defs.push(asm_func);
    }

    resolve_pseudo_operands(&mut asm_program);
    fixup_instructions(&mut asm_program);
    allocate_stack(&mut asm_program);
    asm_program
}

fn emit_asm_function(tacky_func: tacky::FunctionDef) -> FunctionDef {
    let mut body = vec![Instruction {
        kind: InstructionKind::AllocateStack(0), // placeholder, will be replaced later
        debug_info: DebugInfo::new(0, 0, "allocate stack placeholder".to_string()),
    }];

    for (i, param) in tacky_func.params.into_iter().enumerate() {
        let param_operand =
            Operand::Pseudo(param.unwrap_or(format!("fn.{}.arg{}", tacky_func.name, i)));

        if i < ARG_REGISTERS.len() {
            let arg_register = ARG_REGISTERS[i];
            body.push(Instruction {
                kind: InstructionKind::Mov {
                    src: Operand::Register(arg_register),
                    dest: param_operand,
                },
                debug_info: DebugInfo::new(0, 0, format!("move argument {i} into pseudo register")),
            });
        } else {
            body.push(Instruction {
                kind: InstructionKind::Mov {
                    src: Operand::Stack(16 + (i as i32 - 6) * 8),
                    dest: param_operand,
                },
                debug_info: DebugInfo::new(
                    0,
                    0,
                    format!("move argument {i} from stack into pseudo register"),
                ),
            });
        }
    }

    for instr in tacky_func.body {
        emit_asm_instruction(instr, &mut body);
    }

    FunctionDef {
        name: tacky_func.name,
        stack_size: 0,
        body,
    }
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

fn emit_asm_binary_instruction(
    kind: tacky::InstructionKind,
    debug_info: DebugInfo,
    body: &mut Vec<Instruction>,
) {
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
                    binary_op_to_condition(op),
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
                        op,
                        src: right,
                        dest: dest.clone(),
                    },
                    debug_info: debug_info.clone(),
                });
            }
        }

        _ => unreachable!(),
    }
}

fn emit_asm_unary_instruction(
    kind: tacky::InstructionKind,
    debug_info: DebugInfo,
    body: &mut Vec<Instruction>,
) {
    let tacky::InstructionKind::Unary { op, dest, src } = kind else {
        unreachable!("Expected unary instruction");
    };
    let src_operand = tacky_value_to_operand(src);
    let dest_operand = tacky_value_to_operand(dest);

    match op {
        UnaryOp::Not => body.extend_from_slice(&[
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
        ]),
        _ => {
            body.push(Instruction {
                kind: InstructionKind::Mov {
                    src: src_operand,
                    dest: dest_operand.clone(),
                },
                debug_info: debug_info.clone(),
            });
            body.push(Instruction {
                kind: InstructionKind::Unary(op, dest_operand),
                debug_info: debug_info.clone(),
            });
        }
    }
}

fn emit_asm_function_call(
    kind: tacky::InstructionKind,
    debug_info: DebugInfo,
    body: &mut Vec<Instruction>,
) {
    let tacky::InstructionKind::FunctionCall {
        args,
        dest,
        func_name,
        is_external,
    } = kind
    else {
        unreachable!("Expected function call instruction");
    };
    let mut register_args = args;
    let stack_args = if register_args.len() > ARG_REGISTERS.len() {
        register_args.split_off(ARG_REGISTERS.len())
    } else {
        Vec::new()
    };

    let stack_padding = (stack_args.len() as i32 % 2) * 8;
    if stack_padding > 0 {
        body.push(Instruction {
            kind: InstructionKind::AllocateStack(stack_padding as usize),
            debug_info: debug_info.with_additional("stack padding".to_string()),
        });
    }

    for (i, tacky_arg) in register_args.into_iter().enumerate() {
        let arg_operand = tacky_value_to_operand(tacky_arg);
        body.push(Instruction {
            kind: InstructionKind::Mov {
                src: arg_operand,
                dest: Operand::Register(ARG_REGISTERS[i]),
            },
            debug_info: debug_info.with_additional(format!("move argument {i} into register")),
        });
    }

    let stack_args_len = stack_args.len();
    for (i, tacky_arg) in stack_args.into_iter().rev().enumerate() {
        let arg_operand = tacky_value_to_operand(tacky_arg);
        match arg_operand {
            Operand::Imm(_) | Operand::Register(_) => {
                body.push(Instruction {
                    kind: InstructionKind::Push(arg_operand),
                    debug_info: debug_info.with_additional(format!("move argument {i} onto stack")),
                });
            }
            _ => {
                body.push(Instruction {
                    kind: InstructionKind::Mov {
                        src: arg_operand,
                        dest: Operand::Register(Register::AX),
                    },
                    debug_info: debug_info
                        .with_additional(format!("move argument {i} into %rax for stack push")),
                });
                body.push(Instruction {
                    kind: InstructionKind::Push(Operand::Register(Register::AX)),
                    debug_info: debug_info
                        .with_additional(format!("move argument {i} from %rax onto stack")),
                });
            }
        }
    }

    body.push(Instruction {
        kind: InstructionKind::Call {
            func_name: func_name.clone(),
            is_external,
        },
        debug_info: debug_info.with_additional(format!("call function {}", func_name)),
    });

    let bytes_to_remove = 8 * stack_args_len + stack_padding as usize;
    if bytes_to_remove > 0 {
        body.push(Instruction {
            kind: InstructionKind::DeallocateStack(bytes_to_remove),
            debug_info: debug_info.with_additional(format!(
                "clean up stack arguments ({} bytes)",
                bytes_to_remove
            )),
        });
    }

    let dest = tacky_value_to_operand(dest);
    body.push(Instruction {
        kind: InstructionKind::Mov {
            src: Operand::Register(Register::AX),
            dest,
        },
        debug_info,
    });
}

fn emit_asm_instruction(instr: tacky::Instruction, body: &mut Vec<Instruction>) {
    let tacky::Instruction { kind, debug_info } = instr;
    match kind {
        tacky::InstructionKind::Binary { .. } => {
            emit_asm_binary_instruction(kind, debug_info, body);
        }
        tacky::InstructionKind::Copy { dest, src } => {
            let src = tacky_value_to_operand(src);
            let dest = tacky_value_to_operand(dest);
            body.push(Instruction {
                kind: InstructionKind::Mov { src, dest },
                debug_info: debug_info.clone(),
            });
        }
        tacky::InstructionKind::FunctionCall { .. } => {
            emit_asm_function_call(kind, debug_info, body);
        }
        tacky::InstructionKind::Jump(label) => {
            body.push(Instruction {
                kind: InstructionKind::Jmp(label.clone()),
                debug_info: debug_info.clone(),
            });
        }
        tacky::InstructionKind::JumpIfEqual { lhs, rhs, label } => {
            let lhs = tacky_value_to_operand(lhs);
            let rhs = tacky_value_to_operand(rhs);
            body.extend_from_slice(&[
                Instruction {
                    kind: InstructionKind::Cmp(lhs, rhs),
                    debug_info: debug_info.clone(),
                },
                Instruction {
                    kind: InstructionKind::JmpCC(Condition::E, label.clone()),
                    debug_info: debug_info.clone(),
                },
            ]);
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
        tacky::InstructionKind::Return => {
            body.push(Instruction {
                kind: InstructionKind::Ret,
                debug_info: debug_info.clone(),
            });
        }
        tacky::InstructionKind::ReturnValue(value) => {
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
        tacky::InstructionKind::Unary { .. } => {
            emit_asm_unary_instruction(kind, debug_info, body);
        }
    }
}

fn tacky_value_to_operand(value: tacky::Value) -> Operand {
    match value {
        tacky::Value::Constant(c) => Operand::Imm(c),
        tacky::Value::Var(var_id) => Operand::Pseudo(var_id.clone()),
    }
}

fn replace_pseudo_operand(
    stack_offset: i32,
    operand: &mut Operand,
    pseudo_map: &mut std::collections::HashMap<String, i32>,
) -> i32 {
    if let Operand::Pseudo(var_id) = operand {
        if let Some(offset) = pseudo_map.get(var_id) {
            *operand = Operand::Stack(-*offset);
            stack_offset
        } else {
            let new_offset = stack_offset + 8;
            pseudo_map.insert(var_id.clone(), new_offset);
            *operand = Operand::Stack(-new_offset);
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

        func.stack_size = if stack_offset % 16 == 0 {
            stack_offset
        } else {
            stack_offset + (16 - stack_offset % 16)
        };
    }
}

fn fixup_instructions(prog: &mut Program) {
    for func in &mut prog.function_defs {
        let old_body = std::mem::take(&mut func.body);
        let mut new_body = Vec::with_capacity(old_body.len());

        for instr in old_body.into_iter() {
            let Instruction { kind, debug_info } = instr;

            match kind {
                InstructionKind::Binary {
                    op: op @ (BinaryOp::LeftShift | BinaryOp::RightShift),
                    src: operand @ (Operand::Pseudo(_) | Operand::Register(_) | Operand::Stack(_)),
                    dest,
                } => {
                    new_body.extend([
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: operand,
                                dest: Operand::Register(Register::CX),
                            },
                            debug_info: debug_info
                                .with_additional("shamt -> %cl (%rcx)".to_string()),
                        },
                        Instruction {
                            kind: InstructionKind::Binary {
                                op,
                                src: Operand::Register(Register::CL),
                                dest: dest,
                            },
                            debug_info: debug_info.clone(),
                        },
                    ]);
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
                    new_body.extend([
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: src_op.clone(),
                                dest: Operand::Register(Register::R10),
                            },
                            debug_info: debug_info.with_additional(format!(
                                "(stack binary {2} = {2} {0} {1}) {1} -> %r10",
                                op, src_op, dest_op
                            )),
                        },
                        Instruction {
                            kind: InstructionKind::Binary {
                                op: op,
                                src: Operand::Register(Register::R10),
                                dest: dest_op.clone(),
                            },
                            debug_info: debug_info.with_additional(format!(
                                "(stack binary {2} = {2} {0} {1}) %r10 -> {2}",
                                op, src_op, dest_op
                            )),
                        },
                    ]);
                }
                InstructionKind::Binary {
                    op: op @ BinaryOp::Multiply,
                    src,
                    dest: dest_op @ Operand::Stack(_),
                } => {
                    let prefix = format!("(multiply dest stack {1} = {1} * {0})", src, dest_op);
                    new_body.extend([
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: dest_op.clone(),
                                dest: Operand::Register(Register::R11),
                            },
                            debug_info: debug_info
                                .with_additional(format!("{} {} -> %r11", prefix, src)),
                        },
                        Instruction {
                            kind: InstructionKind::Binary {
                                op: op,
                                src: src.clone(),
                                dest: Operand::Register(Register::R11),
                            },
                            debug_info: debug_info
                                .with_additional(format!("{} {} * %r11 -> %r11", prefix, src)),
                        },
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: Operand::Register(Register::R11),
                                dest: dest_op.clone(),
                            },
                            debug_info: debug_info
                                .with_additional(format!("{} %r11 -> {}", prefix, dest_op)),
                        },
                    ]);
                }
                InstructionKind::Cmp(lhs @ Operand::Stack(_), rhs @ Operand::Stack(_)) => {
                    new_body.extend([
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: lhs.clone(),
                                dest: Operand::Register(Register::R10),
                            },
                            debug_info: debug_info
                                .with_additional(format!("(cmp stack) {} -> %r10", lhs)),
                        },
                        Instruction {
                            kind: InstructionKind::Cmp(
                                Operand::Register(Register::R10),
                                rhs.clone(),
                            ),
                            debug_info: debug_info
                                .with_additional(format!("(cmp stack) %r10 -> {}", rhs)),
                        },
                    ]);
                }
                InstructionKind::Cmp(lhs, rhs @ Operand::Imm(_)) => {
                    new_body.extend([
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: rhs.clone(),
                                dest: Operand::Register(Register::R11),
                            },
                            debug_info: debug_info
                                .with_additional(format!("(cmp imm) {} -> %r11", rhs)),
                        },
                        Instruction {
                            kind: InstructionKind::Cmp(
                                lhs.clone(),
                                Operand::Register(Register::R11),
                            ),
                            debug_info: debug_info
                                .with_additional(format!("(cmp imm) cmp {}, {}", lhs, rhs)),
                        },
                    ]);
                }
                InstructionKind::Idiv(imm @ Operand::Imm(_)) => {
                    new_body.extend([
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: imm.clone(),
                                dest: Operand::Register(Register::R10),
                            },
                            debug_info: debug_info
                                .with_additional(format!("(idiv imm) {} -> %r10", imm)),
                        },
                        Instruction {
                            kind: InstructionKind::Idiv(Operand::Register(Register::R10)),
                            debug_info: debug_info
                                .with_additional(format!("(idiv imm) %r10 -> {}", imm)),
                        },
                    ]);
                }
                InstructionKind::Mov {
                    src: Operand::Stack(src),
                    dest: Operand::Stack(dest),
                } => {
                    new_body.extend([
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: Operand::Stack(src),
                                dest: Operand::Register(Register::R10),
                            },
                            debug_info: debug_info
                                .with_additional(format!("(mov stack) {} -> %r10", src)),
                        },
                        Instruction {
                            kind: InstructionKind::Mov {
                                src: Operand::Register(Register::R10),
                                dest: Operand::Stack(dest),
                            },
                            debug_info: debug_info
                                .with_additional(format!("(mov stack) %r10 -> {}", dest)),
                        },
                    ]);
                }
                _ => {
                    // place the instruction back if there is no change
                    new_body.push(Instruction { kind, debug_info });
                }
            }
        }

        func.body = new_body;
    }
}

fn allocate_stack(prog: &mut Program) {
    for func in &mut prog.function_defs {
        if let Some(Instruction {
            kind: InstructionKind::AllocateStack(stack_size),
            ..
        }) = func.body.first_mut()
        {
            *stack_size = func.stack_size as usize;
        }
    }
}
