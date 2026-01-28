use super::{FunctionDef, Instruction, Operand, Program, Register};
use crate::tacky;

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

fn tacky_instr_to_asm(instr: &tacky::Instruction, body: &mut Vec<Instruction>) {
    match instr {
        tacky::Instruction::Return(value) => {
            let operand = tacky_value_to_operand(value);
            body.push(Instruction::Mov {
                src: operand,
                dest: Operand::Register(Register::AX),
            });
            body.push(Instruction::Ret);
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
                Instruction::Mov { src, dest } => {
                    replace_pseudo_operand!(stack_offset, src, pseudo_map);
                    replace_pseudo_operand!(stack_offset, dest, pseudo_map);
                }
                Instruction::Unary(_, operand) => {
                    replace_pseudo_operand!(stack_offset, operand, pseudo_map);
                }
                _ => {}
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
