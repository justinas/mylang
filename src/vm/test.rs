use super::Instruction;
use super::Instruction::*;
use super::Machine;
use super::super::codegen::Program;
use super::STACK_SIZE;

#[cfg(test)]
fn vm_with_ins(ins: Vec<Instruction>) -> Machine {
    let mut w = vec![];
    Program { instructions: ins, ..Default::default() }.encode(&mut w).unwrap();
    Machine::new(&w)
}

#[test]
fn test_vm_push_pop() {
    let mut m = vm_with_ins(vec![Pushiw(123), Pushlw(-1), Pushlw(-2), Poplw(-1), Popn, Pushr]);

    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 1);
    assert_eq!(*m.head(), 123);

    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 2);
    assert_eq!(*m.head(), 123);

    *m.head_mut() = 124;
    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 3);
    assert_eq!(*m.head(), 124);

    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 2);
    assert_eq!(m.stack[STACK_SIZE - 1], 124);

    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 1);

    m.rx = 42;
    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 2);
    assert_eq!(*m.head(), 42);
}
