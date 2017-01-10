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

#[test]
fn test_vm_call() {
    let mut m = vm_with_ins(vec![Nop, Call(123)]);
    m.step();
    m.step();
    assert_eq!(m.fps.len(), 2);
    assert_eq!(*m.fps.last().unwrap(), STACK_SIZE as u64 - 1);
    assert_eq!(*m.head(), 2);
    assert_eq!(m.ip, 123);
}

#[test]
fn test_vm_ret() {
    let mut m = vm_with_ins(vec![Pushiw(0xDEAD), Ret]);
    m.fps.push(999);
    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 1);
    assert_eq!(m.step(), true);

    assert_eq!(m.rx, 0); // should not set the return reg
    assert_eq!(m.ip, 0xDEAD); // should return to the address
    assert_eq!(m.fps.len(), 1); // should pop a frame pointer
}


#[test]
fn test_vm_retw() {
    let mut m = vm_with_ins(vec![Pushiw(0xDEAD), Pushiw(42), Retw]);
    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 1);
    m.step();
    assert_eq!(m.sp, STACK_SIZE as u64 - 2);
    assert_eq!(m.step(), false);

    assert_eq!(m.rx, 42); // should set the return reg
    assert_eq!(m.ip, 0xDEAD); // should return to the address
    assert_eq!(m.fps.len(), 0); // should pop a frame pointer
}
