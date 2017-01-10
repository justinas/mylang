pub use super::codegen::Instruction;
use super::codegen::Instruction::*;

const STACK_SIZE: usize = 64;

#[derive(Debug)]
pub struct Machine {
    fps: Vec<u64>,
    ip: u64, // ip IN INSTRUCTIONS, NOT BYTES
    rx: u64,
    stack: Vec<u64>,
    sp: u64,

    program: Vec<u8>,
}

impl Machine {
    pub fn new(program: &[u8]) -> Self {
        Machine {
            fps: vec![STACK_SIZE as u64],
            ip: 0,
            rx: 0,
            stack: vec![0; STACK_SIZE],
            sp: STACK_SIZE as u64,

            program: program.into(),
        }
    }

    fn fp(&self) -> u64 {
        *self.fps.last().unwrap()
    }

    fn head(&self) -> &u64 {
        &self.stack[self.sp as usize]
    }

    fn head_mut(&mut self) -> &mut u64 {
        &mut self.stack[self.sp as usize]
    }

    fn pop(&mut self) -> u64 {
        self.sp += 1;
        *self.head()
    }

    fn push(&mut self, n: u64) {
        self.sp -= 1;
        *self.head_mut() = n
    }

    // Returns false if the program has finished.
    pub fn step(&mut self) -> bool {
        let bounds = (self.ip as usize) * 10..(self.ip as usize + 1) * 10;
        match Instruction::from_bytes(&self.program[bounds]).unwrap() {
            Poplw(offset) => {
                let fp = self.fp() as i64;
                let idx = (fp + offset) as usize;
                self.stack[idx] = self.pop();
            }
            Popn => {
                self.pop();
            }
            Pushiw(x) => self.push(x as u64),
            Pushlw(offset) => {
                let fp = self.fp() as i64;
                let val = self.stack[(fp + offset) as usize];
                self.push(val);
            }
            Pushr => {
                let val = self.rx;
                self.push(val);
            }
            _ => unimplemented!(),
        };
        self.ip += 1;
        true
    }
}

mod test;
