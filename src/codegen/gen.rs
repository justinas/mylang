use super::super::parser::{Atom, Expr, Operation, Stmt, Type};
use super::Context;
use super::Instruction;
use super::Instruction::*;

pub trait Gen {
    fn gen(&self, &mut Context) -> Result<Vec<Instruction>, ()>;
}

impl Gen for Atom {
    fn gen(&self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        match *self {
            Atom::Const(ref c) => Ok(vec![Pushiw(c.parse::<i64>().unwrap())]),
            Atom::Neg(ref e) => {
                let mut v = (*e).gen(ctx)?;
                v.push(Neg);
                Ok(v)
            }
            _ => unimplemented!(),
        }
    }
}

impl Gen for Expr {
    fn gen(&self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        match *self {
            Expr::Atom(ref a) => a.gen(ctx),
            Expr::Bin(ref e1, ref e2, ref op) => {
                let mut v = e1.gen(ctx)?;
                v.extend_from_slice(&e2.gen(ctx)?);
                match *op {
                    Operation::Add => v.push(Add),
                    Operation::Div => v.push(Div),
                    Operation::Mul => v.push(Mul),
                    Operation::Sub => v.push(Sub),

                    Operation::And => v.push(And),
                    Operation::Or => v.push(Or),
                    _ => unimplemented!(),
                }
                Ok(v)
            }
        }
    }
}

impl Gen for Stmt {
    fn gen(&self, ctx: &mut Context) -> Result<Vec<Instruction>, ()> {
        match *self {
            Stmt::Expr(ref e) => {
                let mut v = e.gen(ctx)?;

                // every expr produces a single result, let's ignore it
                // by popping it off the stack
                v.push(Popn);
                Ok(v)
            }
            _ => unimplemented!(),
        }
    }
}

pub trait Typed {
    // typ() typ-checks the implementor
    // and returns Ok() with the typ or Err() if typ-check failed.
    fn typ(&self) -> Result<Type, ()> {
        Ok(Type::Void)
    }
}

impl Typed for Atom {
    fn typ(&self) -> Result<Type, ()> {
        match *self {
            Atom::Const(..) => Ok(Type::Int),
            Atom::FnCall(..) => unimplemented!(),
            Atom::Ident(..) => unimplemented!(),
            Atom::Neg(ref e) => {
                match e.typ() {
                    t @ Ok(Type::Byte) |
                    t @ Ok(Type::Int) => t,
                    _ => Err(()),
                }
            }
            Atom::PExpr(ref e) => e.typ(),
        }
    }
}

impl Typed for Expr {
    fn typ(&self) -> Result<Type, ()> {
        match *self {
            Expr::Atom(ref a) => a.typ(),
            Expr::Bin(ref e1, ref e2, ref op) => {
                match (e1.typ(), e2.typ()) {
                    (Ok(t1), Ok(t2)) if t1 == t2 => {
                        match *op {
                            Operation::Lt | Operation::Lte | Operation::Gt | Operation::Gte |
                            Operation::Eq | Operation::Neq | Operation::And | Operation::Or => {
                                Ok(Type::Byte)
                            }
                            _ => Ok(t1.clone()),

                        }
                    }
                    _ => Err(()),
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::super::parser::{Atom, Expr, Operation, Type};
    use super::Typed;
    #[test]
    fn test_expr_typed() {
        assert_eq!(Expr::Atom(Atom::Const("1".into())).typ(), Ok(Type::Int));
    }
}
