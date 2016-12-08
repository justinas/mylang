use std::i64;

use super::super::parser::{Atom, Block, Expr, Operation, Stmt, Type, WhileStmt};
use super::Context;
use super::Error;
use super::Instruction;
use super::Instruction::*;
use super::Marker;
use super::Symbol;

pub trait Gen {
    fn gen(&self, &mut Context) -> Result<Vec<Instruction>, Error>;
}

impl Gen for Atom {
    fn gen(&self, ctx: &mut Context) -> Result<Vec<Instruction>, Error> {
        match *self {
            Atom::Const(ref c) => Ok(vec![Pushiw(c.parse::<i64>().unwrap())]),
            Atom::FnCall(ref id, ref args) => {
                let ret = ctx.find_function(&id).ok_or(Error::SymbolNotFound(id.clone()))?.ret;

                let mut v = vec![];
                for a in args.iter() {
                    v.extend_from_slice(&a.gen(ctx)?);
                }
                v.push(__Marker(Marker::PushCurPC));
                v.push(__Marker(Marker::Call(id.clone())));
                for a in args.iter() {
                    v.push(Popn)
                }
                if ret != Type::Void {
                    v.push(Pushr);
                }
                Ok(v)
            }
            Atom::Ident(ref id) => {
                let sym_location = ctx.find_symbol_location(&id)
                    .ok_or(Error::SymbolNotFound(id.clone()))?;
                Ok(vec![Pushlw(sym_location as i64)])
            }
            Atom::Neg(ref e) => {
                let mut v = e.gen(ctx)?;
                v.push(Neg);
                Ok(v)
            }
            Atom::PExpr(ref e) => e.gen(ctx),
        }
    }
}

impl Gen for Block {
    fn gen(&self, ctx: &mut Context) -> Result<Vec<Instruction>, Error> {
        ctx.push_frame(vec![]);
        let mut v = vec![];
        for stmt in self.0.iter() {
            match stmt.gen(ctx) {
                Ok(slice) => v.extend_from_slice(&slice),
                e @ Err(..) => {
                    ctx.pop_frame();
                    return e;
                }
            }
        }
        ctx.pop_frame();
        Ok(v)
    }
}

impl Gen for Expr {
    fn gen(&self, ctx: &mut Context) -> Result<Vec<Instruction>, Error> {
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

                    Operation::Eq => v.push(Eq),
                    Operation::Neq => v.push(Neq),
                    Operation::Lt => v.push(Lt),
                    Operation::Lte => v.push(Lte),
                    Operation::Gt => v.push(Gt),
                    Operation::Gte => v.push(Gte),

                    Operation::And => v.push(And),
                    Operation::Or => v.push(Or),
                }
                Ok(v)
            }
        }
    }
}

impl Gen for Stmt {
    fn gen(&self, ctx: &mut Context) -> Result<Vec<Instruction>, Error> {
        match *self {
            Stmt::Assign(ref stmt) => {
                let sym_loc = ctx.find_symbol_location(&stmt.ident)
                    .ok_or(Error::SymbolNotFound(stmt.ident.clone()))?;
                let typ = stmt.expr.typ(ctx)?;
                let mut v = stmt.expr.gen(ctx)?;
                let sym = ctx.find_symbol(&stmt.ident).unwrap();
                if !typ.compatible_with(&sym.typ) {
                    return Err(Error::TypesIncompatible(typ, sym.typ));
                }
                v.push(Poplw(sym_loc as i64));
                Ok(v)
            }
            Stmt::Break => {
                match ctx.loop_depth {
                    0 => Err(Error::BreakOutOfContext),
                    _ => Ok(vec![__Marker(Marker::Break(ctx.loop_depth as u64))]),
                }
            }
            Stmt::Block(ref block) => block.gen(ctx),
            Stmt::Decl(ref stmt) => {
                if stmt.typ == Type::Void || stmt.typ == Type::Num {
                    return Err(Error::InvalidType(stmt.typ));
                }
                if let Some(_) = ctx.top_frame().iter().find(|s| s.name == stmt.ident) {
                    return Err(Error::VariableRedeclared(stmt.ident.clone()));
                }
                ctx.top_frame_mut().push(Symbol::new(stmt.ident.clone(), stmt.typ));
                Ok(vec![Pushiw(0)])
            }
            Stmt::Expr(ref e) => {
                let mut v = e.gen(ctx)?;

                // every expr except for void function call
                // produces a single result, let's ignore it
                // by popping it off the stack
                if let Expr::Atom(Atom::FnCall(ref f, _)) = *e {
                    if ctx.find_function(&f).unwrap().ret == Type::Void {
                        return Ok(v);
                    }
                }
                v.push(Popn);
                Ok(v)
            }
            Stmt::While(WhileStmt(ref conditional)) => {
                ctx.loop_depth += 1;
                let mut v = vec![];

                // generate cond expr
                let cond_expr = match (&conditional.cond).gen(ctx) {
                    Ok(c) => c,
                    e @ Err(_) => {
                        ctx.loop_depth -= 1;
                        return e;
                    }
                };
                v.extend_from_slice(&cond_expr);

                // mark the jump-if-false instruction
                v.push(__Marker(Marker::Jmpzrel(i64::MAX)));
                let jump_end_idx = v.len() - 1;

                // generate block
                let block = match (&conditional.block).gen(ctx) {
                    Ok(b) => b,
                    e @ Err(_) => {
                        ctx.loop_depth -= 1;
                        return e;
                    }
                };
                v.extend_from_slice(&block);

                // repeat loop
                let loop_start_rel = -(v.len() as i64);
                v.push(__Marker(Marker::Jmprel(loop_start_rel)));

                // set jump-if-false to one-past-end
                let len = v.len();
                v[jump_end_idx] = __Marker(Marker::Jmpzrel(len as i64 - jump_end_idx as i64));

                // Resolve any breaks
                for (pos, ins) in v.iter_mut().enumerate() {
                    if *ins == __Marker(Marker::Break(ctx.loop_depth as u64)) {
                        let to_past_end = len - pos;
                        *ins = __Marker(Marker::Jmpzrel(to_past_end as i64));
                    }
                }
                ctx.loop_depth -= 1;
                Ok(v)
            }
            _ => unimplemented!(),
        }
    }
}

pub trait Typed {
    // typ() typ-checks the implementor
    // and returns Ok() with the typ or Err() if typ-check failed.
    fn typ(&self, ctx: &mut Context) -> Result<Type, Error> {
        Ok(Type::Void)
    }
}

impl Typed for Atom {
    fn typ(&self, ctx: &mut Context) -> Result<Type, Error> {
        match *self {
            Atom::Const(..) => Ok(Type::Int),
            Atom::FnCall(ref id, ref args) => {
                match ctx.find_function(&id) {
                    Some(f) => {
                        if f.params.len() == args.len() {
                            for (param, arg) in f.params.iter().zip(args.iter()) {
                                let mut my_ctx = ctx.clone();
                                let arg_typ = arg.typ(&mut my_ctx)?;
                                if !param.typ.compatible_with(&arg_typ) {
                                    return Err(Error::TypesIncompatible(param.typ, arg_typ));
                                }
                            }
                            Ok(f.ret)
                        } else {
                            Err(Error::RequiresNArgs(f.params.len()))
                        }
                    }
                    None => Err(Error::SymbolNotFound(id.clone())),
                }
            }
            Atom::Ident(ref id) => {
                let sym = ctx.find_symbol(&id).ok_or(Error::SymbolNotFound(id.clone()))?;
                Ok(sym.typ)
            }
            Atom::Neg(ref e) => {
                match e.typ(ctx) {
                    t @ Ok(Type::Byte) |
                    t @ Ok(Type::Int) => t,
                    Ok(t) => Err(Error::InvalidType(t)),
                    err @ Err(..) => err,
                }
            }
            Atom::PExpr(ref e) => e.typ(ctx),
        }
    }
}

impl Typed for Expr {
    fn typ(&self, ctx: &mut Context) -> Result<Type, Error> {
        match *self {
            Expr::Atom(ref a) => a.typ(ctx),
            Expr::Bin(ref e1, ref e2, ref op) => {
                match (e1.typ(ctx), e2.typ(ctx)) {
                    (Ok(t1), Ok(t2)) if t1.compatible_with(&t2) => {
                        match *op {
                            Operation::Lt | Operation::Lte | Operation::Gt | Operation::Gte |
                            Operation::Eq | Operation::Neq | Operation::And | Operation::Or => {
                                Ok(Type::Byte)
                            }
                            _ => Ok(t1.clone()),

                        }
                    }
                    (Ok(t1), Ok(t2)) => Err(Error::TypesIncompatible(t1, t2)),
                    (Err(e), _) => Err(e),
                    (_, Err(e)) => Err(e),
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
        assert_eq!(Expr::Atom(Atom::Const("1".into())).typ(&mut Default::default()),
                   Ok(Type::Int));
    }
}
