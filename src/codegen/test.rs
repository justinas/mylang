use super::super::parser::{self, Atom, Block, Conditional, Expr, FnItem, Operation, Stmt, Type,
                           WhileStmt};
use super::{Context, Gen, Symbol, Typed};
use super::Error;
use super::Instruction;
use super::Instruction::*;
use super::Marker;
use super::parse_program;

macro_rules! empty_fn {
    ($name:expr, $ret:expr) => {
        FnItem{ block: parser::Block(vec![]), name: $name.into(), params: vec![], ret: $ret}
    };
    ($name:expr, $params:expr, $ret:expr) => {
        FnItem{ block: parser::Block(vec![]), name: $name.into(), params: $params, ret: $ret}
    };
}

#[test]
fn test_typed() {

    {
        let fns = &mut [empty_fn!("abc", Type::Int)];
        let mut ctx = Context::new();
        ctx.functions = fns;
        let e = Expr::Atom(Atom::FnCall("abc".into(), vec![]));
        assert_eq!(e.typ(&mut ctx).unwrap(), Type::Int);
    }
}

#[test]
fn test_symbol_stack() {
    macro_rules! sym {
        ($name:expr) => (Symbol::new($name, Type::Void));
    }

    {
        let mut ctx = Context::new();
        ctx.push_frame(vec![sym!("b"), sym!("a")]);
        ctx.push_frame(vec![sym!("a")]);
        assert_eq!(ctx.find_symbol_location("a").unwrap(), -3)
    }

    {
        let mut ctx = Context::new();
        ctx.push_frame(vec![sym!("a")]);
        ctx.push_frame(vec![sym!("b"), sym!("a")]);
        assert_eq!(ctx.find_symbol_location("a").unwrap(), -3)
    }

    {
        let mut ctx = Context::new();
        ctx.arguments.push(sym!("a1"));
        ctx.arguments.push(sym!("a2"));
        ctx.push_frame(vec![sym!("a")]);
        ctx.push_frame(vec![sym!("b"), sym!("a")]);
        assert_eq!(ctx.find_symbol_location("a1").unwrap(), 2);
        assert_eq!(ctx.find_symbol_location("a2").unwrap(), 1);
    }
}

#[test]
fn test_gen_expr() {
    assert_eq!(Expr::Atom(Atom::Const("234".into())).gen(&mut Default::default()).unwrap(),
               vec![Pushiw(234)]);

    {
        let e = Expr::Atom(Atom::Neg(Box::new(Atom::Const("234".into()))));
        assert_eq!(e.gen(&mut Default::default()).unwrap(),
                   vec![Pushiw(234), Neg]);
    }

    {
        let e = Expr::Bin(Box::new(Expr::Atom(Atom::Const("234".into()))),
                          Box::new(Expr::Atom(Atom::Const("456".into()))),
                          Operation::Mul);
        assert_eq!(e.gen(&mut Default::default()).unwrap(),
                   vec![Pushiw(234), Pushiw(456), Mul]);
    }
    {
        let e = Expr::Atom(Atom::Ident("ab".into()));
        let mut ctx = Context::new();
        ctx.push_frame(vec![Symbol::new("cd", Type::Int), Symbol::new("ab", Type::Byte)]);
        assert_eq!(e.gen(&mut ctx).unwrap(), vec![Pushlw(-2)]);
    }
}

#[test]
fn test_gen_stmt_assign() {
    let e = Stmt::Assign(parser::AssignStmt {
        ident: "abc".into(),
        expr: Expr::Atom(Atom::FnCall("def".into(), vec![])),
    });

    let fns = &[empty_fn!("def", vec![], Type::Int)];
    let mut ctx = Context::new();
    ctx.functions = fns;
    ctx.push_frame(vec![Symbol::new("hi", Type::Void), Symbol::new("abc", Type::Int)]);
    assert_eq!(e.gen(&mut ctx).unwrap(),
               vec![__Marker(Marker::PushRetAddr),
                    __Marker(Marker::Call("def".into())),
                    Pushr,
                    Poplw(-2)]);
}

#[test]
fn test_gen_stmt_decl() {
    let e = Stmt::Decl(parser::DeclStmt {
        ident: "abc".into(),
        typ: Type::Int,
    });

    let mut ctx = Context::new();
    ctx.push_frame(vec![Symbol::new("abc", Type::Int)]);
    assert_eq!(e.gen(&mut ctx).unwrap_err(),
               Error::VariableRedeclared("abc".into()));
    ctx.top_frame_mut().clear();
    assert_eq!(e.gen(&mut ctx).unwrap(), vec![Pushiw(0)]);
    assert_eq!(ctx.top_frame().len(), 1);
}


#[test]
fn test_gen_stmt_block() {
    {
        let e = Expr::Bin(Box::new(Expr::Atom(Atom::Const("234".into()))),
                          Box::new(Expr::Atom(Atom::Const("456".into()))),
                          Operation::Mul);
        let s = Stmt::Expr(e);
        let v1 = s.gen(&mut Default::default()).unwrap();
        let v2 = Stmt::Block(parser::Block(vec![s])).gen(&mut Default::default()).unwrap();
        assert_eq!(v1, v2);
    }
}

#[test]
fn test_gen_stmt_return() {
    {
        let fns = &[empty_fn!("abc", Type::Void)];
        let mut ctx = Context::new();
        ctx.functions = fns;
        ctx.this_fn = Some(0);
        assert_eq!(Stmt::Return(None).gen(&mut ctx).unwrap(), vec![Ret]);

        let fns = &[empty_fn!("abc", Type::Int)];
        let mut ctx = Context::new();
        ctx.functions = fns;
        ctx.this_fn = Some(0);
        assert!(Stmt::Return(None).gen(&mut ctx).is_err());
    }

    {
        let s = Stmt::Return(Some(Expr::Atom(Atom::Const("123".into()))));
        let fns = &[empty_fn!("abc", Type::Void)];
        let mut ctx = Context::new();
        ctx.functions = fns;
        ctx.this_fn = Some(0);
        assert!(s.gen(&mut ctx).is_err());

        let fns = &[empty_fn!("abc", Type::Int)];
        let mut ctx = Context::new();
        ctx.functions = fns;
        ctx.this_fn = Some(0);
        assert_eq!(s.gen(&mut ctx).unwrap(), vec![Pushiw(123), Retw]);
    }
}

#[test]
fn test_gen_stmt_expr() {
    {
        let e = Expr::Bin(Box::new(Expr::Atom(Atom::Const("234".into()))),
                          Box::new(Expr::Atom(Atom::Const("456".into()))),
                          Operation::Mul);
        let s = Stmt::Expr(e);
        assert_eq!(s.gen(&mut Default::default()).unwrap(),
                   vec![Pushiw(234), Pushiw(456), Mul, Popn]);
    }
}

#[test]
fn test_gen_stmt_while() {
    {
        let s = Stmt::While(WhileStmt(Conditional {
            block: Block(vec![Stmt::Break, Stmt::Break]),
            cond: Expr::Atom(Atom::Const("1".into())),
        }));
        assert_eq!(s.gen(&mut Default::default()).unwrap(),
                   vec![Pushiw(1),
                        __Marker(Marker::Jmpzrel(4)),
                        __Marker(Marker::Jmpzrel(3)),
                        __Marker(Marker::Jmpzrel(2)),
                        __Marker(Marker::Jmprel(-4))]);
    }
}

#[test]
fn test_gen_fncall() {
    {
        let fns = &[empty_fn!("abc",
                              vec![parser::FnParam {
                                       name: "a".into(),
                                       typ: Type::Int,
                                   }],
                              Type::Int)];

        let mut ctx = Context::default();
        ctx.functions = fns;
        let e = Expr::Atom(Atom::FnCall("abc".into(), vec![Expr::Atom(Atom::Const("234".into()))]));
        assert_eq!(e.gen(&mut ctx).unwrap(),
                   vec![Pushiw(234),
                        __Marker(Marker::PushRetAddr),
                        __Marker(Marker::Call("abc".into())),
                        Popn,
                        Pushr]);
    }

    {
        let fns = &[empty_fn!("abc", vec![], Type::Void)];

        let mut ctx = Context::default();
        ctx.functions = fns;
        let e = Expr::Atom(Atom::FnCall("abc".into(), vec![]));
        assert_eq!(e.gen(&mut ctx).unwrap(),
                   vec![__Marker(Marker::PushRetAddr), __Marker(Marker::Call("abc".into()))]);
    }
}

#[test]
fn test_integration() {
    {
        // Equiv to:
        // fn a() int {
        //     return 1;
        // }
        // fn main() int {
        //     return a();
        // }
        //
        let program = &[FnItem {
                            block:
                                Block(vec![Stmt::Return(Some(Expr::Atom(Atom::Const("1".into()))))]),
                            name: "a".into(),
                            params: vec![],
                            ret: Type::Int,
                        },
                        FnItem {
                            block: Block(vec![Stmt::Return(Some(Expr::Atom(Atom::FnCall("a".into(),
                                                                              vec![]))))]),
                            name: "main".into(),
                            params: vec![],
                            ret: Type::Int,
                        }];
        assert_eq!(parse_program(program).unwrap(),
                   vec![// main start
                        __Marker(Marker::PushRetAddr),
                        __Marker(Marker::Call("a".into())),
                        Pushr,
                        Retw,
                        // a start
                        Pushiw(1),
                        Retw,]);
    }
}
