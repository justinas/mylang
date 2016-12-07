use super::super::parser::{self, Atom, Expr, Operation, Stmt, Type};
use super::{Context, Function, Gen, Symbol, Typed};
use super::Instruction;
use super::Instruction::*;
use super::Marker;

macro_rules! empty_fn {
    ($name:expr, $ret:expr) => {
        parser::FnItem{ block: parser::Block(vec![]), name: $name.into(), params: vec![], ret: $ret}
    };
    ($name:expr, $params:expr, $ret:expr) => {
        parser::FnItem{ block: parser::Block(vec![]), name: $name.into(), params: $params, ret: $ret}
    };
}

#[test]
fn test_resolve_locals() {
    {
        let mut item = parser::FnItem {
            block: parser::Block(vec![]),
            name: "a".into(),
            params: vec![],
            ret: parser::Type::Void,
        };
        assert_eq!(Function::new(item).unwrap().locals.len(), 0);
    }

    {
        let mut item = parser::FnItem {
            block: parser::Block(vec![]),
            name: "a".into(),
            params: vec![parser::FnParam {
                             name: "l1".into(),
                             typ: parser::Type::Int,
                         },
                         parser::FnParam {
                             name: "l2".into(),
                             typ: parser::Type::Byte,
                         }],
            ret: parser::Type::Void,
        };
        let f = Function::new(item).unwrap();
        assert_eq!(f.locals.len(), 2);
        assert_eq!(f.locals["l1"].typ, parser::Type::Int);
        assert_eq!(f.locals["l2"].typ, parser::Type::Byte);
    }

    {
        let mut item = parser::FnItem {
            block: parser::Block(vec![
                parser::Stmt::Block(parser::Block(vec![])),
                parser::Stmt::Decl(parser::DeclStmt{ident: "l3".into(), typ: parser::Type::Int}),
            ]),
            name: "a".into(),
            params: vec![parser::FnParam {
                             name: "l1".into(),
                             typ: parser::Type::Int,
                         },
                         parser::FnParam {
                             name: "l2".into(),
                             typ: parser::Type::Byte,
                         }],
            ret: parser::Type::Void,
        };
        let f = Function::new(item).unwrap();
        assert_eq!(f.locals.len(), 3);
        assert_eq!(f.locals["l1"].typ, parser::Type::Int);
        assert_eq!(f.locals["l2"].typ, parser::Type::Byte);
        assert_eq!(f.locals["l3"].typ, parser::Type::Int);
    }
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
    {
        let mut ctx = Context::new();
        ctx.push_frame(vec![Symbol::new("b"), Symbol::new("a")]);
        ctx.push_frame(vec![Symbol::new("a")]);
        assert_eq!(ctx.find_symbol_location("a").unwrap(), -3)
    }

    {
        let mut ctx = Context::new();
        ctx.push_frame(vec![Symbol::new("a")]);
        ctx.push_frame(vec![Symbol::new("b"), Symbol::new("a")]);
        assert_eq!(ctx.find_symbol_location("a").unwrap(), -3)
    }

    {
        let mut ctx = Context::new();
        ctx.arguments.push(Symbol::new("a1"));
        ctx.arguments.push(Symbol::new("a2"));
        ctx.push_frame(vec![Symbol::new("a")]);
        ctx.push_frame(vec![Symbol::new("b"), Symbol::new("a")]);
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
                        __Marker(Marker::PushCurPC),
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
                   vec![__Marker(Marker::PushCurPC), __Marker(Marker::Call("abc".into()))]);
    }
}
