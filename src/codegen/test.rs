use super::super::parser::{self, Atom, Expr, Operation, Stmt};
use super::{Context, Function, Gen, Symbol, Typed};
use super::Instruction;
use super::Instruction::*;

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
fn test_symbol_stack() {
    {
        let mut ctx = Context::new();
        ctx.push_frame(vec![Symbol::new("b"), Symbol::new("a")]);
        ctx.push_frame(vec![Symbol::new("a")]);
        assert_eq!(&ctx.symbol_stack[1][0] as *const _,
                   ctx.find_symbol("a").unwrap() as *const _)
    }

    {
        let mut ctx = Context::new();
        ctx.push_frame(vec![Symbol::new("a")]);
        ctx.push_frame(vec![Symbol::new("b"), Symbol::new("a")]);
        assert_eq!(&ctx.symbol_stack[1][1] as *const _,
                   ctx.find_symbol("a").unwrap() as *const _)
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
