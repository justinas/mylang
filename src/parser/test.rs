use super::*;
use super::expr::{parse_atom, parse_comparison, parse_logexpr, parse_product, parse_sum};

#[cfg(test)]
fn str_to_tokens(s: &str) -> Vec<Token> {
    let lexer = super::super::lexer::Lexer::new(s.as_bytes()).unwrap();
    let tokens_at = lexer.lex().unwrap();
    tokens_at.into_iter().map(|t| t.0).collect::<Vec<_>>()
}

#[test]
fn test_parse() {
    {
        let body = r#"
        fn dothis() {
            var a byte;
            var b byte;
            a = 3 / 4 + 2;
            b = a + 1;
            return a;
        }

        fn dothat(d int, e byte) {
            c = 15 * 6;
            if c <= 5 {
                c = c * 2;
            }
        }
        "#;
        let tokens = str_to_tokens(body);
        let funcs = parse(&tokens).unwrap();
        assert_eq!(funcs.len(), 2);
        assert_eq!(funcs[0].name, "dothis");
        assert_eq!(funcs[0].block.0.len(), 5);
        assert_eq!(funcs[1].name, "dothat");
        assert_eq!(funcs[1].block.0.len(), 2);
        assert_eq!(funcs[1].params,
                   vec![FnParam {
                            name: "d".into(),
                            typ: Type::Int,
                        },
                        FnParam {
                            name: "e".into(),
                            typ: Type::Byte,
                        }]);
    }
}

#[test]
fn test_parse_break() {
    {
        let body = r#"break;"#;
        let tokens = str_to_tokens(body);
        let stmt = parse_stmt(&tokens).0.unwrap();
        assert_eq!(stmt, Stmt::Break);
    }
}


#[test]
fn test_parse_ifstmt() {
    {
        let body = r#"if 1 {} "#;
        let tokens = str_to_tokens(body);
        let stmt = parse_ifstmt(&tokens).0.unwrap();
        let expected = IfStmt {
            _if: Conditional {
                block: Block(vec![]),
                cond: Expr::Atom(Atom::Const("1".into())),
            },
            _eifs: vec![],
            _else: None,
        };
        assert_eq!(stmt, expected);
    }

    {
        let body = r#"if 1 {} else {}"#;
        let tokens = str_to_tokens(body);
        let stmt = parse_ifstmt(&tokens).0.unwrap();
        let expected = IfStmt {
            _if: Conditional {
                block: Block(vec![]),
                cond: Expr::Atom(Atom::Const("1".into())),
            },
            _eifs: vec![],
            _else: Some(Block(vec![])),
        };
        assert_eq!(stmt, expected);
    }

    {
        let body = r#"if 1 {} else if 2 {} else {}"#;
        let tokens = str_to_tokens(body);
        let stmt = parse_ifstmt(&tokens).0.unwrap();
        let expected = IfStmt {
            _if: Conditional {
                block: Block(vec![]),
                cond: Expr::Atom(Atom::Const("1".into())),
            },
            _eifs: vec![Conditional {
                            block: Block(vec![]),
                            cond: Expr::Atom(Atom::Const("2".into())),
                        }],
            _else: Some(Block(vec![])),
        };
        assert_eq!(stmt, expected);
    }
}

#[test]
fn test_parse_whilestmt() {
    {
        let body = r#"while 1 {} "#;
        let tokens = str_to_tokens(body);
        let stmt = parse_whilestmt(&tokens).0.unwrap();
        assert_eq!(stmt,
                   WhileStmt(Conditional {
                       block: Block(vec![]),
                       cond: Expr::Atom(Atom::Const("1".into())),
                   }));
    }
}


#[test]
fn test_parse_fn() {
    {
        let tokens = str_to_tokens("fn dothis() { var a byte; a = 3 / 4 + 2;}");
        let func = parse_fn(&tokens).0.unwrap();
        assert_eq!(func.block.0.len(), 2);
        assert_eq!(func.name, "dothis".to_string());
        assert_eq!(func.params, vec![]);
        assert_eq!(func.ret, Type::Void);
    }

    {
        let tokens = str_to_tokens("fn dothis() int { var a byte; a = 3 / 4 + 2;}");
        let func = parse_fn(&tokens).0.unwrap();
        assert_eq!(func.block.0.len(), 2);
        assert_eq!(func.name, "dothis".to_string());
        assert_eq!(func.params, vec![]);
        assert_eq!(func.ret, Type::Int);
    }
}

#[test]
fn test_parse_block() {
    {
        let tokens = &[Token::Delim(Delimiter::LCurly), Token::Delim(Delimiter::RCurly)];
        let res = parse_block(tokens);
        assert_eq!(res.0.unwrap(), Block(vec![]));
        assert_eq!(res.1, &[]);
    }


    {
        let tokens = str_to_tokens("{ var a byte; a = 3 / 4 + 2;}");
        let res = parse_block(&tokens);
        let block = res.0.unwrap();
        assert_eq!(block.0.len(), 2);
        assert_eq!(block.0[0],
                   Stmt::Decl(DeclStmt {
                       ident: "a".into(),
                       typ: Type::Byte,
                   }));
    }
}

#[test]
fn test_parse_stmt() {
    {
        let tokens = &[Token::Keyword(Keyword::Return), Token::Ident("abc".into())];

        let res = parse_stmt(tokens);
        let stmt = res.0.unwrap();
        let expected = Stmt::Return(Some(Expr::Atom(Atom::Ident("abc".into()))));
        assert_eq!(stmt, expected);
    }

    {
        let tokens = &[Token::Ident("abc".into()),
                       Token::Op(Operator::Assign),
                       Token::Const("2".into()),
                       Token::Op(Operator::Plus),
                       Token::Const("2".into())];

        let res = parse_stmt(tokens);
        let stmt = res.0.unwrap();
        let expected = Stmt::Assign(AssignStmt {
            ident: "abc".into(),
            expr: Expr::Bin(Box::new(Expr::Atom(Atom::Const("2".into()))),
                            Box::new(Expr::Atom(Atom::Const("2".into()))),
                            Operation::Add),
        });
        assert_eq!(stmt, expected);
    }

    {
        let tokens = &[Token::Keyword(Keyword::Var),
                       Token::Ident("foo".to_string()),
                       Token::Keyword(Keyword::Byte)];
        let (stmt, remaining) = parse_stmt(tokens);
        assert_eq!(stmt.unwrap(),
                   Stmt::Decl(DeclStmt {
                       ident: "foo".to_string(),
                       typ: Type::Byte,
                   }));
        assert_eq!(remaining, &[]);
    }

}

#[test]
fn test_parse_assignstmt() {
    {
        let tokens = &[Token::Ident("abc".into()),
                       Token::Op(Operator::Assign),
                       Token::Const("2".into()),
                       Token::Op(Operator::Plus),
                       Token::Const("2".into())];

        let res = parse_assignstmt(tokens);
        let stmt = res.0.unwrap();
        let expected = AssignStmt {
            ident: "abc".into(),
            expr: Expr::Bin(Box::new(Expr::Atom(Atom::Const("2".into()))),
                            Box::new(Expr::Atom(Atom::Const("2".into()))),
                            Operation::Add),
        };
        assert_eq!(stmt, expected);
    }
}

#[test]
fn test_parse_declstmt() {
    {
        let tokens = &[Token::Keyword(Keyword::Var),
                       Token::Ident("foo".to_string()),
                       Token::Keyword(Keyword::Byte)];
        let (declstmt, remaining) = parse_declstmt(tokens);
        assert_eq!(declstmt.unwrap(),
                   DeclStmt {
                       ident: "foo".to_string(),
                       typ: Type::Byte,
                   });
        assert_eq!(remaining, &[]);
    }

    {
        let tokens = &[Token::Keyword(Keyword::Var), Token::Ident("foo".to_string())];
        let (declstmt, remaining) = parse_declstmt(tokens);
        assert_eq!(declstmt, None);
        assert_eq!(remaining, tokens);
    }
}

#[test]
fn test_parse_atom() {
    {
        let tokens = &[];
        let expected: (Option<Atom>, &[Token]) = (None, &[]);
        assert_eq!(parse_atom(tokens), expected);
    }

    {
        let tokens = &[Token::Ident("abc".into())];
        let expected: (Option<Atom>, &[Token]) = (Some(Atom::Ident("abc".into())), &[]);
        assert_eq!(parse_atom(tokens), expected);
    }

    {
        let tokens = &[Token::Op(Operator::Not), Token::Ident("abc".into())];
        let res = parse_atom(tokens);
        assert_eq!(res.0.unwrap(),
                   Atom::Neg(Box::new(Atom::Ident("abc".into()))));
        assert!(res.1.is_empty());
    }


    {
        let tokens = &[Token::Delim(Delimiter::LParen),
                       Token::Const("2".into()),
                       Token::Delim(Delimiter::RParen)];
        let res = parse_atom(tokens);
        assert_eq!(res.0.unwrap(),
                   Atom::PExpr(Box::new(Expr::Atom(Atom::Const("2".into())))));
    }

    // fncall, 0 args
    {
        let tokens = &[Token::Ident("dothis".into()),
                       Token::Delim(Delimiter::LParen),
                       Token::Delim(Delimiter::RParen)];
        let res = parse_atom(tokens);
        assert_eq!(res.0.unwrap(), Atom::FnCall("dothis".into(), vec![]));
    }

    // fncall, 1 arg
    {
        let tokens = &[Token::Ident("dothis".into()),
                       Token::Delim(Delimiter::LParen),
                       Token::Const("2".into()),
                       Token::Delim(Delimiter::RParen)];
        let res = parse_atom(tokens);
        let expected = Atom::FnCall("dothis".into(), vec![Expr::Atom(Atom::Const("2".into()))]);
        assert_eq!(res.0.unwrap(), expected);
    }

    // fncall, 2 args
    {
        let tokens = &[Token::Ident("dothis".into()),
                       Token::Delim(Delimiter::LParen),
                       Token::Const("2".into()),
                       Token::Delim(Delimiter::Comma),
                       Token::Ident("ab".into()),
                       Token::Delim(Delimiter::RParen)];
        let res = parse_atom(tokens);
        let expected = Atom::FnCall("dothis".into(),
                                    vec![
                                    Expr::Atom(Atom::Const("2".into())),
                                    Expr::Atom(Atom::Ident("ab".into())),
                                    ]);
        assert_eq!(res.0.unwrap(), expected);
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
#[test]
fn test_parse_product() {
    {
        let tokens = &[Token::Const("123".into())];
        let res = parse_product(tokens);
        assert_eq!(res.0.unwrap(), Expr::Atom(Atom::Const("123".into())));
        assert!(res.1.is_empty());
    }

    {
        // 123 / a / b
        let tokens = &[Token::Const("123".into()),
                       Token::Op(Operator::Div),
                       Token::Ident("a".into()),
                       Token::Op(Operator::Div),
                       Token::Ident("b".into())];
        let res = parse_product(tokens);
        let expected =
            Expr::Bin(Box::new(Expr::Bin(Box::new(Expr::Atom(Atom::Const("123".into()))),
                                         Box::new(Expr::Atom(Atom::Ident("a".into()))),
                                         Operation::Div)),
                      Box::new(Expr::Atom(Atom::Ident("b".into()))),
                      Operation::Div);
        assert_eq!(res.0.unwrap(), expected);
        assert!(res.1.is_empty());
    }

    {
        // 123 * a * b
        let tokens = &[Token::Const("123".into()),
                       Token::Op(Operator::Mul),
                       Token::Ident("a".into()),
                       Token::Op(Operator::Mul),
                       Token::Ident("b".into())];
        let res = parse_product(tokens);
        let expected =
            Expr::Bin(Box::new(Expr::Bin(Box::new(Expr::Atom(Atom::Const("123".into()))),
                                         Box::new(Expr::Atom(Atom::Ident("a".into()))),
                                         Operation::Mul)),
                      Box::new(Expr::Atom(Atom::Ident("b".into()))),
                      Operation::Mul);
        assert_eq!(res.0.unwrap(), expected);
        assert!(res.1.is_empty());
    }

    {
        // 123 * a / b
        let tokens = &[Token::Const("123".into()),
                       Token::Op(Operator::Mul),
                       Token::Ident("a".into()),
                       Token::Op(Operator::Div),
                       Token::Ident("b".into())];
        let res = parse_product(tokens);
        let expected =
            Expr::Bin(Box::new(Expr::Bin(Box::new(Expr::Atom(Atom::Const("123".into()))),
                                         Box::new(Expr::Atom(Atom::Ident("a".into()))),
                                         Operation::Mul)),
                      Box::new(Expr::Atom(Atom::Ident("b".into()))),
                      Operation::Div);
        assert_eq!(res.0.unwrap(), expected);
        assert!(res.1.is_empty());
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
#[test]
fn test_parse_sum() {
    {
        let tokens = &[Token::Const("123".into())];
        let res = parse_sum(tokens);
        assert_eq!(res.0.unwrap(), Expr::Atom(Atom::Const("123".into())));
        assert!(res.1.is_empty());
    }

    {
        // 123 + a - b
        let tokens = &[Token::Const("123".into()),
                       Token::Op(Operator::Plus),
                       Token::Ident("a".into()),
                       Token::Op(Operator::Minus),
                       Token::Ident("b".into())];
        let res = parse_sum(tokens);
        let expected =
            Expr::Bin(Box::new(Expr::Bin(Box::new(Expr::Atom(Atom::Const("123".into()))),
                                         Box::new(Expr::Atom(Atom::Ident("a".into()))),
                                         Operation::Add)),
                      Box::new(Expr::Atom(Atom::Ident("b".into()))),
                      Operation::Sub);
        assert_eq!(res.0.unwrap(), expected);
        assert!(res.1.is_empty());
    }

    {
        // 123 * a - 456 / b
        let tokens = &[Token::Const("123".into()),
                       Token::Op(Operator::Mul),
                       Token::Ident("a".into()),
                       Token::Op(Operator::Minus),
                       Token::Const("456".into()),
                       Token::Op(Operator::Div),
                       Token::Ident("b".into())];
        let res = parse_sum(tokens);
        let left = Expr::Bin(Box::new(Expr::Atom(Atom::Const("123".into()))),
                             Box::new(Expr::Atom(Atom::Ident("a".into()))),
                             Operation::Mul);
        let right = Expr::Bin(Box::new(Expr::Atom(Atom::Const("456".into()))),
                              Box::new(Expr::Atom(Atom::Ident("b".into()))),
                              Operation::Div);

        let expected = Expr::Bin(Box::new(left), Box::new(right), Operation::Sub);
        assert_eq!(res.0.unwrap(), expected);
        assert!(res.1.is_empty());
    }
}

#[test]
fn test_parse_logexpr() {
    {
        let tokens = &[Token::Const("123".into()),
                       Token::Op(Operator::Lte),
                       Token::Ident("b".into()),

                       Token::Op(Operator::And),

                       Token::Const("456".into()),
                       Token::Op(Operator::Gte),
                       Token::Ident("b".into())];
        let res = parse_logexpr(tokens);
        assert_eq!(res.0.unwrap(),
                   Expr::Bin(Box::new(Expr::Bin(Box::new(Expr::Atom(Atom::Const("123".into()))),
                                                Box::new(Expr::Atom(Atom::Ident("b".into()))),
                                                Operation::Lte)),
                             Box::new(Expr::Bin(Box::new(Expr::Atom(Atom::Const("456".into()))),
                                                Box::new(Expr::Atom(Atom::Ident("b".into()))),
                                                Operation::Gte)),
                             Operation::And));
    }

}

#[test]
fn test_parse_comparison() {
    {
        let tokens =
            &[Token::Const("123".into()), Token::Op(Operator::Gte), Token::Ident("b".into())];
        let res = parse_comparison(tokens);
        assert_eq!(res.0.unwrap(),
                   Expr::Bin(Box::new(Expr::Atom(Atom::Const("123".into()))),
                             Box::new(Expr::Atom(Atom::Ident("b".into()))),
                             Operation::Gte));
    }

}
