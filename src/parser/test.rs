use super::*;
use super::expr::*;

#[test]
fn test_parse_declstmt() {
    {
        let tokens = &[Token::Keyword(Keyword::Var),
                       Token::Ident("foo".to_string()),
                       Token::Keyword(Keyword::Byte)];
        let (declstmt, remaining) = parse_declstmt(tokens);
        assert_eq!(declstmt,
                   Some(DeclStmt {
                       ident: "foo".to_string(),
                       typ: Type::Byte,
                   }));
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
}

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

#[test]
fn test_parse_sum() {
    {
        let tokens = &[Token::Const("123".into())];
        let res = parse_product(tokens);
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
