use super::{Delimiter, Lexer, Operator, Token};

#[test]
fn test_lex_ident() {
    let tokens = Lexer::new("aaa bb2b ccc".as_bytes()).unwrap().lex().unwrap();
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], Token::Ident("aaa".to_string()));
    assert_eq!(tokens[1], Token::Ident("bb2b".to_string()));
    assert_eq!(tokens[2], Token::Ident("ccc".to_string()));
}

#[test]
fn test_lex_str() {
    let tokens = Lexer::new("aaa \"bbb\\nccc d\\\"dd\"".as_bytes()).unwrap().lex().unwrap();
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[1], Token::String("bbb\nccc d\"dd".to_string()));
}

#[test]
fn test_lex_const() {
    let tokens = Lexer::new("aaa -123 456-".as_bytes()).unwrap().lex().unwrap();
    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[1], Token::Const("-123".to_string()));
    assert_eq!(tokens[2], Token::Const("456".to_string()));
}

#[test]
fn test_lex_op() {
    let tokens = Lexer::new("-+*/>>=<=<!=! ===".as_bytes()).unwrap().lex().unwrap();
    assert_eq!(tokens,
               vec![
               Token::Op(Operator::Minus),
               Token::Op(Operator::Plus),
               Token::Op(Operator::Mul),
               Token::Op(Operator::Div),
               Token::Op(Operator::Gt),
               Token::Op(Operator::Gte),
               Token::Op(Operator::Lte),
               Token::Op(Operator::Lt),
               Token::Op(Operator::Neq),
               Token::Op(Operator::Not),
               Token::Op(Operator::Eq),
               Token::Op(Operator::Assign),
               ])
}

#[test]
fn test_lex_line_comment() {
    let tokens = Lexer::new("+ 123 // this is ignored".as_bytes()).unwrap().lex().unwrap();
    assert_eq!(tokens,
               vec![Token::Op(Operator::Plus), Token::Const("123".to_string())])
}

#[test]
fn test_lex_multiline_comment() {
    let source = r###"
        a + /*
        multi-line
        comment
        */ *"###;

    let tokens = Lexer::new(source.as_bytes()).unwrap().lex().unwrap();
    assert_eq!(tokens,
               vec![Token::Ident("a".to_string()),
                    Token::Op(Operator::Plus),
                    Token::Op(Operator::Mul)])
}

#[test]
fn test_lex_multiline_comment_no_end() {
    {
        let source = r###"
        a + /*
        multi-line
        comment
        "###;

        assert!(Lexer::new(source.as_bytes()).unwrap().lex().is_err());
    }
    {
        let source = r###"
        a + /*
        multi-line
        comment
        *"###;

        assert!(Lexer::new(source.as_bytes()).unwrap().lex().is_err());
    }
}

#[test]
fn test_lex_delimiters() {
    let tokens = Lexer::new("()[]{},".as_bytes()).unwrap().lex().unwrap();
    assert_eq!(tokens,
               vec![
               Token::Delim(Delimiter::LParen),
               Token::Delim(Delimiter::RParen),
               Token::Delim(Delimiter::LSquare),
               Token::Delim(Delimiter::RSquare),
               Token::Delim(Delimiter::LCurly),
               Token::Delim(Delimiter::RCurly),
               Token::Delim(Delimiter::Comma),
    ])
}
