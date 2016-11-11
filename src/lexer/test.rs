use super::*;


#[cfg(test)]
fn trylex(body: &str) -> Vec<Token> {
    Lexer::new(body.as_bytes()).unwrap().lex().unwrap().into_iter().map(|t| t.0).collect()
}

#[test]
fn test_lex_ident() {
    let tokens = trylex("aaa bb2b ccc");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], Token::Ident("aaa".to_string()));
    assert_eq!(tokens[1], Token::Ident("bb2b".to_string()));
    assert_eq!(tokens[2], Token::Ident("ccc".to_string()));
}

#[test]
fn test_lex_keyword() {
    let tokens = trylex("break");
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0], Token::Keyword(Keyword::Break));
}


#[test]
fn test_lex_str() {
    let tokens = trylex("aaa \"bbb\\nccc d\\\"dd\"");
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[1], Token::String("bbb\nccc d\"dd".to_string()));
}

#[test]
fn test_lex_const() {
    let tokens = trylex("aaa -123 456-");
    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[1], Token::Const("-123".to_string()));
    assert_eq!(tokens[2], Token::Const("456".to_string()));
}

#[test]
fn test_lex_op() {
    let tokens = trylex("-+*/>>=<=<!=! === &&||");
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
               Token::Op(Operator::And),
               Token::Op(Operator::Or),
               ])
}

#[test]
fn test_lex_line_comment() {
    let tokens = trylex("+ 123 // this is ignored");
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

    let tokens = trylex(source);
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
    let tokens = trylex("()[]{},");
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

#[test]
fn test_lexer_errors() {
    {
        let result = Lexer::new("a /*".as_bytes()).unwrap().lex().err().unwrap();
        assert_eq!(result.0.len(), 1);
        let err = result.1;
        assert_eq!(err.var, ErrorVariant::EOF);
        assert_eq!(err.line, 1);
        assert_eq!(err.chr, 5);
    }

    {
        let result = Lexer::new(r###""a\x""###.as_bytes()).unwrap().lex().err().unwrap();
        let err = result.1;
        assert_eq!(err.var, ErrorVariant::InvalidStringEscape);
        assert_eq!(err.line, 1);
        assert_eq!(err.chr, 4);
    }

    {
        let result = Lexer::new(r###""a"###.as_bytes()).unwrap().lex().err().unwrap();
        let err = result.1;
        assert_eq!(err.var, ErrorVariant::EOF);
        assert_eq!(err.line, 1);
        assert_eq!(err.chr, 3);
    }

    {
        let result = Lexer::new("ab č".as_bytes()).unwrap().lex().err().unwrap();
        assert_eq!(result.0.len(), 1);
        let err = result.1;
        assert_eq!(err.var, ErrorVariant::UnknownCharacter);
        assert_eq!(err.line, 1);
        assert_eq!(err.chr, 4);
    }
}
