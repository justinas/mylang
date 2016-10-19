use super::{Lexer, Token};

#[test]
fn test_lex_ident() {
    let tokens = Lexer::new("aaa bbb ccc".as_bytes()).unwrap().lex().unwrap();
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], Token::Ident("aaa".to_string()));
    assert_eq!(tokens[1], Token::Ident("bbb".to_string()));
    assert_eq!(tokens[2], Token::Ident("ccc".to_string()));
}
