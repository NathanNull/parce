pub mod parser;
mod range;
mod tuple;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Parser, ValidToken, recursive::p_recursive};
    use std::sync::Arc;

    #[test]
    fn basics() {
        let p = parser![
            ('c' >> |_| 15),
            (('h' * 3..=3)?),
            ('c' | (c >> c.is_numeric())),
            'b',
            (rec<(char,)>(|_| 'c')),
            (),
            (!"abcde"),
            ('c'?)
        ];

        assert_eq!(
            p.parse_full("c7bcabcdec"),
            Ok((15, None, '7', 'b', 'c', Some('c')))
        );
        assert_eq!(
            p.parse_full("chhh7bcabcde"),
            Ok((15, Some(vec!['h'; 3]), '7', 'b', 'c', None))
        );
        assert!(p.parse_full("test").is_err());
    }

    #[test]
    fn recursive() {
        let p1 = p_recursive::<str, (String,), _>(|p| {
            'a'.and(p.maybe())
                .map(|(c, rest)| c.to_string() + rest.unwrap_or(String::new()).as_str())
        });

        let p2 = 'b'.and(p1);

        assert_eq!(p2.parse_full("baaaaa"), Ok(('b', String::from("aaaaa"))));
        assert!(p2.parse_full("aabaa").is_err());
    }

    #[test]
    fn array() {
        let p = parser!['c', 'd', 'e'];
        assert!(p.parse_full("ced").is_err());
        assert_eq!(p.parse_full("cde"), Ok(('c', 'd', 'e')));
    }

    #[test]
    fn regex() {
        let p = parser!((r r"\w*") & !(r r"\r\n"));
        assert!(p.parse_full("as??df \r\n").is_err());
        assert_eq!(p.parse_full("asdf\r\n"), Ok(Arc::from("asdf")));
    }

    #[derive(Clone, PartialEq, Debug)]
    struct TestToken(usize);
    impl ValidToken for TestToken {}

    #[test]
    fn custom() {
        let p = parser!(&[TestToken]: (TestToken(0)) & (TestToken(1)));
        assert_eq!(
            p.parse_full(&[TestToken(0), TestToken(1)]),
            Ok((TestToken(0), TestToken(1)))
        );
        assert!(p.parse_full(&[TestToken(0), TestToken(2)]).is_err());
    }
}
