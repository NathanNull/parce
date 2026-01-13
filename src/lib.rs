pub mod parser;
mod range;
mod tuple;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Parser, ValidToken};
    use std::sync::Arc;

    #[test]
    fn basics() {
        p![(c >> c.is_numeric()), (c >> c.is_numeric())];
        let p = p![
            ('c' >> |_| 15),
            (('h' * 3..=3)?),
            ('c' | (c >> c.is_numeric())),
            'b',
            (rec(|_| 'c')),
            (),
            (!"abcde"),
            ('c'?)
        ];
        assert_eq!(
            p.parse_full("c7bcc"),
            Some((15, None, '7', 'b', 'c', Some('c')))
        );
        assert_eq!(
            p.parse_full("chhh7bc"),
            Some((15, Some(vec!['h'; 3]), '7', 'b', 'c', None))
        );
        assert_eq!(p.parse_full("test"), None);
    }

    #[test]
    fn array() {
        let p = p!['c', 'd', 'e'];
        assert_eq!(p.parse_full("ced"), None);
        assert_eq!(p.parse_full("cde"), Some(('c', 'd', 'e')));
    }

    #[test]
    fn regex() {
        let p = p!((r r"\w*") & !(r r"\r\n"));
        assert_eq!(p.parse_full("as??df \r\n"), None);
        assert_eq!(p.parse_full("asdf\r\n"), Some(Arc::from("asdf")));
    }

    #[derive(Clone, PartialEq, Debug)]
    struct TestToken(usize);
    impl ValidToken for TestToken {}

    #[test]
    fn custom() {
        let p = p!(&[TestToken]: (TestToken(0)) & (TestToken(1)));
        assert_eq!(
            p.parse_full(&[TestToken(0), TestToken(1)]),
            Some((TestToken(0), TestToken(1)))
        );
        assert_eq!(p.parse_full(&[TestToken(0), TestToken(2)]), None);
    }
}
