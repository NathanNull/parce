use crate::parser;

use super::{Parser, ParserIterator, RawParser};
use fancy_regex::Regex;
use itertools::Itertools;
use std::{str::FromStr, sync::Arc};

trait AsStr {
    fn chars(&self) -> impl Iterator<Item = char>;
}
impl AsStr for &str {
    fn chars(&self) -> impl Iterator<Item = char> {
        str::chars(self)
    }
}
impl AsStr for String {
    fn chars(&self) -> impl Iterator<Item = char> {
        self.as_str().chars()
    }
}
impl<T: AsStr + Clone + Send + Sync + 'static> Parser<str> for T {
    type Out = (Arc<str>,);

    fn box_clone(&self) -> Arc<dyn Parser<str, Out = Self::Out>> {
        Arc::new(self.clone())
    }

    fn parse_raw<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.chars()
            .to_vec()
            .map(|chars| Arc::from(chars.into_iter().collect::<String>().as_str()))
            .parse_raw(input)
    }
}

// Need this for the type annotations apparently
fn regex_inner(regex: Regex) -> impl Fn(&str) -> Option<((Arc<str>,), &str)> + Clone + 'static {
    move |input: &str| -> Option<((Arc<str>,), &str)> {
        let match_str: Arc<str> = if let Some(Ok(m)) = regex
            .find_iter(input)
            .find(|m| m.as_ref().is_ok_and(|m| m.start() == 0))
        {
            Arc::from(m.as_str())
        } else {
            return None;
        };
        let match_len = match_str.len();
        Some(((match_str,), &input[match_len..]))
    }
}

pub fn p_regex(s: impl Into<String>) -> impl Parser<str, Out = (Arc<str>,)> {
    let regex = Regex::new(&s.into()).expect("Invalid regex argument");
    RawParser::<str, (Arc<str>,)>(Arc::new(regex_inner(regex)))
}

pub fn p_alnum() -> impl Parser<str, Out = (Arc<str>,)> {
    parser!(((c >> c.is_alphanumeric()) * 1..) >> |r| Arc::<str>::from(r.iter().join("").as_str()))
}

pub fn p_newline() -> impl Parser<str, Out = (char,)> {
    parser!((!('r' * 0..=1)) & '\n')
}

pub fn p_n<Int: FromStr + Clone + Send + Sync + 'static>() -> impl Parser<str, Out = (Int,)> {
    parser!(('-' * 0..=1) & ((c >> c.is_ascii_digit()) * ..))
        .try_map(|(sign, chars)| str::parse::<Int>(&sign.into_iter().chain(chars).join("")).ok())
}
