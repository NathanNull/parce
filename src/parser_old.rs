#![allow(unused)]

use super::{range::Range, tuple::Tuple};
use crate::p;
use fancy_regex::Regex;
use itertools::Itertools;
use std::{
    ops::{BitAnd, BitOr, Mul, Not, Shl, Shr},
    str::FromStr,
    sync::{Arc, Weak},
};

pub trait ParserFriendly: Clone + Send + Sync {}
impl<T: Clone + Send + Sync> ParserFriendly for T {}

trait ParserFn<Out: Tuple + 'static>:
    Fn(&str) -> Option<(Out::Destructured, &str)> + Send + Sync
{
    fn box_clone(&self) -> Box<dyn ParserFn<Out>>;
}
impl<
        Out: Tuple + 'static,
        T: Fn(&str) -> Option<(Out::Destructured, &str)> + ParserFriendly + 'static,
    > ParserFn<Out> for T
{
    fn box_clone(&self) -> Box<dyn ParserFn<Out>> {
        Box::new(self.clone())
    }
}

pub struct MultiParser<Out: Tuple + 'static>(Box<dyn ParserFn<Out>>);
pub type Parser<Out> = MultiParser<(Out,)>;

impl<Out: Tuple> Clone for MultiParser<Out> {
    fn clone(&self) -> Self {
        Self(self.0.box_clone())
    }
}

impl<Out: Tuple> MultiParser<Out> {
    pub fn parse<'a>(&self, input: &'a str) -> Option<(Out::Destructured, &'a str)> {
        (self.0)(input)
    }
    fn parse_raw<'a>(&self, input: &'a str) -> Option<(Out, &'a str)> {
        (self.0)(input).map(|(o, res)| (Out::from_destructured(o), res))
    }
    pub fn parse_full(&self, input: &str) -> Result<Out::Destructured, String> {
        let (out, res) = self
            .parse(input)
            .ok_or_else(|| "Parser failed to parse".to_string())?;
        if res.is_empty() {
            Ok(out)
        } else {
            Err(format!("Some input remaining: '{res}'"))
        }
    }
    fn map_raw<T: Tuple + 'static>(
        self,
        func: impl Fn(Out) -> Option<T> + ParserFriendly + 'static,
    ) -> MultiParser<T> {
        MultiParser(Box::new(move |input| {
            let (out, res) = self.parse_raw(input)?;
            Some(((func)(out)?.destructure(), res))
        }))
    }
    pub fn try_map<T: 'static>(
        self,
        func: impl Fn(Out::Destructured) -> Option<T> + ParserFriendly + 'static,
    ) -> Parser<T> {
        self.map_raw(move |out| (func)(out.destructure()).map(|t| (t,)))
    }
    pub fn map<T: 'static>(
        self,
        func: impl Fn(Out::Destructured) -> T + ParserFriendly + 'static,
    ) -> Parser<T> {
        self.try_map(move |out| Some((func)(out)))
    }
    pub fn sep_by<Sep: Tuple>(
        self,
        sep: MultiParser<Sep>,
        range: impl Into<Range>,
    ) -> Parser<Vec<Out::Destructured>> {
        let range = range.into();
        let p1 = self.clone();
        let p2 = (sep >> self) * range.shift(-1);
        let p_total = p1.and_raw(p2);
        let mut p = p_total.map(move |(first, (mut rest,))| {
            rest.insert(0, first.destructure());
            rest
        });
        if range.contains(0) {
            p = p | p!(()).map(|()| vec![]);
        }
        p
    }
    pub fn matches<T: ParserFriendly, Tests: IntoIterator<Item = (Out::Destructured, T)>>(
        self,
        tests: Tests,
        fail: T,
    ) -> Parser<T>
    where
        Out::Destructured: PartialEq + ParserFriendly,
    {
        let tests = tests.into_iter().collect::<Vec<_>>();
        self.map(move |out| {
            if let Some((_, success)) = tests.iter().find(|(o, _)| *o == out) {
                success.clone()
            } else {
                fail.clone()
            }
        })
    }
    pub fn maybe(self) -> Parser<Option<Out::Destructured>>
    where
        Out::Destructured: ParserFriendly,
    {
        (self * (0..=1)).map(|v| v.first().cloned())
    }
    fn and_raw<O2: Tuple>(self, rhs: MultiParser<O2>) -> MultiParser<(Out, O2)> {
        MultiParser(Box::new(move |input| {
            self.parse_raw(input)
                .and_then(|(o1, res)| Some((o1, rhs.parse_raw(res)?)))
                .map(|(o1, (o2, res))| ((o1, o2), res))
        }))
    }
}

impl<O1: Tuple, O2: Tuple> BitAnd<MultiParser<O2>> for MultiParser<O1> {
    type Output = MultiParser<O1::Concat<O2>>;

    fn bitand(self, rhs: MultiParser<O2>) -> Self::Output {
        self.and_raw(rhs).map_raw(|(o1, o2)| Some(o1.concat(o2)))
    }
}

impl<Out: Tuple> BitOr<MultiParser<Out>> for MultiParser<Out> {
    type Output = MultiParser<Out>;

    fn bitor(self, rhs: MultiParser<Out>) -> Self::Output {
        MultiParser(Box::new(move |input| {
            self.parse(input).or_else(|| rhs.parse(input))
        }))
    }
}

impl<O1: Tuple, O2: Tuple> Shr<MultiParser<O2>> for MultiParser<O1> {
    type Output = MultiParser<O2>;

    fn shr(self, rhs: MultiParser<O2>) -> Self::Output {
        !self & rhs
    }
}

impl<O1: Tuple, O2: Tuple> Shl<MultiParser<O2>> for MultiParser<O1> {
    type Output = MultiParser<O1>;

    fn shl(self, rhs: MultiParser<O2>) -> Self::Output {
        self.and_raw(!rhs).map_raw(|(o1, _)| Some(o1))
    }
}

impl<Out: Tuple, R: Into<Range>> Mul<R> for MultiParser<Out> {
    type Output = Parser<Vec<Out::Destructured>>;

    fn mul(self, rhs: R) -> Self::Output {
        let range = rhs.into();
        MultiParser(Box::new(move |mut input| {
            let mut res = vec![];
            while res.len() <= range.max() {
                if let Some((out, rem)) = self.parse(input) {
                    input = rem;
                    res.push(out);
                } else {
                    break;
                }
            }
            if range.contains(res.len()) {
                Some((res, input))
            } else {
                None
            }
        }))
    }
}

impl<Out: Tuple> Not for MultiParser<Out> {
    type Output = MultiParser<()>;

    fn not(self) -> Self::Output {
        self.map_raw(|_| Some(()))
    }
}

impl<F: Fn(char) -> bool + ParserFriendly + 'static> From<F> for Parser<char> {
    fn from(pred: F) -> Self {
        MultiParser(Box::new(move |input| {
            let c = input.chars().next()?;
            if (pred)(c) {
                Some((c, &input[1..]))
            } else {
                None
            }
        }))
    }
}

impl From<char> for Parser<char> {
    fn from(c: char) -> Self {
        (move |test| test == c).into()
    }
}

impl From<()> for MultiParser<()> {
    fn from(_: ()) -> Self {
        MultiParser(Box::new(|input| Some(((), input))))
    }
}

#[macro_export]
macro_rules! p {
    ($v: expr) => {
        $crate::repl::parser::MultiParser::from($v)
    };
}

pub fn p_oneof(c: impl IntoIterator<Item = char>) -> Parser<char> {
    let chars = c.into_iter().collect::<Vec<_>>();
    (move |c| chars.contains(&c)).into()
}

pub trait ParserIterator<Out: Tuple + 'static>: Iterator<Item = MultiParser<Out>> + Sized {
    fn to_vec(self) -> Parser<Vec<Out::Destructured>> {
        let parsers = self.collect::<Vec<_>>();
        MultiParser(Box::new(move |mut input| {
            let mut out = vec![];
            for parser in &parsers {
                let (o_n, res) = parser.parse(input)?;
                input = res;
                out.push(o_n);
            }
            Some((out, input))
        }))
    }
}
impl<Out: Tuple + 'static, T: Iterator<Item = MultiParser<Out>>> ParserIterator<Out> for T {}

impl<'a> From<&'a str> for Parser<Arc<str>> {
    fn from(value: &'a str) -> Self {
        value.to_string().into()
    }
}

impl From<String> for Parser<Arc<str>> {
    fn from(value: String) -> Self {
        value
            .chars()
            .map(Parser::from)
            .to_vec()
            .map(|chars| Arc::from(chars.into_iter().collect::<String>().as_str()))
    }
}

pub struct Recursive<T: 'static> {
    inner: Weak<Parser<T>>,
}

impl<T: 'static> Recursive<T> {
    fn parser(self) -> Parser<T> {
        MultiParser(Box::new(move |input| {
            self.inner.upgrade().expect("yeah").parse_raw(input).map(|((out,), res)| (out, res))
        }))
    }
}

pub fn p_recursive<T>(f: impl FnOnce(Parser<T>) -> Parser<T>) -> Parser<T> {
    let arc = Arc::<Parser<T>>::new_cyclic(|arc| {
        let rec = Recursive { inner: arc.clone() };
        f(rec.parser())
    });
    MultiParser(Box::new(move |input| {
        arc.parse_raw(input).map(|((out,), res)| (out, res))
    }))
}

pub fn p_regex(s: impl Into<String>) -> Parser<Arc<str>> {
    let regex = Regex::new(&s.into()).expect("Invalid regex argument");
    MultiParser(Box::new(move |input| {
        if let Some(Ok(m)) = regex
            .find_iter(input)
            .find(|m| m.as_ref().is_ok_and(|m| m.start() == 0))
        {
            let match_str = m.as_str();
            Some((Arc::from(match_str), &input[match_str.len()..]))
        } else {
            None
        }
    }))
}

pub fn p_alnum() -> Parser<Arc<str>> {
    (p!(|c: char| c.is_alphanumeric()) * (1..)).map(|r| Arc::from(r.iter().join("").as_str()))
}

pub fn p_newline() -> Parser<char> {
    (p!('\r') * (0..=1)) >> p!('\n')
}

pub fn p_n<Int: FromStr>() -> Parser<Int> {
    ((p!('-') * (0..=1)) & (p!(|c: char| c.is_ascii_digit()) * ..))
        .try_map(|(sign, chars)| sign.into_iter().chain(chars).join("").parse::<Int>().ok())
}
