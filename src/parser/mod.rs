#![allow(unused)]

use crate::{
    range::{Range, RangeIndex},
    tuple::Tuple,
};
use fancy_regex::Regex;
use std::{
    marker::PhantomData,
    ops::{Deref, Index},
    sync::{Arc, Weak},
};

pub mod recursive;
pub mod stringbased;

pub trait TokenSlice: RangeIndex + 'static {
    type Token;
    fn is_empty(&self) -> bool;
    fn first(&self) -> Option<Self::Token>;
}

impl TokenSlice for str {
    type Token = char;
    fn is_empty(&self) -> bool {
        str::is_empty(self)
    }
    fn first(&self) -> Option<char> {
        self.chars().next()
    }
}
impl<T: ValidToken + Clone> TokenSlice for [T] {
    type Token = T;

    fn is_empty(&self) -> bool {
        self.is_empty()
    }

    fn first(&self) -> Option<Self::Token> {
        self.first().cloned()
    }
}

pub trait ValidToken: 'static {}
impl ValidToken for char {}

#[macro_export]
macro_rules! parser_token {
    ($token:ty) => {
        impl ValidToken for $token {}
    };
}

#[allow(unused)]
pub trait Parser<Slice: TokenSlice + ?Sized> {
    type Out: Tuple + 'static;

    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>>;
    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)>;
    fn parse<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(<Self::Out as Tuple>::Destructured, &'a Slice)> {
        self.parse_raw(input)
            .map(|(out, res)| (out.destructure(), res))
    }
    fn parse_full(&self, input: &Slice) -> Option<<Self::Out as Tuple>::Destructured> {
        let (out, res) = self.parse(input)?;
        if res.is_empty() { Some(out) } else { None }
    }
    fn and_raw<Rhs: Parser<Slice>>(self, rhs: Rhs) -> AndParser<Slice, Self, Rhs>
    where
        Self: Sized,
    {
        AndParser(self, rhs, PhantomData)
    }
    fn and<Rhs: Parser<Slice>>(
        self,
        rhs: Rhs,
    ) -> impl Parser<Slice, Out = <Self::Out as Tuple>::Concat<Rhs::Out>>
    where
        Self: Sized,
    {
        AndParser(self, rhs, PhantomData).map_raw(|(o1, o2)| Some(o1.concat(o2)))
    }
    fn or<Rhs: Parser<Slice, Out = Self::Out>>(
        self,
        rhs: Rhs,
    ) -> impl Parser<Slice, Out = Self::Out>
    where
        Self: Sized,
    {
        OrParser(self, rhs, PhantomData)
    }
    fn mul(
        self,
        range: impl Into<Range>,
    ) -> impl Parser<Slice, Out = (Vec<<Self::Out as Tuple>::Destructured>,)>
    where
        Self: Sized,
    {
        MulParser(self, range.into(), PhantomData)
    }
    fn maybe(self) -> impl Parser<Slice, Out = (Option<<Self::Out as Tuple>::Destructured>,)>
    where
        Self: Sized,
        <Self::Out as Tuple>::Destructured: Clone,
    {
        self.mul(0..=1).map(|vec| vec.first().cloned())
    }
    fn map_raw<Res: Tuple + 'static, F: Fn(Self::Out) -> Option<Res> + Clone + 'static>(
        self,
        map: F,
    ) -> impl Parser<Slice, Out = Res>
    where
        Self: Sized,
    {
        MapParser(self, Box::new(map))
    }
    fn try_map<
        T: Clone + 'static,
        F: Fn(<Self::Out as Tuple>::Destructured) -> Option<T> + Clone + 'static,
    >(
        self,
        map: F,
    ) -> impl Parser<Slice, Out = (T,)>
    where
        Self: Sized,
    {
        self.map_raw(move |out| map(out.destructure()).map(|res| (res,)))
    }
    fn map<T: Clone + 'static, F: Fn(<Self::Out as Tuple>::Destructured) -> T + Clone + 'static>(
        self,
        map: F,
    ) -> impl Parser<Slice, Out = (T,)>
    where
        Self: Sized,
    {
        self.try_map(move |out| Some(map(out)))
    }
    fn matches<
        T: Clone + 'static,
        Tests: IntoIterator<Item = (<Self::Out as Tuple>::Destructured, T)>,
    >(
        self,
        tests: Tests,
        fail: T,
    ) -> impl Parser<Slice, Out = (T,)>
    where
        Self: Sized,
        <Self::Out as Tuple>::Destructured: PartialEq + Clone,
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
    fn ignore(self) -> impl Parser<Slice, Out = ()>
    where
        Self: Sized,
    {
        self.map_raw(|_| Some(()))
    }
    fn sep_by<Sep: Parser<Slice>>(
        self,
        sep: Sep,
        range: impl Into<Range>,
    ) -> impl Parser<Slice, Out = (Vec<<Self::Out as Tuple>::Destructured>,)>
    where
        Self: Sized,
        Self::Out: 'static,
        Slice: 'static,
    {
        let range = range.into();
        let p2 = sep.ignore().and(self.box_clone()).mul(range.shift(-1));
        let p_total = self.and_raw(p2);
        let p = p_total.map(move |(first, (mut rest,))| {
            rest.insert(0, first.destructure());
            rest
        });

        let rhs = p.box_clone().or(().map(|()| vec![]));
        p.choose(rhs, !range.contains(0))
    }

    fn choose<Rhs: Parser<Slice, Out = Self::Out>>(
        self,
        rhs: Rhs,
        use_rhs: bool,
    ) -> impl Parser<Slice, Out = Self::Out>
    where
        Self: Sized,
    {
        self.map_raw(move |o| if use_rhs { None } else { Some(o) })
            .or(rhs.map_raw(move |o| if use_rhs { Some(o) } else { None }))
    }
}

impl<'b, Slice: TokenSlice + ?Sized, Out: Tuple + 'static> Parser<Slice>
    for Box<dyn Parser<Slice, Out = Out> + 'b>
{
    type Out = Out;
    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        self.deref().parse_raw(input)
    }
    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        self.deref().box_clone()
    }
}

impl<'b, Slice: TokenSlice + ?Sized, Out: Tuple + 'static> Parser<Slice>
    for Arc<dyn Parser<Slice, Out = Out> + 'b>
{
    type Out = Out;
    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        self.deref().parse_raw(input)
    }
    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        self.deref().box_clone()
    }
}

impl<Slice: TokenSlice + ?Sized> Parser<Slice> for () {
    type Out = ();
    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        Some(((), input))
    }
    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(())
    }
}

macro_rules! box_clone {
    ($(($($vis:tt)*))? $newname: ident$([$($gen: ident$(:($($bound: tt)*))?),+])?: $($traitname:tt)+ ) => {
        $($($vis)*)? trait $newname$(<$($gen$(:$($bound)*)?),+>)?: $($traitname)+ + 'static {
            fn as_box(&self) -> Box<dyn $newname$(<$($gen),+>)?>;
            fn get_ref(&self) -> &dyn $($traitname)+;
        }

        impl<T$($(,$gen$(:$($bound)*)?)+)?> $newname$(<$($gen),+>)? for T
        where
            T: $($traitname)+ + Clone + 'static,
        {
            fn as_box(&self) -> Box<dyn $newname$(<$($gen),+>)?> {
                Box::new(self.clone())
            }
            fn get_ref(&self) -> &dyn $($traitname)+ {
                self
            }
        }

        impl$(<$($gen:$($($bound)* + )?'static),+>)? Clone for Box<dyn $newname$(<$($gen),+>)? + 'static> {
            fn clone(&self) -> Self {
                $newname::as_box(self)
            }
        }
    };
}

box_clone!((pub) FnClone [Args,Out] : Fn(Args)->Out);

#[derive(Clone)]
pub struct PredicateParser<Token: 'static>(Box<dyn FnClone<Token, bool>>);

impl<Token> PredicateParser<Token> {
    pub fn new<F: FnClone<Token, bool> + 'static>(func: F) -> Self {
        Self(Box::new(func))
    }
}

impl<Slice: TokenSlice + ?Sized> Parser<Slice> for PredicateParser<Slice::Token> where Slice::Token: Clone {
    type Out = (Slice::Token,);
    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<((Slice::Token,), &'a Slice)> {
        if let Some(tok) = input.first()
            && self.0.get_ref()(tok.clone())
        {
            Some(((tok.clone(),), &input[1..]))
        } else {
            None
        }
    }
    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(self.clone())
    }
}

impl<Token: ValidToken, Slice: TokenSlice<Token = Token> + ?Sized> Parser<Slice> for Token where Self: PartialEq + Clone + 'static {
    type Out = (Slice::Token,);

    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(self.clone())
    }

    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        if let Some(first) = input.first()
            && first == *self
        {
            Some(((first.clone(),), &input[1..]))
        } else {
            None
        }
    }
}

pub struct AndParser<Slice: TokenSlice + ?Sized, P1: Parser<Slice>, P2: Parser<Slice>>(
    P1,
    P2,
    PhantomData<Slice>,
);

impl<Slice: TokenSlice + ?Sized, P1: Parser<Slice>, P2: Parser<Slice>> Parser<Slice>
    for AndParser<Slice, P1, P2>
{
    type Out = (P1::Out, P2::Out);
    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        let (o1, res1) = self.0.parse_raw(input)?;
        let (o2, res2) = self.1.parse_raw(res1)?;
        Some(((o1, o2), res2))
    }
    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(AndParser(
            self.0.box_clone(),
            self.1.box_clone(),
            PhantomData,
        ))
    }
}

pub struct OrParser<Slice: TokenSlice + ?Sized, P1: Parser<Slice>, P2: Parser<Slice, Out = P1::Out>>(
    P1,
    P2,
    PhantomData<P1::Out>,
);

impl<Slice: TokenSlice + ?Sized, P1: Parser<Slice>, P2: Parser<Slice, Out = P1::Out>> Parser<Slice>
    for OrParser<Slice, P1, P2>
{
    type Out = P1::Out;
    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        self.0.parse_raw(input).or_else(|| self.1.parse_raw(input))
    }
    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(OrParser(
            self.0.box_clone(),
            self.1.box_clone(),
            PhantomData,
        ))
    }
}

pub struct MulParser<Slice: TokenSlice + ?Sized, P: Parser<Slice>>(P, Range, PhantomData<Slice>);

impl<Slice: TokenSlice + ?Sized, P: Parser<Slice>> Parser<Slice> for MulParser<Slice, P> {
    type Out = (Vec<<P::Out as Tuple>::Destructured>,);

    fn parse_raw<'a>(
        &self,
        mut input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        let mut res = vec![];
        while res.len() <= self.1.max
            && let Some((out, rem)) = self.0.parse(input)
        {
            input = rem;
            res.push(out);
        }
        if self.1.contains(res.len()) {
            Some(((res,), input))
        } else {
            None
        }
    }
    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(MulParser(self.0.box_clone(), self.1.clone(), PhantomData))
    }
}

pub struct MapParser<Slice: TokenSlice + ?Sized, P: Parser<Slice>, Res: Tuple>(
    P,
    Box<dyn FnClone<P::Out, Option<Res>>>,
);

impl<Slice: TokenSlice + ?Sized, P1: Parser<Slice>, Res: Tuple + 'static> Parser<Slice>
    for MapParser<Slice, P1, Res>
{
    type Out = Res;
    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        self.0
            .parse_raw(input)
            .and_then(|(out, res)| Some((self.1.get_ref()(out)?, res)))
    }
    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(MapParser(self.0.box_clone(), self.1.clone()))
    }
}

pub trait ParserIterator<Slice: TokenSlice + ?Sized, P: Parser<Slice>>: Iterator<Item = P> + Sized {
    fn to_vec(self) -> impl Parser<Slice, Out = (Vec<<P::Out as Tuple>::Destructured>,)> {
        // There are too many boxes here for my liking
        self.fold(().map(|_| vec![]).box_clone(), |acc, p| {
            acc.and_raw(p)
                .map(|((mut v,), new)| {
                    v.push(new.destructure());
                    v
                })
                .box_clone()
        })
    }
}
impl<Slice: TokenSlice + ?Sized, P: Parser<Slice>, It: Iterator<Item = P>> ParserIterator<Slice, P> for It {}

box_clone!(ParseFn[TokenSlice: (?Sized), Out]: Fn(&TokenSlice) -> Option<(Out, &TokenSlice)>);
struct RawParser<Slice: TokenSlice + ?Sized, Out: Tuple + 'static>(Arc<dyn ParseFn<Slice, Out>>);

impl<Slice: TokenSlice + ?Sized, Out: Tuple + 'static> Clone for RawParser<Slice, Out> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Slice: TokenSlice + ?Sized, Out: Tuple + 'static> Parser<Slice> for RawParser<Slice, Out> {
    type Out = Out;

    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(self.clone())
    }

    fn parse_raw<'a>(
        &self,
        input: &'a Slice,
    ) -> Option<(Self::Out, &'a Slice)> {
        self.0(input)
    }
}

#[macro_export]
macro_rules! parser {
    // Brackets
    (&$ty:ty: ($($rest: tt)+)) => {
        parser!(&$ty: $($rest)+)
    };
    (&$ty:ty: [$($rest: tt)+]) => {
        parser![&$ty: $($rest)+]
    };
    // Predicates
    (&$ty:ty: $c:ident >> $pred:expr) => {
        $crate::parser::PredicateParser::new(|c: char|{
            let $c = c;
            $pred
        })
    };
    // Maybe
    (&$ty:ty: ! $($lhs: tt)*) => {
        $crate::parser::Parser::<$ty>::ignore(parser!(&$ty: $($lhs)*))
    };
    // Regex
    (&$ty:ty: r $regex:expr) => {
        $crate::parser::stringbased::p_regex($regex)
    };
    // And
    (&$ty:ty: $lhs: tt & $($rest: tt)+ ) => {
        $crate::parser::Parser::<$ty>::and(parser!(&$ty: $lhs),(parser!(&$ty: $($rest)+)))
    };
    // Or
    (&$ty:ty: $lhs: tt | $($rest: tt)+) => {
        $crate::parser::Parser::<$ty>::or(parser!(&$ty: $lhs),(parser!(&$ty: $($rest)+)))
    };
    // Map
    (&$ty:ty: $lhs: tt >> $e:expr) => {
        $crate::parser::Parser::<$ty>::map(parser!(&$ty: $lhs),$e)
    };
    // Recursive
    (&$ty:ty: rec($($r:tt)+)) => {
        *$crate::parser::recursive::p_recursive::<$ty,_,_>($($r)+)
    };
    // Repeat
    (&$ty:ty: $lhs: tt * $e:expr) => {
        $crate::parser::Parser::<$ty>::mul(parser!(&$ty: $lhs),$e)
    };
    // Maybe
    (&$ty:ty: $lhs: tt ?) => {
        $crate::parser::Parser::<$ty>::maybe(parser!(&$ty: $lhs))
    };
    // Other stuff
    (&$ty:ty: $lhs: expr) => {
        $lhs
    };
    [&$ty:ty: $($e:tt),*] => {
        parser!(&$ty: $($e)&*)
    };
    ($($tt:tt)*) => {
        parser!(&str: $($tt)*)
    };
    [$($tt:tt)*] => {
        parser!(&str: $($tt)*)
    };
}
