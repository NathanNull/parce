use super::{Parser, ValidToken};
use crate::{parser::TokenSlice, tuple::Tuple};
use std::{
    marker::PhantomData,
    sync::{Arc, Weak},
};

pub struct Recursive<Slice: TokenSlice + ?Sized, P: Parser<Slice>> {
    inner: Weak<P>,
    _p: PhantomData<Slice>,
}

impl<Slice: TokenSlice + 'static, Out: Tuple + 'static, P: Parser<Slice, Out = Out> + 'static>
    Parser<Slice> for Recursive<Slice, P>
{
    type Out = Out;

    fn box_clone(&self) -> Arc<dyn Parser<Slice, Out = Self::Out>> {
        Arc::new(Self {
            inner: self.inner.clone(),
            _p: PhantomData,
        })
    }

    fn parse_raw<'a>(&self, input: &'a Slice) -> Option<(Out, &'a Slice)> {
        self.inner
            .upgrade()
            .expect("Inner value dropped, Houston we have a problem")
            .parse_raw(input)
    }
}

pub fn p_recursive<Slice: TokenSlice + ?Sized, Out: Tuple + 'static, P: Parser<Slice, Out = Out>>(
    f: impl FnOnce(Recursive<Slice, P>) -> P,
) -> Arc<P> {
    Arc::<P>::new_cyclic(|arc| {
        let rec = Recursive {
            inner: arc.clone(),
            _p: PhantomData,
        };
        f(rec)
    })
}
