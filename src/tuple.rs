#![allow(dead_code)]

pub trait Tuple: Clone {
    type Destructured: Clone;
    fn from_destructured(input: Self::Destructured) -> Self;
    fn destructure(self) -> Self::Destructured;

    type Prepend<P: Clone>: Tuple;
    fn prepend<P: Clone>(self, item: P) -> Self::Prepend<P>;
    type Concat<T: Tuple>: Tuple;
    fn concat<T: Tuple>(self, rhs: T) -> Self::Concat<T>;

    type Pop;
    type Popped: Tuple;
    fn pop(self) -> (Self::Popped, Self::Pop);
}

impl<A: Clone> Tuple for (A,) {
    type Destructured = A;
    fn from_destructured(input: Self::Destructured) -> Self {
        (input,)
    }
    fn destructure(self) -> Self::Destructured {
        #[allow(non_snake_case)]
        let (A,) = self;
        #[allow(clippy::double_parens)]
        (A)
    }

    type Prepend<P: Clone> = (P, A);
    fn prepend<P: Clone>(self, item: P) -> Self::Prepend<P> {
        (item, self.0)
    }

    type Concat<T: Tuple> = T::Prepend<A>;
    fn concat<T: Tuple>(self, rhs: T) -> Self::Concat<T> {
        rhs.prepend(self.0)
    }

    type Pop = A;
    type Popped = ();

    fn pop(self) -> (Self::Popped, Self::Pop) {
        ((), self.0)
    }
}

pub enum Never {}
impl Tuple for () {
    type Destructured = Self;
    type Prepend<P: Clone> = (P,);

    fn from_destructured(input: Self::Destructured) -> Self {
        input
    }

    fn destructure(self) -> Self::Destructured {}
    fn prepend<P: Clone>(self, item: P) -> Self::Prepend<P> {
        (item,)
    }

    type Concat<T: Tuple> = T;
    fn concat<T: Tuple>(self, rhs: T) -> Self::Concat<T> {
        rhs
    }

    type Pop = Never;

    type Popped = Self;

    fn pop(self) -> (Self::Popped, Self::Pop) {
        panic!("Can't pop from ()")
    }
}

macro_rules! tuples {
    ($(
        ($($t:ident-$i:tt),*:$last:ident-$li:tt)
        +P>
        $((P,$($ts:ident-$tsi:tt),*))?
        $(^Last(P,$($tl:ident),*:$slast:ident))?
    );+$(;)?) => {
    $(
        impl<$($t: Clone,)*$last: Clone> Tuple for ($($t,)*$last,) {
            #[allow(unused_parens)]
            type Destructured = ($($t,)*$last);
            fn from_destructured(input: Self::Destructured) -> Self {
                (
                    $(input.$i,)*
                    input.$li,
                )
            }
            fn destructure(self) -> Self::Destructured {
                ($(self.$i,)*self.$li)
            }

            type Prepend<P: Clone> = <Self as TmpPrepend>::Prepend<P>;
            fn prepend<P: Clone>(self, item: P) -> Self::Prepend<P> {
                <Self as TmpPrepend>::prepend(self, item)
            }

            type Concat<T: Tuple> =
                <Self::Popped as Tuple>::Concat<T::Prepend<Self::Pop>>;
            fn concat<T: Tuple>(self, rhs: T) -> Self::Concat<T> {
                let (rest, last) = self.pop();
                rest.concat(rhs.prepend(last))
            }

            type Pop = $last;
            type Popped = ($($t,)*);
            fn pop(self) -> (Self::Popped, Self::Pop) {
                #[allow(clippy::double_parens)]
                (($(self.$i,)*), self.$li)
            }
        }

        $(
            impl<$($ts),*> TmpPrepend for ($($ts,)*) {
                type Prepend<P> = (P, $($ts,)*);
                fn prepend<P>(self, item: P) -> Self::Prepend<P> {
                    (item, $(self.$tsi),*)
                }
            }
        )?
        $(
            impl<$($tl: Clone,)*$slast: Clone> TmpPrepend for ($($tl,)*$slast,) {
                type Prepend<P> = ((P, $($tl),*), $slast);
                fn prepend<P>(self, item: P) -> Self::Prepend<P> {
                    #[allow(non_snake_case)]
                    let ($($tl,)*$slast) = self.destructure();
                    ((item, $($tl),*), $slast)
                }
            }
        )?
    )+
    };
}

// This is insane
tuples!(
    (A-0:B-1) +P> (P,A-0,B-1);
    (A-0,B-1:C-2) +P> (P,A-0,B-1,C-2);
    (A-0,B-1,C-2:D-3) +P> (P,A-0,B-1,C-2,D-3);
    (A-0,B-1,C-2,D-3:E-4) +P> (P,A-0,B-1,C-2,D-3,E-4);
    (A-0,B-1,C-2,D-3,E-4:F-5) +P> (P,A-0,B-1,C-2,D-3,E-4,F-5);
    (A-0,B-1,C-2,D-3,E-4,F-5:G-6) +P> (P,A-0,B-1,C-2,D-3,E-4,F-5,G-6);
    (A-0,B-1,C-2,D-3,E-4,F-5,G-6:H-7) +P> ^Last(P,A,B,C,D,E,F,G:H);
);

pub trait TmpPrepend {
    type Prepend<P>;
    fn prepend<P>(self, item: P) -> Self::Prepend<P>;
}
