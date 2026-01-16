use std::{fmt::Debug, ops::{
    Index, Range as RangeBase, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
}};

#[derive(Clone)]
pub struct Range {
    pub min: usize,
    pub max: usize,
    range_type: RangeType,
}

impl Debug for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.range_type {
            RangeType::Base => write!(f, "{}..{}", self.min, self.max.saturating_add(1)),
            RangeType::From => write!(f, "{}..", self.min),
            RangeType::Full => write!(f, ".."),
            RangeType::Inclusive => write!(f, "{}..={}", self.min, self.max),
            RangeType::To => write!(f, "..{}", self.max.saturating_add(1)),
            RangeType::ToInclusive => write!(f, "..={}", self.max),
        }
    }
}

#[derive(Clone)]
enum RangeType {
    Base,
    From,
    Full,
    Inclusive,
    To,
    ToInclusive,
}

impl Range {
    pub fn contains(&self, val: usize) -> bool {
        self.min <= val && val <= self.max
    }

    pub fn shift(&self, val: isize) -> Self {
        let min = self.min.saturating_add_signed(val);
        let max = self.max.saturating_add_signed(val);
        match (min, max) {
            (0, max) => (..=max).into(),
            (min, usize::MAX) => (min..).into(),
            (min, max) => (min..=max).into(),
        }
    }
}

#[allow(unused)]
pub trait RangeIndex:
    Index<RangeBase<usize>, Output = Self>
    + Index<RangeFrom<usize>, Output = Self>
    + Index<RangeFull, Output = Self>
    + Index<RangeTo<usize>, Output = Self>
    + Index<RangeInclusive<usize>, Output = Self>
    + Index<RangeToInclusive<usize>, Output = Self>
{
    fn range_index(&self, index: Range) -> &Self;
}

impl<
    T: Index<RangeBase<usize>, Output = Self>
        + Index<RangeFrom<usize>, Output = Self>
        + Index<RangeFull, Output = Self>
        + Index<RangeTo<usize>, Output = Self>
        + Index<RangeInclusive<usize>, Output = Self>
        + Index<RangeToInclusive<usize>, Output = Self>
        + ?Sized,
> RangeIndex for T
{
    fn range_index(&self, index: Range) -> &Self {
        match index.range_type {
            RangeType::Base | RangeType::Inclusive | RangeType::To | RangeType::ToInclusive => {
                &self[index.min..index.max + 1]
            }
            RangeType::From => &self[index.min..],
            RangeType::Full => self,
        }
    }
}

impl From<RangeBase<usize>> for Range {
    fn from(value: RangeBase<usize>) -> Self {
        Self {
            min: value.start,
            max: value.end - 1,
            range_type: RangeType::Base,
        }
    }
}
impl From<RangeInclusive<usize>> for Range {
    fn from(value: RangeInclusive<usize>) -> Self {
        Self {
            min: *value.start(),
            max: *value.end(),
            range_type: RangeType::Inclusive,
        }
    }
}
impl From<RangeFrom<usize>> for Range {
    fn from(value: RangeFrom<usize>) -> Self {
        Self {
            min: value.start,
            max: usize::MAX,
            range_type: RangeType::From,
        }
    }
}

impl From<RangeTo<usize>> for Range {
    fn from(value: RangeTo<usize>) -> Self {
        Self {
            min: 0,
            max: value.end - 1,
            range_type: RangeType::To,
        }
    }
}
impl From<RangeToInclusive<usize>> for Range {
    fn from(value: RangeToInclusive<usize>) -> Self {
        Self {
            min: 0,
            max: value.end,
            range_type: RangeType::ToInclusive,
        }
    }
}
impl From<RangeFull> for Range {
    fn from(_: RangeFull) -> Self {
        Self {
            min: 0,
            max: usize::MAX,
            range_type: RangeType::Full,
        }
    }
}
