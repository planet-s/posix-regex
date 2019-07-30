#[cfg(feature = "no_std")]
use std::prelude::*;

use std::cell::RefCell;

pub struct ImmutVecItem<T> {
    prev: Option<usize>,
    data: T,
}
pub struct ImmutVec<'a, T> {
    inner: &'a RefCell<Vec<ImmutVecItem<T>>>,
    id: Option<usize>,
}
impl<'a, T> Copy for ImmutVec<'a, T> {}
impl<'a, T> Clone for ImmutVec<'a, T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner,
            id: self.id,
        }
    }
}
impl<'a, T> ImmutVec<'a, T> {
    pub fn new(inner: &'a RefCell<Vec<ImmutVecItem<T>>>) -> Self {
        Self { inner, id: None }
    }
    #[must_use = "push does nothing to the original vector"]
    pub fn push(self, item: T) -> Self {
        let mut inner = self.inner.borrow_mut();
        let id = inner.len();
        inner.push(ImmutVecItem {
            prev: self.id,
            data: item,
        });
        Self {
            id: Some(id),
            ..self
        }
    }
}
impl<'a, T: Clone> ImmutVec<'a, T> {
    #[must_use = "pop does nothing to the original vector"]
    pub fn pop(self) -> (Self, Option<T>) {
        let inner = self.inner.borrow();
        let id = match self.id {
            None => return (self, None),
            Some(id) => id,
        };
        let item = &inner[id];
        (
            Self {
                id: item.prev,
                ..self
            },
            Some(item.data.clone()),
        )
    }
    pub fn iter_rev(self) -> ImmutVecIter<'a, T> {
        ImmutVecIter(self)
    }
}

pub struct ImmutVecIter<'a, T: Clone>(ImmutVec<'a, T>);
impl<'a, T: Clone> Iterator for ImmutVecIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let (new, item) = self.0.pop();
        self.0 = new;
        item
    }
}
