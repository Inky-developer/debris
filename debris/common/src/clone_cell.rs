use std::cell::RefCell;

/// Cell which supports `get()` by cloning the value out.
#[derive(Debug, Default, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct CloneCell<T> {
    inner: RefCell<T>,
}

impl<T> CloneCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: RefCell::new(value),
        }
    }

    /// Sets the value of this cell to `value` and returns the old value
    pub fn set(&self, value: T) -> T {
        std::mem::replace(&mut *self.inner.borrow_mut(), value)
    }
}

impl<T> CloneCell<T>
where
    T: Clone,
{
    /// Clone the value contained in this cell and returns it
    pub fn get(&self) -> T {
        self.inner.borrow().clone()
    }
}

impl<T> From<T> for CloneCell<T> {
    fn from(value: T) -> Self {
        Self {
            inner: RefCell::new(value),
        }
    }
}
