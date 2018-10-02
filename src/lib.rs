#![cfg_attr(feature = "no_std", no_std)]
#![cfg_attr(feature = "no_std", feature(alloc))]
#![feature(nll)]

#[cfg(feature = "no_std")]
#[cfg_attr(test, macro_use)]
extern crate alloc;

#[cfg(feature = "no_std")]
mod std {
    pub use alloc::{borrow, rc};
    pub use core::*;

    pub mod collections {
        pub use alloc::collections::*;
        pub use alloc::collections::BTreeMap as HashMap;
    }
    pub mod prelude {
        pub use alloc::borrow::ToOwned;
        pub use alloc::boxed::Box;
        pub use alloc::string::String;
        pub use alloc::vec::Vec;
    }
}

pub mod compile;
pub mod ctype;
pub mod matcher;

pub use compile::PosixRegexBuilder;
pub use matcher::PosixRegex;
