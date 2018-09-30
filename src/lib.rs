#![cfg_attr(feature = "no_std", no_std)]
#![cfg_attr(feature = "no_std", feature(alloc))]

#[cfg(feature = "no_std")]
mod std {
    extern crate alloc;

    pub use alloc::*;
    pub use core::*;

    pub mod prelude {
        pub use super::alloc::string::String;
        pub use super::alloc::vec::Vec;
    }
}
#[cfg(feature = "no_std")]
use std::prelude::*;

pub mod compile;
pub mod ctype;
pub mod matcher;

pub use compile::PosixRegexBuilder;
pub use matcher::PosixRegex;
