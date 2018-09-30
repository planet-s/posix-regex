//! The matcher: Can find substrings in a string that match any compiled regex

use compile::{Token, Range};
use std::fmt;

/// A regex matcher, ready to match stuff
#[derive(Clone)]
pub struct PosixRegex {
    pub(crate) search: Vec<Vec<(Token, Range)>>
}
impl PosixRegex {
    /// Match the string starting at the current position. This does not find
    /// substrings.
    pub fn matches_exact(self, input: &[u8]) -> Option<PosixRegexResult> {
        // let mut groups = Vec::new();
        let mut matcher = PosixRegexMatcher {
            input,
            offset: 0
            // groups: &mut groups
        };
        let start = matcher.offset;
        if !matcher.matches_exact(self.search.iter().filter_map(|tokens| Branch::new(tokens)).collect()) {
            return None;
        }
        let end = matcher.offset;

        Some(PosixRegexResult {
            start,
            end
        })
    }
}

struct Branch<'a> {
    index: usize,
    repeated: u32,
    tokens: &'a [(Token, Range)]
}
impl<'a> fmt::Debug for Branch<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_token())
    }
}
impl<'a> Branch<'a> {
    fn new(tokens: &'a [(Token, Range)]) -> Option<Self> {
        if tokens.is_empty() {
            return None;
        }
        Some(Self {
            index: 0,
            repeated: 0,
            tokens
        })
    }
    fn next_branch(&self) -> Option<Self> {
        if self.index + 1 >= self.tokens.len() {
            return None;
        }
        Some(Self {
            index: self.index + 1,
            repeated: 0,
            tokens: self.tokens
        })
    }
    fn get_token(&self) -> &(Token, Range) {
        &self.tokens[self.index]
    }
}

struct PosixRegexMatcher<'a> {
    input: &'a [u8],
    offset: usize
    // TODO: groups: &'a mut Vec<(usize, usize)>
}
impl<'a> PosixRegexMatcher<'a> {
    fn matches_exact(&mut self, mut branches: Vec<Branch>) -> bool {
        while let Some(&next) = self.input.get(self.offset) {
            println!();
            self.offset += 1;

            let mut index = 0;
            let mut remove = 0;

            for i in 0..branches.len() {
                let branch = &branches[i];
                let (ref token, Range(min, _)) = *branch.get_token();
                if branch.repeated >= min {
                    if let Some(next) = branch.next_branch() {
                        println!("{:?} ---[Cloned]--> {:?}", token, next.get_token());
                        branches.push(next);
                    }
                }
            }

            println!("Branches: {:?}", branches);
            loop {
                if index >= branches.len() {
                    break;
                }
                if remove > 0 {
                    branches.swap(index, index-remove);
                }
                let branch = &mut branches[index-remove];
                index += 1;

                branch.repeated += 1;
                let (ref token, Range(_, max)) = *branch.get_token();
                println!("Does {:?} match {:?}?", token, next as char);

                let accepts = match *token {
                    Token::Any => true,
                    Token::Char(c) => next == c,
                    Token::OneOf { invert, ref list } => list.iter().any(|c| c.matches(next)) == !invert,
                    _ => unimplemented!("TODO")
                };
                if !accepts || max.map(|max| branch.repeated > max).unwrap_or(false) {
                    println!("-> Delete!");
                    remove += 1;
                    continue;
                }
            }
            let end = branches.len() - remove;
            branches.truncate(end);

            if branches.is_empty() {
                return false;
            }
        }
        println!("Everything went successful so far, returning.");
        println!("Branches: {:?}", branches);

        for mut branch in branches {
            loop {
                let (ref token, Range(min, _)) = *branch.get_token();
                if branch.repeated < min {
                    println!("Token {:?} did not get explored fully ({}/{})", token, branch.repeated, min);
                    break;
                }

                if let Some(next) = branch.next_branch() {
                    branch = next;
                } else {
                    println!("Token {:?} *did* get explored fully", token);
                    return true;
                }
            }
        }
        false
    }
}

/// A single result, including start and end bounds
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PosixRegexResult {
    /// An offset in the original string to where the match started (inclusive)
    pub start: usize,
    /// An offset in the original string to where the match ended (exclusive)
    pub end: usize
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::PosixRegexBuilder;

    fn matches_exact(regex: &str, input: &str) -> Option<PosixRegexResult> {
        println!("----- TRYING TO MATCH {:?} AND {:?}", regex, input);
        PosixRegexBuilder::new(regex.as_bytes())
            .with_default_classes()
            .compile()
            .expect("error compiling regex")
            .matches_exact(input.as_bytes())
    }

    #[test]
    fn basic() {
        assert!(matches_exact("abc", "abc").is_some());
        assert!(matches_exact("abc", "bbc").is_none());
        assert!(matches_exact("abc", "acc").is_none());
        assert!(matches_exact("abc", "abd").is_none());
    }
    #[test]
    fn repetitions() {
        assert!(matches_exact("abc*", "ab").is_some());
        assert!(matches_exact("abc*", "abc").is_some());
        assert!(matches_exact("abc*", "abccc").is_some());

        assert!(matches_exact(r"a\{1,2\}b", "b").is_none());
        assert!(matches_exact(r"a\{1,2\}b", "ab").is_some());
        assert!(matches_exact(r"a\{1,2\}b", "aab").is_some());
        assert!(matches_exact(r"a\{1,2\}b", "aaab").is_none());
    }
    #[test]
    fn any() {
        assert!(matches_exact(".*", "").is_some());
        assert!(matches_exact(".*b", "b").is_some());
        assert!(matches_exact(".*b", "ab").is_some());
        assert!(matches_exact(".*b", "aaaaab").is_some());
        assert!(matches_exact(".*b", "HELLO WORLD").is_none());
        assert!(matches_exact(".*b", "HELLO WORLDb").is_some());
        assert!(matches_exact("H.*O WORLD", "HELLO WORLD").is_some());
        assert!(matches_exact("H.*ORLD", "HELLO WORLD").is_some());
    }
    #[test]
    fn brackets() {
        assert!(matches_exact("[abc]*d", "abcd").is_some());
        assert!(matches_exact("[0-9]*d", "1234d").is_some());
        assert!(matches_exact("[[:digit:]]*d", "1234d").is_some());
        assert!(matches_exact("[[:digit:]]*d", "abcd").is_none());
    }
    //#[test]
    //fn offsets() {
    //    assert_eq!(matches_exact("abc", "abcd"), Some(PosixRegexResult { start: 0, end: 3 }));
    //    assert_eq!(matches_exact(r"[[:alpha:]]\+", "abcde12345"), Some(PosixRegexResult { start: 0, end: 5 }));
    //}
    //#[test]
    //fn start_and_end() {
    //    assert!(matches_exact("^abc$", "abc").is_some());
    //    assert!(matches_exact("abc$", "abcd").is_none());
    //    assert!(matches_exact("^bcd", "abcd").is_none());
    //}
    //#[test]
    //fn groups() {
    //    assert!(matches_exact(r"\(a*\|b\|c\)d", "d").is_some());
    //    assert!(matches_exact(r"\(a*\|b\|c\)d", "aaaad").is_some());
    //    assert!(matches_exact(r"\(a*\|b\|c\)d", "bd").is_some());
    //    assert!(matches_exact(r"\(a*\|b\|c\)d", "bbbbbd").is_none());
    //}
    //#[test]
    //fn repeating_groups() {
    //    assert!(matches_exact(r"\(a\|b\|c\)*d", "d").is_some());
    //    assert!(matches_exact(r"\(a\|b\|c\)*d", "aaaad").is_some());
    //    assert!(matches_exact(r"\(a\|b\|c\)*d", "bbbbd").is_some());
    //    assert!(matches_exact(r"\(a\|b\|c\)*d", "aabbd").is_some());
    //}
}
