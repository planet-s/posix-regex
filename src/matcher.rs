//! The matcher: Can find substrings in a string that match any compiled regex

use compile::{Token, Range};

/// A regex matcher, ready to match stuff
pub struct PosixRegex {
    pub(crate) search: Vec<(Token, Range)>
}
impl PosixRegex {
    /// Match the string starting at the current position. This does not find
    /// substrings.
    pub fn matches_exact(&self, input: &[u8]) -> Option<PosixRegexResult> {
        // let mut groups = Vec::new();
        let mut matcher = PosixRegexMatcher {
            input,
            state: PosixRegexMatcherState {
                offset: 0
            },
            // groups: &mut groups
        };
        let start = matcher.state.offset;
        if !matcher.matches_exact(&self.search) {
            return None;
        }
        let end = matcher.state.offset;

        Some(PosixRegexResult {
            start,
            end
        })
    }
}

// This is a struct because it might need to keep more stuff later.
// TODO: Maybe remove this.
#[derive(Clone, Copy)]
struct PosixRegexMatcherState {
    offset: usize
}
struct PosixRegexMatcher<'a> {
    input: &'a [u8],
    state: PosixRegexMatcherState,
    // TODO: groups: &'a mut Vec<(usize, usize)>
}
impl<'a> PosixRegexMatcher<'a> {
    fn next(&mut self) -> Option<u8> {
        self.input.get(self.state.offset)
            .map(|&c| { self.state.offset += 1; c })
    }
    fn peek(&self) -> Option<u8> {
        self.input.get(self.state.offset).cloned()
    }
    fn match_token(&mut self, token: &Token) -> bool {
        //println!("Matching {:?} with {:?}", token, &self.input[self.state.offset..]);
        match *token {
            Token::Any => self.next().is_some(),
            Token::Char(c) => self.peek() == Some(c) && self.next().is_some(),
            Token::End => self.next().is_none(),
            Token::Group(_) => unimplemented!("TODO: Groups"),
            Token::OneOf { invert, ref list } => if let Some(c) = self.next() {
                list.iter().any(|collation| collation.matches(c)) == !invert
            } else {
                false
            },
            Token::Start => self.state.offset == 0,
            Token::WordEnd |
            Token::WordStart => unimplemented!("TODO: Word boundaries")
        }
    }
    fn matches_exact(&mut self, mut tokens: &[(Token, Range)]) -> bool {
        loop {
            //println!("Matching {:?} and {:?}", tokens, &self.input[self.state.offset..]);

            if tokens.is_empty() {
                return true;
            }

            let (ref token, Range(start, end)) = *tokens.first().unwrap();
            tokens = &tokens[1..];

            let mut repetition_branches = Vec::new();

            // Make sure it matches at least <start> times:
            for _ in 1..=start {
                //println!("Must match: {:?}", token);
                if !self.match_token(token) {
                    return false;
                }
            }

            //println!("Matches enough times, at least");

            // Try all times, greedily (so in reverse order):
            let mut max = end.map(|end| end - start);

            let original = self.state;

            while max.map(|max| max > 0).unwrap_or(true) && self.match_token(token) {
                //println!("Repetitions left: {:?}", max);
                repetition_branches.push(self.state);
                max = max.map(|max| max - 1);
            }

            for branch in repetition_branches.into_iter().rev() {
                self.state = branch;
                //println!("- Branch: {:?}", &self.input[self.state.offset..]);
                if self.matches_exact(tokens) {
                    return true;
                }
            }

            self.state = original;
        }
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
        //println!("----- TRYING TO MATCH {:?} AND {:?}", regex, input);
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
    }
    #[test]
    fn brackets() {
        assert!(matches_exact("[abc]*d", "abcd").is_some());
        assert!(matches_exact("[0-9]*d", "1234d").is_some());
        assert!(matches_exact("[[:digit:]]*d", "1234d").is_some());
        assert!(matches_exact("[[:digit:]]*d", "abcd").is_none());
    }
    #[test]
    fn offsets() {
        assert_eq!(matches_exact("abc", "abcd"), Some(PosixRegexResult { start: 0, end: 3 }));
        assert_eq!(matches_exact(r"[[:alpha:]]\+", "abcde12345"), Some(PosixRegexResult { start: 0, end: 5 }));
    }
    #[test]
    fn start_and_end() {
        assert!(matches_exact("^abc$", "abc").is_some());
        assert!(matches_exact("abc$", "abcd").is_none());
        assert!(matches_exact("^bcd", "abcd").is_none());
    }
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
