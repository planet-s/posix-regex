//! The matcher: Can find substrings in a string that match any compiled regex

#[cfg(feature = "no_std")]
use std::prelude::*;

use compile::{Token, Range};
use std::borrow::{Borrow, Cow};
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

/// A regex matcher, ready to match stuff
#[derive(Clone)]
pub struct PosixRegex {
    pub(crate) search: Vec<Vec<(Token, Range)>>
}
impl PosixRegex {
    /// Match the string starting at the current position. This does not find
    /// substrings.
    pub fn matches_exact(self, input: &[u8]) -> Option<PosixRegexResult> {
        let mut groups = Vec::new();
        let mut matcher = PosixRegexMatcher {
            input,
            offset: 0,
            groups: &mut groups
        };
        let start = matcher.offset;
        if !matcher.matches_exact(self.search.iter().filter_map(|tokens| Branch::new(tokens)).collect()) {
            return None;
        }
        let end = matcher.offset;

        Some(PosixRegexResult {
            start,
            end,
            groups
        })
    }
}

enum CowRc<'a, B: ToOwned + ?Sized> {
    Borrowed(&'a B),
    Owned(Rc<<B as ToOwned>::Owned>)
}
impl<'a, B: ToOwned + ?Sized> Clone for CowRc<'a, B> {
    fn clone(&self) -> Self {
        match self {
            CowRc::Borrowed(inner) => CowRc::Borrowed(inner),
            CowRc::Owned(inner) => CowRc::Owned(Rc::clone(inner)),
        }
    }
}
impl<'a, B: ToOwned + ?Sized> Deref for CowRc<'a, B> {
    type Target = B;

    fn deref(&self) -> &Self::Target {
        match self {
            CowRc::Borrowed(borrow) => borrow,
            CowRc::Owned(owned) => (**owned).borrow()
        }
    }
}

#[derive(Clone)]
struct Branch<'a> {
    index: usize,
    repeated: u32,
    tokens: CowRc<'a, [(Token, Range)]>,

    group_ids: Rc<[usize]>,
    repeat_min: u32,
    repeat_max: Option<u32>,
    repeats: Option<Rc<Vec<Rc<Vec<(Token, Range)>>>>>,
    next: Option<Rc<Branch<'a>>>
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
            tokens: CowRc::Borrowed(tokens),
            group_ids: Vec::new().into(),
            repeat_min: 0,
            repeat_max: Some(0),
            repeats: None,
            next: None
        })
    }
    fn group(
        group_ids: Rc<[usize]>,
        tokens: Rc<Vec<(Token, Range)>>,
        range: Range,
        repeats: Rc<Vec<Rc<Vec<(Token, Range)>>>>,
        next: Option<Branch<'a>>
    ) -> Option<Self> {
        if tokens.is_empty() {
            return None;
        }
        Some(Self {
            index: 0,
            repeated: 0,
            tokens: CowRc::Owned(tokens),
            group_ids,
            repeat_min: range.0.saturating_sub(1),
            repeat_max: range.1.map(|i| i.saturating_sub(1)),
            repeats: Some(repeats),
            next: next.map(Rc::new)
        })
    }
    fn get_token(&self) -> &(Token, Range) {
        &self.tokens[self.index]
    }
    /// Returns if this node is "explored" enough times,
    /// meaning it has repeated as many times as it want to and has nowhere to go next.
    fn is_explored(&self) -> bool {
        let mut branch = Cow::Borrowed(self);

        loop {
            if branch.repeat_min > 0 {
                // Group did not repeat enough times!
                return false;
            }

            let (_, Range(min, _)) = *branch.get_token();
            if branch.repeated < min {
                return false;
            }
            match branch.next_branch() {
                Some(next) => branch = Cow::Owned(next),
                None => break
            }
        }
        true
    }
    fn next_branch(&self) -> Option<Self> {
        if self.repeat_min > 0 {
            // Don't add the next branch until we've repeated this one enough
            return None;
        }
        if self.index + 1 >= self.tokens.len() {
            return self.next.as_ref().map(|inner| (**inner).clone());
        }
        Some(Self {
            index: self.index + 1,
            repeated: 0,
            ..self.clone()
        })
    }
    fn add_repeats(&self, branches: &mut Vec<Branch<'a>>) {
        if self.repeat_max.map(|max| max == 0).unwrap_or(false) {
            //println!("Don't add repeats, cuz repeat_max = {:?}", self.repeat_max);
            return;
        }
        if let Some(ref repeats) = self.repeats {
            for branch in &**repeats {
                //println!("REPEAT!");
                branches.push(Self {
                    index: 0,
                    repeated: 0,
                    repeat_min: self.repeat_min.saturating_sub(1),
                    repeat_max: self.repeat_max.map(|max| max - 1),
                    tokens: CowRc::Owned(Rc::clone(branch)),
                    ..self.clone()
                });
            }
        }
    }
}

struct PosixRegexMatcher<'a> {
    input: &'a [u8],
    offset: usize,
    groups: &'a mut Vec<(usize, usize)>
}
impl<'a> PosixRegexMatcher<'a> {
    fn expand<'b>(&mut self, branches: &[Branch<'b>]) -> Vec<Branch<'b>> {
        let mut insert = Vec::new();

        for branch in branches {
            let (ref token, range) = *branch.get_token();

            if let Token::Group(ref inner) = token {
                let group_id = self.groups.len();
                self.groups.push((self.offset, 0));

                let repeats = Rc::new(inner.iter().cloned().map(Rc::new).collect());

                let mut ids = Vec::with_capacity(branch.group_ids.len() + 1);
                ids.extend(&*branch.group_ids);
                ids.push(group_id);
                let ids = ids.into();
                for alternation in &*repeats {
                    if let Some(branch) = Branch::group(
                        Rc::clone(&ids),
                        Rc::clone(alternation),
                        range,
                        Rc::clone(&repeats),
                        branch.next_branch()
                    ) {
                        //println!("{:?} ---[G Cloned]--> {:?}", token, branch.get_token());
                        insert.push(branch);
                    }
                }
            }
            if branch.repeated >= range.0 {
                if let Some(next) = branch.next_branch() {
                    //println!("{:?} ---[Cloned]--> {:?}", token, next.get_token());
                    insert.push(next);
                }
                branch.add_repeats(&mut insert);
            }
        }

        if !insert.is_empty() {
            let mut new = self.expand(&insert);
            insert.append(&mut new);
        }
        insert
    }

    fn matches_exact(&mut self, mut branches: Vec<Branch>) -> bool {
        // Whether or not any branch, at any point, got fully explored. This
        // means at least one path of the regex successfully completed!
        let mut succeeded = false;
        let mut prev = None;

        loop {
            let next = self.input.get(self.offset).cloned();
            //println!();

            let mut index = 0;
            let mut remove = 0;

            let mut insert = self.expand(&branches);
            branches.append(&mut insert);

            //println!("Branches: {:?}", branches);
            loop {
                if index >= branches.len() {
                    break;
                }
                if remove > 0 {
                    branches.swap(index, index-remove);
                }
                let branch = &mut branches[index-remove];
                index += 1;

                let (ref token, Range(_, mut max)) = *branch.get_token();
                let mut token = token;
                //println!("Does {:?} match {:?}?", token, next.map(|c| c as char));

                let mut accepts = true;

                loop {
                    match token {
                        Token::Start |
                        Token::WordEnd |
                        Token::WordStart => {
                            // Should be cheap to clone since we already make sure
                            // it's a type that doesn't hold any data.
                            let original = token.clone();

                            // Skip ahead to the next token.
                            match branch.next_branch() {
                                Some(next) => *branch = next,
                                None => break
                            }
                            let (ref new_token, Range(_, new_max)) = *branch.get_token();
                            token = new_token;
                            max = new_max;

                            //println!("Or {:?}", token);

                            accepts = match original {
                                Token::Start => self.offset == 0,
                                Token::WordEnd => next.map(::ctype::is_word_boundary).unwrap_or(true),
                                Token::WordStart => prev.map(::ctype::is_word_boundary).unwrap_or(true),
                                _ => unreachable!()
                            };
                        },
                        _ => break
                    }
                }

                //println!("Accepts before: {:?}", accepts);

                accepts = accepts && match *token {
                    Token::Any => next.is_some(),
                    Token::Char(c) => next == Some(c),
                    Token::End => next.is_none(),
                    Token::Group(_) => false, // <- handled separately
                    Token::OneOf { invert, ref list } => if let Some(next) = next {
                        list.iter().any(|c| c.matches(next)) == !invert
                    } else { false },

                    // These will only get called if they are encountered at
                    // EOF, for example "abc\>" or "^". Then we simply want to
                    // return true as to preserve the current `accepts` status.
                    Token::Start |
                    Token::WordEnd |
                    Token::WordStart => true
                };

                //println!("Accepts after: {:?}", accepts);

                if !accepts || max.map(|max| branch.repeated >= max).unwrap_or(false) {
                    succeeded = succeeded || branch.is_explored();
                    //println!("-> Deleted! Succeeded: {}", succeeded);
                    for &id in &*branch.group_ids {
                        self.groups[id].1 = self.offset;
                    }
                    remove += 1;
                    continue;
                }
                branch.repeated += 1;
            }
            let end = branches.len() - remove;
            branches.truncate(end);

            if branches.is_empty() {
                return succeeded;
            }

            if next.is_some() {
                self.offset += 1;
                prev = next;
            }
        }
    }
}

/// A single result, including start and end bounds
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PosixRegexResult {
    /// An offset in the input string to where the match started (inclusive)
    pub start: usize,
    /// An offset in the input string to where the match ended (exclusive)
    pub end: usize,
    /// Start/end offsets in the input to where a group matched
    pub groups: Vec<(usize, usize)>
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
        assert!(matches_exact("H.*ORLD", "HELLO WORLD").is_some());
    }
    #[test]
    fn brackets() {
        assert!(matches_exact("[abc]*d", "abcd").is_some());
        assert!(matches_exact("[0-9]*d", "1234d").is_some());
        assert!(matches_exact("[[:digit:]]*d", "1234d").is_some());
        assert!(matches_exact("[[:digit:]]*d", "abcd").is_none());
    }
    #[test]
    fn alternations() {
        assert!(matches_exact(r"abc\|bcd", "abc").is_some());
        assert!(matches_exact(r"abc\|bcd", "bcd").is_some());
        assert!(matches_exact(r"abc\|bcd", "cde").is_none());
        assert!(matches_exact(r"[A-Z]\+\|yee", "").is_none());
        assert!(matches_exact(r"[A-Z]\+\|yee", "HELLO").is_some());
        assert!(matches_exact(r"[A-Z]\+\|yee", "yee").is_some());
        assert!(matches_exact(r"[A-Z]\+\|yee", "hello").is_none());
    }
    #[test]
    fn offsets() {
        assert_eq!(
            matches_exact("abc", "abcd"),
            Some(PosixRegexResult { start: 0, end: 3, groups: vec![] })
        );
        assert_eq!(
            matches_exact(r"[[:alpha:]]\+", "abcde12345"),
            Some(PosixRegexResult { start: 0, end: 5, groups: vec![] })
        );
        assert_eq!(
            matches_exact(r"a\(bc\)\+d", "abcbcd"),
            Some(PosixRegexResult { start: 0, end: 6, groups: vec![(1, 5)] })
        );
        assert_eq!(
            matches_exact(r"hello\( \(world\|universe\) :D\)\?!", "hello world :D!"),
            Some(PosixRegexResult { start: 0, end: 15, groups: vec![(5, 14), (6, 11)] })
        );
        assert_eq!(
            matches_exact(r"hello\( \(world\|universe\) :D\)\?", "hello world :D"),
            Some(PosixRegexResult { start: 0, end: 14, groups: vec![(5, 14), (6, 11)] })
        );
    }
    #[test]
    fn start_and_end() {
        assert!(matches_exact("^abc$", "abc").is_some());
        assert!(matches_exact("^bcd", "bcde").is_some());
        assert!(matches_exact("^bcd", "abcd").is_none());
        assert!(matches_exact("abc$", "abc").is_some());
        assert!(matches_exact("abc$", "abcd").is_none());

        assert!(matches_exact(r".*\(^\|a\)c", "c").is_some());
        assert!(matches_exact(r".*\(^\|a\)c", "ac").is_some());
        assert!(matches_exact(r".*\(^\|a\)c", "bc").is_none());

        // Tests if ^ can be repeated without issues
        assert!(matches_exact(".*^^a", "helloabc").is_none());
        assert!(matches_exact(".*^^a", "abc").is_some());
    }
    #[test]
    fn word_boundaries() {
        assert!(matches_exact(r"hello\>.world", "hello world").is_some());
        assert!(matches_exact(r"hello\>.world", "hello!world").is_some());
        assert!(matches_exact(r"hello\>.world", "hellooworld").is_none());

        assert!(matches_exact(r"hello.\<world", "hello world").is_some());
        assert!(matches_exact(r"hello.\<world", "hello!world").is_some());
        assert!(matches_exact(r"hello.\<world", "hellooworld").is_none());

        assert!(matches_exact(r".*\<hello\>", "hihello").is_none());
        assert!(matches_exact(r".*\<hello\>", "hi_hello").is_none());
        assert!(matches_exact(r".*\<hello\>", "hi hello").is_some());
    }
    #[test]
    fn groups() {
        assert!(matches_exact(r"\(a*\|b\|c\)d", "d").is_some());
        assert!(matches_exact(r"\(a*\|b\|c\)d", "aaaad").is_some());
        assert!(matches_exact(r"\(a*\|b\|c\)d", "bd").is_some());
        assert!(matches_exact(r"\(a*\|b\|c\)d", "bbbbbd").is_none());
    }
    #[test]
    fn repeating_groups() {
        assert!(matches_exact(r"\(a\|b\|c\)*d", "d").is_some());
        assert!(matches_exact(r"\(a\|b\|c\)*d", "aaaad").is_some());
        assert!(matches_exact(r"\(a\|b\|c\)*d", "bbbbd").is_some());
        assert!(matches_exact(r"\(a\|b\|c\)*d", "aabbd").is_some());

        assert!(matches_exact(r"\(a\|b\|c\)\{1,2\}d", "d").is_none());
        assert!(matches_exact(r"\(a\|b\|c\)\{1,2\}d", "ad").is_some());
        assert!(matches_exact(r"\(a\|b\|c\)\{1,2\}d", "abd").is_some());
        assert!(matches_exact(r"\(a\|b\|c\)\{1,2\}d", "abcd").is_none());

        assert!(matches_exact(r"\(a\|b\|c\)\{4\}d", "ababad").is_none());
        assert!(matches_exact(r"\(a\|b\|c\)\{4\}d", "ababd").is_some());
        assert!(matches_exact(r"\(a\|b\|c\)\{4\}d", "abad").is_none());
    }
}
