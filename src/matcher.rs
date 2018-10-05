//! The matcher: Can find substrings in a string that match any compiled regex

#[cfg(feature = "no_std")]
use std::prelude::*;

use compile::{Token, Range};
use ctype;
use std::borrow::Cow;
use std::rc::Rc;
use std::{fmt, mem};

/// A regex matcher, ready to match stuff
#[derive(Clone)]
pub struct PosixRegex<'a> {
    branches: Cow<'a, [Vec<(Token, Range)>]>,
    case_insensitive: bool,
    newline: bool,
    no_start: bool,
    no_end: bool
}
impl<'a> PosixRegex<'a> {
    /// Create a new matcher instance from the specified alternations. This
    /// should probably not be used and instead an instance should be obtained
    /// from `PosixRegexBuilder`, which also compiles a string into regex.
    pub fn new(branches: Cow<'a, [Vec<(Token, Range)>]>) -> Self {
        Self {
            branches,
            case_insensitive: false,
            newline: false,
            no_start: false,
            no_end: false
        }
    }
    /// Chainable function to enable/disable case insensitivity. Default: false.
    /// When enabled, single characters match both their upper and lowercase
    /// representations.
    pub fn case_insensitive(mut self, value: bool) -> Self {
        self.case_insensitive = value;
        self
    }
    /// Chainable function to enable/disable newline mode. Default: false.
    /// When enabled, ^ and $ match newlines as well as start/end.
    /// This behavior overrides both no_start and no_end.
    pub fn newline(mut self, value: bool) -> Self {
        self.newline = value;
        self
    }
    /// Chainable function to enable/disable no_start mode. Default: false.
    /// When enabled, ^ doesn't actually match the start of a string.
    pub fn no_start(mut self, value: bool) -> Self {
        self.no_start = value;
        self
    }
    /// Chainable function to enable/disable no_start mode. Default: false.
    /// When enabled, $ doesn't actually match the end of a string.
    pub fn no_end(mut self, value: bool) -> Self {
        self.no_end = value;
        self
    }
    /// Match the string starting at the current position. This does not find
    /// substrings.
    pub fn matches_exact(&self, input: &[u8]) -> Option<Vec<(usize, usize)>> {
        let mut groups = Vec::new();
        let mut matcher = PosixRegexMatcher {
            base: self,
            input,
            offset: 0,
            groups: &mut groups
        };
        let branches = self.branches.iter()
            .filter_map(|tokens| Branch::new(tokens))
            .collect();

        matcher.groups.push((matcher.offset, 0));
        if !matcher.matches_exact(branches) {
            return None;
        }
        groups[0].1 = matcher.offset;

        Some(groups)
    }
    /// Match any substrings in the string, but optionally no more than `max`
    pub fn matches(&self, input: &[u8], mut max: Option<usize>) -> Vec<Vec<(usize, usize)>> {
        let mut groups = Vec::new();
        let mut matcher = PosixRegexMatcher {
            base: self,
            input,
            offset: 0,
            groups: &mut groups
        };

        let tokens = vec![
            (Token::InternalStart, Range(0, None)),
            (Token::Group(self.branches.to_vec()), Range(1, Some(1)))
        ];
        let branches = vec![
            Branch::new(&tokens).unwrap()
        ];

        let mut matches = Vec::new();
        while max.map(|max| max > 0).unwrap_or(true) && matcher.matches_exact(branches.clone()) {
            matches.push(mem::replace(matcher.groups, Vec::new()));
            max = max.map(|max| max - 1);
        }
        matches
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Group {
    index: usize,
    variant: usize,

    start: usize,
    end: usize
}

#[derive(Clone)]
struct Branch<'a> {
    index: usize,
    repeated: u32,
    tokens: &'a [(Token, Range)],
    path: Box<[Group]>,
    prev: Vec<(Box<[(usize, usize)]>, (usize, usize))>,

    repeat_min: u32,
    repeat_max: Option<u32>,
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
            tokens: tokens,
            path: Box::new([]),
            prev: Vec::new(),

            repeat_min: 0,
            repeat_max: Some(0),
            next: None
        })
    }
    fn group(
        path: Box<[Group]>,
        prev: Vec<(Box<[(usize, usize)]>, (usize, usize))>,
        tokens: &'a [(Token, Range)],
        range: Range,
        next: Option<Branch<'a>>
    ) -> Option<Self> {
        if tokens.is_empty() {
            return None;
        }
        Some(Self {
            index: 0,
            repeated: 0,
            tokens,
            path,
            prev,
            repeat_min: range.0.saturating_sub(1),
            repeat_max: range.1.map(|i| i.saturating_sub(1)),
            next: next.map(Rc::new)
        })
    }
    fn parent_tokens(&self) -> &[(Token, Range)] {
        let mut tokens = self.tokens;

        let len = self.path.len();
        if len > 0 {
            for group in &self.path[..len-1] {
                match tokens[group.index] {
                    (Token::Group(ref inner), _) => tokens = &inner[group.variant],
                    _ => panic!("non-group index in path")
                }
            }
        }

        tokens
    }
    fn tokens(&self) -> &[(Token, Range)] {
        let mut tokens = self.parent_tokens();

        if let Some(group) = self.path.last() {
            match tokens[group.index] {
                (Token::Group(ref inner), _) => tokens = &inner[group.variant],
                _ => panic!("non-group index in path")
            }
        }

        tokens
    }
    fn get_token(&self) -> &(Token, Range) {
        &self.tokens()[self.index]
    }
    fn update_group_end(&mut self, offset: usize) {
        for group in &mut *self.path {
            group.end = offset;
        }
    }
    fn push_to_prev(&self, prev: &mut Vec<(Box<[(usize, usize)]>, (usize, usize))>) {
        for i in 0..self.path.len() {
            let key: Vec<_> = self.path[..=i].iter().map(|g| (g.index, g.variant)).collect();
            let key = key.into();
            let group = &self.path[i];
            let value = (group.start, group.end);

            if let Some(slot) = prev.iter_mut().find(|(key2, _)| key == *key2) {
                *slot = (key, value);
            } else {
                prev.push((key, value));
            }
        }
    }
    fn next_branch(&self) -> Option<Self> {
        if self.repeat_min > 0 {
            // Don't add the next branch until we've repeated this one enough
            return None;
        }
        if self.index + 1 >= self.tokens().len() {
            if let Some(ref next) = self.next {
                // Group is closing, migrate previous & current groups to next.
                let mut next = (**next).clone();

                for (key, value) in &self.prev {
                    if let Some(slot) = next.prev.iter_mut().find(|(key2, _)| key == key2) {
                        *slot = (key.clone(), value.clone());
                    } else {
                        next.prev.push((key.clone(), value.clone()));
                    }
                }
                self.push_to_prev(&mut next.prev);

                return Some(next)
            }
            return None;
        }
        Some(Self {
            index: self.index + 1,
            repeated: 0,
            ..self.clone()
        })
    }
    fn add_repeats(&self, branches: &mut Vec<Branch<'a>>, offset: usize) {
        if self.repeat_max.map(|max| max == 0).unwrap_or(false) {
            return;
        }

        let tokens = self.parent_tokens();
        let group = self.path.last().expect("add_repeats called on top level");
        match tokens[group.index] {
            (Token::Group(ref repeats), _) => {
                for alternative in 0..repeats.len() {
                    let mut path = self.path.clone();
                    let last = path.last_mut().unwrap();
                    last.start = offset;
                    last.variant = alternative;

                    branches.push(Self {
                        index: 0,
                        path,
                        repeated: 0,
                        repeat_min: self.repeat_min.saturating_sub(1),
                        repeat_max: self.repeat_max.map(|max| max - 1),
                        ..self.clone()
                    });
                }
            },
            _ => panic!("non-group index in path")
        }
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
}

struct PosixRegexMatcher<'a> {
    base: &'a PosixRegex<'a>,
    input: &'a [u8],
    offset: usize,
    groups: &'a mut Vec<(usize, usize)>
}
impl<'a> PosixRegexMatcher<'a> {
    fn expand<'b>(&mut self, branches: &mut [Branch<'b>]) -> Vec<Branch<'b>> {
        let mut insert = Vec::new();

        for branch in branches {
            branch.update_group_end(self.offset);

            let (ref token, range) = *branch.get_token();

            if let Token::Group(ref inner) = token {
                for alternation in 0..inner.len() {
                    let mut path = Vec::with_capacity(branch.path.len() + 1);
                    path.extend_from_slice(&branch.path);
                    path.push(Group {
                        index: branch.index,
                        variant: alternation,
                        start: self.offset,
                        end: 0
                    });

                    if let Some(branch) = Branch::group(
                        path.into(),
                        branch.prev.clone(),
                        branch.tokens,
                        range,
                        branch.next_branch()
                    ) {
                        insert.push(branch);
                    }
                }
            }
            if branch.repeated >= range.0 {
                // Push the next element as a new branch
                if let Some(next) = branch.next_branch() {
                    insert.push(next);
                }
                branch.add_repeats(&mut insert, self.offset);
            }
        }

        if !insert.is_empty() {
            // Resolve recursively
            let mut new = self.expand(&mut insert);
            insert.append(&mut new);
        }
        insert
    }

    fn matches_exact(&mut self, mut branches: Vec<Branch>) -> bool {
        // Whether or not any branch, at any point, got fully explored. This
        // means at least one path of the regex successfully completed!
        let mut succeeded = None;
        let mut prev = self.offset.checked_sub(1).and_then(|index| self.input.get(index).cloned());

        loop {
            let next = self.input.get(self.offset).cloned();

            let mut index = 0;
            let mut remove = 0;

            let mut insert = self.expand(&mut branches);
            branches.append(&mut insert);

            loop {
                if index >= branches.len() {
                    break;
                }
                if remove > 0 {
                    // Just like Rust's `retain` function, shift all elements I
                    // want to keep back and `truncate` when I'm done.
                    branches.swap(index, index-remove);
                }
                let branch = &mut branches[index-remove];
                index += 1;

                let (ref token, Range(_, mut max)) = *branch.get_token();
                let mut token = token;

                let mut accepts = true;

                // Step 1: Handle zero-width stuff like ^ and \<
                loop {
                    match token {
                        Token::End |
                        Token::Start |
                        Token::WordEnd |
                        Token::WordStart => {
                            accepts = accepts && match token {
                                Token::End =>
                                    (!self.base.no_end && next.is_none())
                                        || (self.base.newline && next == Some(b'\n')),
                                Token::Start =>
                                    (!self.base.no_start && self.offset == 0)
                                        || (self.base.newline && prev == Some(b'\n')),
                                Token::WordEnd => next.map(ctype::is_word_boundary).unwrap_or(true),
                                Token::WordStart => prev.map(ctype::is_word_boundary).unwrap_or(true),
                                _ => unreachable!()
                            };

                            // Skip ahead to the next token.
                            match branch.next_branch() {
                                Some(next) => *branch = next,
                                None => break
                            }
                            let (ref new_token, Range(_, new_max)) = *branch.get_token();
                            token = new_token;
                            max = new_max;
                        },
                        _ => break
                    }
                }

                // Step 2: Check if the token matches
                accepts = accepts && match *token {
                    Token::InternalStart => next.is_some(),

                    Token::Any => next.map(|c| !self.base.newline || c != b'\n').unwrap_or(false),
                    Token::Char(c) => if self.base.case_insensitive {
                        next.map(|c2| c & !32 == c2 & !32).unwrap_or(false)
                    } else {
                        next == Some(c)
                    },
                    Token::Group(_) => false, // <- handled separately
                    Token::OneOf { invert, ref list } => if let Some(next) = next {
                        (!invert || !self.base.newline || next != b'\n')
                        && list.iter().any(|c| c.matches(next, self.base.case_insensitive)) == !invert
                    } else { false },

                    // These will only get called if they are encountered at
                    // EOF (because next_branch returns None), for example
                    // "abc\>" or "^". Then we simply want to return true as to
                    // preserve the current `accepts` status.
                    Token::End |
                    Token::Start |
                    Token::WordEnd |
                    Token::WordStart => true
                };

                if !accepts || max.map(|max| branch.repeated >= max).unwrap_or(false) {
                    if branch.is_explored() {
                        succeeded = Some(branch.clone());
                    }
                    remove += 1;
                    continue;
                }

                branch.repeated += 1;
            }
            let end = branches.len() - remove;
            branches.truncate(end);

            if branches.is_empty() ||
                    // The internal start thing is lazy, not greedy:
                    (succeeded.is_some() && branches.iter().all(|t| t.get_token().0 == Token::InternalStart)) {
                if let Some(ref branch) = succeeded {
                    // Push the bounds of all successful groups
                    let mut prev = branch.prev.clone();
                    branch.push_to_prev(&mut prev);

                    for &(_, group) in &prev {
                        self.groups.push(group);
                    }
                }
                return succeeded.is_some();
            }

            if next.is_some() {
                self.offset += 1;
                prev = next;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "bench")]
    extern crate test;

    #[cfg(feature = "bench")]
    use self::test::Bencher;

    use super::*;
    use ::PosixRegexBuilder;

    fn compile(regex: &str) -> PosixRegex {
        PosixRegexBuilder::new(regex.as_bytes())
            .with_default_classes()
            .compile()
            .expect("error compiling regex")
    }
    fn matches(regex: &str, input: &str) -> Vec<Vec<(usize, usize)>> {
        compile(regex)
            .matches(input.as_bytes(), None)
    }
    fn matches_exact(regex: &str, input: &str) -> Option<Vec<(usize, usize)>> {
        compile(regex)
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
            Some(vec![(0, 3)])
        );
        assert_eq!(
            matches_exact(r"[[:alpha:]]\+", "abcde12345"),
            Some(vec![(0, 5)])
        );
        assert_eq!(
            matches_exact(r"a\(bc\)\+d", "abcbcd"),
            Some(vec![(0, 6), (3, 5)])
        );
        assert_eq!(
            matches_exact(r"hello\( \(world\|universe\) :D\)\?!", "hello world :D!"),
            Some(vec![(0, 15), (5, 14), (6, 11)])
        );
        assert_eq!(
            matches_exact(r"hello\( \(world\|universe\) :D\)\?", "hello world :D"),
            Some(vec![(0, 14), (5, 14), (6, 11)])
        );
        assert_eq!(
            matches_exact(r"\(\<hello\>\) world", "hello world"),
            Some(vec![(0, 11), (0, 5)])
        );
        assert_eq!(
            matches_exact(r".*d", "hid howd ared youd"),
            Some(vec![(0, 18)])
        );
        assert_eq!(
            matches_exact(r".*\(a\)", "bbbbba"),
            Some(vec![(0, 6), (5, 6)])
        );
        assert_eq!(
            matches_exact(r"\(a \(b\) \(c\)\) \(d\)", "a b c d"),
            Some(vec![(0, 7), (0, 5), (2, 3), (4, 5), (6, 7)])
        );
        assert_eq!(
            matches_exact(r"\(.\)*", "hello"),
            Some(vec![(0, 5), (4, 5)])
        );
        assert_eq!(
            matches("hi", "hello hi lol"),
            vec!(vec![(6, 8)])
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
    #[test]
    fn case_insensitive() {
        assert!(compile(r"abc[de]")
            .case_insensitive(true)
            .matches_exact(b"ABCD")
            .is_some());
        assert!(compile(r"abc[de]")
            .case_insensitive(true)
            .matches_exact(b"ABCF")
            .is_none());
    }
    #[test]
    fn newline() {
        assert!(compile(r"^hello$")
            .newline(true)
            .matches(b"hi\nhello\ngreetings", None)
            .len() == 1);
        assert!(compile(r"^hello$")
            .newline(true)
            .matches(b"hi\ngood day\ngreetings", None)
            .is_empty());
    }
    #[test]
    fn no_start_end() {
        assert!(compile(r"^hello")
            .no_start(true)
            .matches_exact(b"hello")
            .is_none());
        assert!(compile(r"hello$")
            .no_end(true)
            .matches_exact(b"hello")
            .is_none());
    }

    #[cfg(feature = "bench")]
    #[bench]
    fn speed_matches_exact(b: &mut Bencher) {
        b.iter(|| {
            assert!(matches_exact(r"\(\(a*\|b\|c\) test\|yee\)", "aaaaa test").is_some());
        })
    }
    #[cfg(feature = "bench")]
    #[bench]
    fn speed_matches(b: &mut Bencher) {
        b.iter(|| {
            assert!(matches(r"\(\(a*\|b\|c\) test\|yee\)", "oooo aaaaa test", None).len() == 1);
        })
    }
}
