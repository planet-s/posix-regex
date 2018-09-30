# posix-regex

A WIP library for parsing POSIX regular expressions. Only supports ASCII.
Created for use in relibc and does not require the std.

Currently only supports enhanced regex.

## Known TODOs

Regex compiler:
 - Unnamed groups
 - Alternative syntax for word boundaries: `[[:<:]]` and `[[:>:]]`

Matcher:
 - Groups (these are difficult because you can repeat them like any other token)
 - Word boundaries
