# posix-regex

A WIP library for parsing POSIX regular expressions. Only supports ASCII.
Created for use in relibc and does not require the std.

Currently only supports enhanced regex.

## Known TODOs

Regex compiler:
 - Alternative syntax for word boundaries: `[[:<:]]` and `[[:>:]]`
 - Short forms of character classes, like `\d`
 - Unnamed groups
