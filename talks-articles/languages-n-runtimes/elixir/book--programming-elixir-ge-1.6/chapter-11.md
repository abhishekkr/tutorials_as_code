
## Chapter.11 Strings and Binaries

> code examples at [chapter-11.exs](./chapter-11.exs)

### String Literals

* 2 kinds: single & double quoted; differ a lot in internal representation bit many similarities

> * can hold UTF-8 encoding characters
>
> * may contain escape sequences: `\a BEL (0x07)`, `\e ESC (0x1b)`, `\r CR (0x0d)`, `\v VT (0x0b)`, `\b BS (0x08)`, `\d DEL (0x7f)`, `\f FF (0x0c)`, `\n NL (0x0a)`, `\s SP (0x20)`, `\t TAB (0x09)`, `\uhhh 1-6 hex digits`, `\xhh 2 hex digits`
>
> * allow `#{..}` interpolation and escape special characters
>
> * support heredocs, as at `UseStr.heredoc/0`

#### Heredocs

* `".."` allow multiline but indentation in new lines get used; heredoc notation `"""..."""` fixes multiline string leading space issue


#### Sigils

* `~[A-Za-z]DELIMS-style` literals are Sigils that let you crunch out different type data in short-form notation as in `UseStr.sigil`

> `[..]`, `{..}`, `(..)`, `|..|`, `<..>`, `/../`, `".."`, `'..'` can be used as elimiters

* `~W[]` & `~w[]` sigilas take an optional type specifier `a`, `c` or `s` determining returned list is of atoms, char-lists or strings; delimiter can be any non-word character

* nesting isn't checked, so `~s{a{b}` is `a{b`


### The Name "strings"

* here `".."` are strings, `'..'` are character lists


### Single-Quoted Strings: Lists of Characters

* represented as integer list, each value mapped to string codepoint

* notation `?a` returns integer code for character `a`; useful when extracting pattern as in `UseStr.parse_digits/1`


### Binaries

* binary type represents bit sequence, literal looks like `<< term, ... >>`

* `b = <<1, 2, 3>>` would have `byte_size(b) == 3` & `bit_size(b) == 24`

* can specify bit size modifiers as `b2 = <<1::size(2), 1::size(3)>>` would have `byte_size(b2) == 1` & `bit_size(b2) == 5`

* can store integers, floats & other binaries in binaries as in `UseBinaries.try/0`


### Double Quoted Strings are Binaries

* double quoted strings are stored as consecutive sequence of bytes in UTF-8 encoding; so size of string isn't necessarily string length

> `UseStringModule.try/0` helps showcase multiple String module useful functionalities
>
> `StringNBinaries5.center/1` implement String And Binaries Exercise 5


### Binaries and Pattern Matching

* if in doubt, specify type of each field; available are `binary, bits, bitstring, bytes, float, integer, utf8, utf16, utf32`

> * can add qualifiers like `size(n)` in bits
>
> * `signed` or `unsigned` for int fields
>
> * endianness of `big`, `little` or `native`
>
> use hyphens to separate several attributes

```
<< length::unsigned-integer-size(12), flags::bitstring-size(4) >> = data
```

> `StringNBinaries6n7` module implement String And Binaries Exercise 6 and 7

### Familiar Yet Strange

* pattern matching works smooth with strings being binary

---
