Convert ABNF like grammar to [rust-peg] build `rowan` lossless syntax tree

Using `rowan` library lossless syntax tree

[rust-peg]: https://github.com/kevinmehall/rust-peg

# How To Use

Add dependencies:

```
char-classes = "0.3.4"
peg = "0.8.5"
rowan = "0.15.15"
rowan-peg-utils = "0.2.3"
```

Edit your grammar declaration:

```abnf
exports [ pair-list ]

pair-list = whitespace *pair
pair    = key whitespace "=" whitespace val whitespace
key     = text
val     = number / text
number  = int ["." int]
int     = $(+<0-9>) @int ; Add `int` to the expected set when parsing fails
text    = $(+<a-zA-Z_->) @text
whitespace = $(*< \t\r\n>)
```

Generate grammar from your grammar declaration:

```sh
rowan-peg your-lang.abnf > src/grammar.rs
```

# Example

## Cargo build

If you want to use cargo to build grammar, you can use rowan-peg-macro

```rust
#[rowan_peg_macro::build(path = "grammar.abnf")]
mod grammar {}
```

---

A simple json parser [example-json](./examples/example-json)

```
cd examples/example-json
cargo run -- example.json
[examples/example-json/src/main.rs:23:5] &json = JsonText(
    JSON_TEXT@0..476
      WS@0..0 ""
      VALUE@0..476
        OBJECT@0..476
          BEGIN_OBJECT@0..6
            WS@0..0 ""
            L_CURLY@0..1 "{"
            WS@1..6 "\n    "
          MEMBER@6..136
            STRING@6..15
              QUOTATION_MARK@6..7 "\""
              CHAR@7..8
                UNESCAPED@7..8 "s"
              CHAR@8..9
                UNESCAPED@8..9 "t"
              CHAR@9..10
...
...
```

## Generate Grammar Declaration

```
cargo run -- examples/example-json/json.abnf
```


Grammar
===============================================================================

## Exports

Export or rename rules using `exports []`

Such as `exports [ foo ]`, export `foo` rule (public it)

Such as `exports [ bar = foo ]`, export `bar` rule (public and renamed into `bar`)


## Rule

Define a rule using `my-rule = "rule"`

When a rule belongs to the following situations, it is a token rule:

- rule body only exist a [slice](#slice) and optional expected

  such as: `a = $"x"`, `a = $("a" ["b"])`, `a = $("a" ["b"]) @a`


## Syntax Elements


### String

Hard matched content cannot be escaped, such as `"a"` `"+"`

- For identifiers, they will be renamed as `{name}_kw`, such as `"if"` -> `if_kw`
- For some punctuation marks, they will be renamed as names, such as `"!"` -> `bang`
- For those that cannot be renamed, an error will be reported.
  Please refer to the [Slice](#slice) for solution

> [!NOTE]
> Because it cannot be escaped, double quotes are represented by `<">`


### Matches

Match a some of characters or character ranges, can only be used in [slice](#slice)

Escaping equivalent rust string-literal

Syntax equivalent [char-classes](https://github.com/A4-Tacks/char-classes-rs)::any!() macro

Such as `<a-z>` matches lowercase alpha, `<^0-9>` matches non number


### Slice

Collect any parsing range as a token, must be used in token rule

Such as: `a-or-ab = $("a" ["b"])`, it can parsing `a`, `ab`


### Choice

Sequential choice, from PEG

Such as: `x = "a" "b" / "c"`, it can parser `ab`, `c`

When a branch fails, it will return to the beginning to try parsing the next branch

If no branch is successful, the this rule fails


### Repeat

Repeat parsing specified number of times

- `+x`:     Repeat x 1 to more times
- `*x`:     Repeat x 0 to more times
- `233*x`:  Repeat x 233 to more times
- `*6x`:    Repeat x 0 to 6 times
- `2*6x`:   Repeat x 2 to 6 times
- `6x`:     Repeat x 6 times
- `6(x y)`: Repeat x y 6 times


### Lookahead

Positive lookahead and Negative lookahead, from PEG

- Positive lookahead:
  Parsing, but success also returns to the beginning, which is a non consumption parsing
- Negative lookahead:
  Similar to positive lookahead, but fails upon success and succeeds upon failure


### Quiet

Map to [rust-peg] `quiet! {}`, will not add any expecteds from the target to the error message

Such as: `x = "x" ~" " "y"`, quiet space token expecteds message


### Comment

Use semicolons for comment, such as:

```abnf
x = "a" ; this is a comment
```
