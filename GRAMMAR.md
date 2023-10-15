## Grammar

```bnf
nodes ::= newline*
        | newline* node EOF
        | newline* node ";" nodes
        | newline* node newline nodes
node ::= type-annot? name prop-or-arg-or-disabled-children* children?
prop-or-arg-or-disabled-children ::= "/-"? prop-or-arg | "/-" children
prop-or-arg ::= name "=" value | value
value ::= type-annot? (string | raw-string | integer | float | "true" | "false" | "null")
children ::= "/-"? "{" nodes "}"
type-annot ::= "(" name ")"
name ::= ident | string | raw-string
```

## Tokens

```bnf
newline ::= CRLF | CR | LF | NEL | FF | LS | PS

single-line-comment ::= "//" [^ newline]* (newline | EOF)
multi-line-comment ::= "/*" ([^ "*/"]* | multi-line-comment) "*/"

line-cont ::= "\\" single-line-comment
            | "\\" newline

identchar ::= [^ "\\" "/" "(" ")" "{" "}" "<" ">" ";" "[" "]" "=" "," '"' 0x0..0x20 ]
identstart ::= identchar - "0".."9"
ident ::= "-" identstart identchar*
        | "-"
        | (identstart - "-") identchar*

hex-digit ::= "0".."9" | "a".."f" | "A".."F"
dec-digit ::= "0".."9"
oct-digit ::= "0".."7"
bin-digit ::= "0" | "1"
decimal-nat ::= dec-digit (dec-digit | "_")*
sign ::= "-" | "+"
exponent ::= ("e" | "E") sign? decimal-nat
dec-int ::= sign? decimal-nat
dec-float ::= sign? decimal-nat ("." decimal-nat exponent? | exponent)
hex-int ::= sign? "0x" hex-digit (hex-digit | "_")*
oct-int ::= sign? "0o" oct-digit (oct-digit | "_")*
bin-int ::= sign? "0b" bin-digit (bin-digit | "_")*
integer ::= dec-int | hex-int | oct-int | bin-int
float ::= dec-float

raw-string ::= "r" <any number of #> '"' <any-char>* '"' <the same number of #>
string ::= '"' string-character* '"'
string-character ::= <any-regular-char>
                   | "\n" | "\r" | "\t" | "\\" | '\"' | "\b" | "\f"
                   | "\u{" hex-digit{1,6} "}"
```

`ws` (whitespace) is used as a token delimiter, defined as the union of the unicode
characters described [here][], `single-line-comment`, `multi-line-comment`,
and `line-cont`.

[here]: https://github.com/kdl-org/kdl/blob/1.0.0/SPEC.md#whitespace

A keyword (`true`, `false`, `null`) cannot be an `ident`.

Note that `EOF` is not allowed after the `\` line continuation without a single-line comment.

Whitespace around `=` and after the `)` type annotation is forbiddened by a semantic check.

---

_Note:_ This grammar is not exactly identical to https://github.com/kdl-org/kdl/blob/main/SPEC.md.

Among some of the differences, a disabled (`/-`) children block is allowed
before arguments/properties in `ocaml-kdl`. In general, `ocaml-kdl` is more
whitespace-insensitive. For example, the KDL spec forbids `node"str1""str2"`,
while `ocaml-kdl` allows it. Everything valid by the KDL spec should also be
valid in `ocaml-kdl`, but the opposite is not always true.

The original grammar is made mostly for recursive descent parsers without a
separate lexical analyzer.
