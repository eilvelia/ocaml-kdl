let test str =
  match Kdl.of_string str with
  | Ok result -> result
    |> Kdl.sexp_of_t
    |> Sexplib0.Sexp.to_string_hum
    |> print_endline
  | Error err ->
    print_endline ("Error: " ^ Kdl.show_error err)

let%expect_test "empty document" =
  test {||};
  [%expect {| () |}]

let%expect_test "a node without entities" =
  test {|node|};
  [%expect {| (node) |}]

let%expect_test "a node with one argument" =
  test {|node 0 |};
  [%expect {| (node (number-int 0)) |}]

let%expect_test "all literals as arguments" =
  test {|node 1 true false "str" 2 r"raw string" null|};
  [%expect {|
    (node (number-int 1) (bool true) (bool false) (string str) (number-int 2)
     (string "raw string") (null)) |}]

let%expect_test "a node with properties" =
  test {|node key=true foo="bar"|};
  [%expect {| (node (prop key (bool true)) (prop foo (string bar))) |}]

let%expect_test "a node with mixed properties and arguments" =
  test {|node true key=true false foo="bar" null|};
  [%expect {|
    (node (bool true) (bool false) (null) (prop key (bool true))
     (prop foo (string bar))) |}]

let%expect_test "- is a valid node name" =
  test {|- true|};
  [%expect {| (- (bool true)) |}]

let%expect_test "special symbols are allowed in an identifier" =
  test {|foo123~!@#$%^&*.:'|?+ "weeee"|};
  [%expect {| (foo123~!@#$%^&*.:'|?+ (string weeee)) |}]

let%expect_test "a node terminated with a semicolon" =
  test {|node 2;|};
  [%expect {| (node (number-int 2)) |}]

let%expect_test "several nodes separated by semicolon" =
  test {| node1 true arg1=1; node2 false arg2=2; node3; node4 |};
  [%expect {|
    ((node1 (bool true) (prop arg1 (number-int 1)))
     (node2 (bool false) (prop arg2 (number-int 2))) (node3) (node4)) |}]

let%expect_test "several nodes separated by semicolon and newline" =
  test {|
    node1 true
    node2 false
    node3;
    node4; node5
  |};
  [%expect {| ((node1 (bool true)) (node2 (bool false)) (node3) (node4) (node5)) |}]

let%expect_test "a node with a children block" =
  test {|node key="val" { inner; inner2 true; }|};
  [%expect {| (node (prop key (string val)) (children (inner) (inner2 (bool true)))) |}]

let%expect_test "a node inside the children block cannot end with }" =
  test {|- { inner }|};
  [%expect {| Error: :1:11-1:12: A node should be terminated with a semicolon or newline |}]

let%expect_test "nodes delimited by newline inside a children block" =
  test {|
    contents {
      section "First section" {
        paragraph "This is the first paragraph"
        paragraph "This is the second paragraph"
      }
    }|};
  [%expect {|
    (contents
     (children
      (section (string "First section")
       (children (paragraph (string "This is the first paragraph"))
        (paragraph (string "This is the second paragraph")))))) |}]

let%expect_test "redefinition of a children block" =
  test {|- 1 { }    { }|};
  [%expect {| Error: :1:12-1:15: A children block must be defined only once |}]

let%expect_test "props/args after a children block" =
  test {|- 1 { node; } 2|};
  [%expect {| Error: :1:5-1:14: A children block must be the last element of a node |}]

let%expect_test "a type-annotated node" =
  test {|(author)node null|};
  [%expect {| (node (type author) (null)) |}]

let%expect_test "type-annotated values" =
  test {|node (u8)250 (date)"2021-02-03" filter=(regex)r"$\d+"|};
  [%expect {|
    (node (number-int 250 u8) (string 2021-02-03 date)
     (prop filter (string "$\\d+" regex))) |}]

let%expect_test "cannot use 'true' as a node name" =
  test {|truecorrect; true|};
  [%expect {| Error: :1:14-1:18: 'true' is not a valid node name |}]

let%expect_test "cannot use 'false' as a node name" =
  test {|falsecorrect; false|};
  [%expect {| Error: :1:15-1:20: 'false' is not a valid node name |}]

let%expect_test "cannot use 'null' as a node name" =
  test {|null key="value"|};
  [%expect {| Error: :1:1-1:5: 'null' is not a valid node name |}]

let%expect_test "cannot use a number as a node name" =
  test {|-3|};
  [%expect {| Error: :1:1-1:3: A number is not a valid node name |}]

let%expect_test "cannot use a keyword as a type annotation" =
  test {|(true)node|};
  [%expect {| Error: :1:2-1:6: 'true' is not a valid type annotation name |}]

let%expect_test "cannot use a keyword as a property name" =
  (* TODO: A better error perhaps? (true is parsed as a value, so = is invalid.) *)
  test {|- true=false|};
  [%expect {| Error: :1:7-1:8: Expected a value |}]

let%expect_test "string literals as a node name" =
  test {|"node \t [1]" null; "true" true|};
  [%expect {| (("node \t [1]" (null)) (true (bool true))) |}]

let%expect_test "raw string literals as a node name" =
  test {|r#"no\nde"#; r"false" false|};
  [%expect {| (("no\\nde") (false (bool false))) |}]

let%expect_test "(raw) string literals as a type annotation" =
  test {|(r"no de")node ("true")true|};
  [%expect {| (node (type "no de") (bool true true)) |}]

let%expect_test "(raw) string literals as a property name" =
  test {|- "key\n"="value" r##"key\t"##=true|};
  [%expect {| (- (prop "key\n" (string value)) (prop "key\\t" (bool true))) |}]

let%expect_test "identifiers cannot start with r#" =
  test {|- r#=0|};
  test {|- r#a=1|};
  test {|- r#<=1|};
  [%expect {|
    Error: :1:3-1:5: An identifier cannot start with r#
    Error: :1:3-1:6: An identifier cannot start with r#
    Error: :1:3-1:5: An identifier cannot start with r# |}]

let%expect_test "identifiers cannot contain special characters (<, etc.)" =
  test {|- a<=1|};
  [%expect {| Error: :1:4-1:5: Illegal character '<' |}]

let%expect_test "-- as an identifier" =
  test {|- --=0|};
  [%expect {| (- (prop -- (number-int 0))) |}]

let%expect_test "single-line comments" =
  test {|node // comment // commment
        // comment node|};
  [%expect {| (node) |}]

let%expect_test "single-line comments can be empty" =
  test {|node //|};
  [%expect {| (node) |}]

let%expect_test "multiline comments" =
  test {|node /* * node " */ true /* { **/ { /**/ inner; }|};
  [%expect {| (node (bool true) (children (inner))) |}]

let%expect_test "multiline comments can be nested" =
  test {|node /* comment /* also a comment */ comment */ 0|};
  [%expect {| (node (number-int 0)) |}]

let%expect_test "multiline comments can wrap lines" =
  test {|node true /*
    comment
    */ false|};
  [%expect {| (node (bool true) (bool false)) |}]

let%expect_test "unterminated multiline comment" =
  test {|- /* comment "/*" */ |};
  [%expect {| Error: :1:22-1:22: Unterminated comment |}]

let%expect_test "/- can disable a node as a whole" =
  test {|
    node1 1
    /-mynode "foo" key=1 {
      a
      b
      c
    }
    node2 2
    |};
  [%expect {| ((node1 (number-int 1)) (node2 (number-int 2))) |}]

let%expect_test "/- can disable arguments and properties" =
  test {|mynode /-"commented" "not commented" /-key="value"|};
  [%expect {| (mynode (string "not commented")) |}]

let%expect_test "/- can disable a children block" =
  test {|
    mynode /-"commented" "not commented" /-{
      a
      b
    }|};
  [%expect {| (mynode (string "not commented")) |}]

let%expect_test "/- can disable a children block before the args or props" =
  (* Note: this is non-standard for now *)
  test {|
    mynode /-"commented" "not commented" /-{
      a
      b
    } key="value" { inner1; }|};
  [%expect {|
    (mynode (string "not commented") (prop key (string value))
     (children (inner1))) |}]

let%expect_test "/- cannot be empty" =
  test {|node /- /-"val" {}|};
  [%expect {| Error: :1:9-1:11: Expected a value |}]

let%expect_test "arguments without a separating whitespace" =
  (* Non-standard *)
  test {|node"str1""str2"|};
  [%expect {| (node (string str1) (string str2)) |}]

let%expect_test "using tabs instead of 0x20 spaces" =
  test "\tnode\tkey1=true\tkey2=false";
  [%expect {| (node (prop key1 (bool true)) (prop key2 (bool false))) |}]

let%expect_test "whitespace should not be allowed before =" =
  test {|- key  =  "value"|};
  [%expect {| Error: :1:6-1:8: Whitespace before '=' is not allowed |}]

let%expect_test "whitespace should not be allowed after =" =
  test {|- key= "value"|};
  test {|- key= (string)"value"|};
  [%expect {|
    Error: :1:7-1:8: Whitespace after '=' is not allowed
    Error: :1:7-1:8: Whitespace after '=' is not allowed |}]

let%expect_test "whitespace should not be allowed after a node type annotation" =
  test {|(author) node true|};
  [%expect {| Error: :1:9-1:10: Whitespace after a type annotation is not allowed |}]

let%expect_test "whitespace should not be allowed after a value type annotation" =
  test {|node prop=(year) "2022"          (u8) 58|};
  [%expect {| Error: :1:17-1:18: Whitespace after a type annotation is not allowed |}]

let%expect_test "redefinition of a property" =
  test {|- a=0 prop=1 prop=2 prop=3 b=4 |};
  [%expect {|
    (- (prop a (number-int 0)) (prop prop (number-int 3))
     (prop b (number-int 4))) |}]

let%test_module "numbers" = (module struct
  let%expect_test "an integer" =
    test {|node 51235|};
    [%expect {| (node (number-int 51235)) |}]

  let%expect_test "a negative integer" =
    test {|node -30; - -3|};
    [%expect {| ((node (number-int -30)) (- (number-int -3))) |}]

  let%expect_test "a large integer" =
    test {|node 12351823951203598125123512041234935|};
    [%expect {| (node (number-int-raw 12351823951203598125123512041234935)) |}]

  let%expect_test "underscore separators" =
    test {|node 93_33__43_|};
    [%expect {| (node (number-int 933343)) |}]

  let%expect_test "underscore at the beginning is an identifier" =
    test {|_11|};
    [%expect {| (_11) |}]

  let%expect_test "float literals" =
    test {|node 3.14; node 3___14_.0_01_; node -5.2|};
    [%expect {|
      ((node (number-decimal 3.14)) (node (number-decimal 3___14_.0_01_))
       (node (number-decimal -5.2))) |}]

  let%expect_test "the underscore just after the decimal separator is not allowed" =
    test {|node 3._1|};
    [%expect {| Error: :1:6-1:10: Invalid number literal 3._1 |}]

  let%expect_test "the E notation" =
    test {|a 32e7; b 3e+3; c 2e-3; d 3.43e+4; e -3E2|};
    [%expect {|
      ((a (number-decimal 32e7)) (b (number-decimal 3e+3))
       (c (number-decimal 2e-3)) (d (number-decimal 3.43e+4))
       (e (number-decimal -3E2))) |}]

  let%expect_test "hexadecimal prefix" =
    test {|- 0x0523f15; - -0xead_f00d; - 0xEAD_F00D|};
    [%expect {|
      ((- (number-int 5390101)) (- (number-int -246280205))
       (- (number-int 246280205))) |}]

  let%expect_test "octal prefix" =
    test {|- 0o713_; - -0o755|};
    [%expect {| ((- (number-int 459)) (- (number-int -493))) |}]

  let%expect_test "8 / 9 is an invalid octal digit" =
    test {|- 0o798|};
    [%expect {| Error: :1:3-1:8: Invalid number literal 0o798 |}]

  let%expect_test "binary prefix" =
    test {|- 0b0101_0101; - -0b11110|};
    [%expect {| ((- (number-int 85)) (- (number-int -30))) |}]

  let%expect_test "the prefix cannot be used in a float literal" =
    test {|- 0x2.3|};
    [%expect {| Error: :1:3-1:8: Invalid number literal 0x2.3 |}]

  let%expect_test "a number cannot be followed by identifier characters" =
    test {| - 42a |};
    [%expect {| Error: :1:4-1:7: Invalid number literal 42a |}]

  let%expect_test "a hex number cannot be followed by G-Z letters" =
    test {|- 0xfeag|};
    [%expect {| Error: :1:3-1:9: Invalid number literal 0xfeag |}]
end)

let%test_module "strings" = (module struct
  let%expect_test "a normal string" =
    test {|- "hello world" "hi"|};
    [%expect {| (- (string "hello world") (string hi)) |}]

  let%expect_test "strings can be multiline" =
    test {|- "multiline

      string
      "|};
    [%expect {|
      (- (string  "multiline\
                 \n\
                 \n      string\
                 \n      ")) |}]

  let%expect_test "raw strings" =
    test {|- r"this is a raw string"|};
    [%expect {| (- (string "this is a raw string")) |}]

  let%expect_test "raw strings also can be multiline" =
    test {|- r"multiline
               raw string
      "|};
    [%expect {|
      (- (string  "multiline\
                 \n               raw string\
                 \n      ")) |}]

  let%expect_test "raw strings can be enclosed by arbitrary number of #" =
    test {|- r####"raw " string"####   r##"raw "# string"##|};
    [%expect {| (- (string "raw \" string") (string "raw \"# string")) |}]

  let%expect_test "raw strings can contain a \" followed by a greater number of #" =
    (* TODO: Does this follow the spec? For example, it is incorrect in Rust. *)
    test {|- r#"hello "## world"#   r"hello "# world"|};
    [%expect {| (- (string "hello \"## world") (string "hello \"# world")) |}]

  let%expect_test "unterminated raw string" =
    test {|- r##"raw string"#|};
    [%expect {| Error: :1:19-1:19: Unterminated raw string |}]

  let%expect_test "closing the raw string with a greater number of #" =
    (* Another possible choice is parsing the last # as an identifier *)
    test {|- r#"raw string"##|};
    [%expect {| Error: :1:19-1:19: Unterminated raw string |}]

  let%expect_test "escapes" =
    test {|- "\"\\\b\f\n\r\t"|};
    [%expect {|
      (- (string  "\"\\\b\012\
                 \n\r\t")) |}]

  let%expect_test "escapes should not be interpreted in a raw string" =
    test {|- r#"\"\\\/\b\f\n\r\t"#|};
    [%expect {| (- (string "\\\"\\\\\\/\\b\\f\\n\\r\\t")) |}]

  let%expect_test "\\u{...} unicode escapes" =
    test {|- "\u{61} _ \u{0061} _ \u{00205F}"|};
    [%expect {| (- (string "a _ a _ \226\129\159")) |}]

  let%expect_test "\\u{...} cannot contain more than 6 hex digits" =
    test {|- "\u{1234567}"|};
    [%expect {| Error: :1:4-1:15: Invalid unicode code point, cannot contain more than 6 hex digits |}]

  let%expect_test "\\u{...} should not accept a > 0x10FFFF code point" =
    test {|- "\u{11FBBF} _"|};
    [%expect {| Error: :1:4-1:14: Invalid unicode code point, cannot be greater than 10FFFF |}]

  let%expect_test "empty \\u{} without the code point" =
    test {|- "\u{}"|};
    [%expect {| (- (string "\\u{}")) |}]

  let%expect_test "whitespace escape should remove space from the string" =
    test {|- "foo \   bar"|};
    [%expect {| (- (string "foo bar")) |}]

  let%expect_test "whitespace escape should also remove a newline" =
    test {|- "foo \
              bar"|};
    [%expect {| (- (string "foo bar")) |}]

  let%expect_test "whitespace escape should remove multiple newlines" =
    (* This behavior is the same as in Rust
       https://github.com/rust-lang/reference/pull/1042 *)
    test {|- "foo\


            bar"|};
    [%expect {| (- (string foobar)) |}]

  let%expect_test "whitespace escape preceding space and then newline" =
    test "- \"foo \\ \n bar\"";
    [%expect {| (- (string "foo bar")) |}]
end)

let%test_module "line continuations" = (module struct
  let%expect_test "basic line continuation" =
    test {|node key1="v1" \
      key2="v2"|};
    [%expect {| (node (prop key1 (string v1)) (prop key2 (string v2))) |}]

  let%expect_test "line continuation should allow empty lines" =
    test {|
      node true \
        \
        \
        false \
    |};
    [%expect {| (node (bool true) (bool false)) |}]

  let%expect_test "single-line comments should be allowed after \\" =
    test {|node k1=1 \ // comment
                k2=2
           node2|};
    [%expect {| ((node (prop k1 (number-int 1)) (prop k2 (number-int 2))) (node2)) |}]

  let%expect_test "multiline comments should be allowed after \\" =
    test {|
      node true \ /* comment

        comment */
        false |};
    [%expect {| (node (bool true) (bool false)) |}]

  let%expect_test "EOF is allowed after \\ followed by a comment" =
    test {|node \ // comment|};
    [%expect {| (node) |}]

  let%expect_test "EOF is disallowed after \\ without comments" =
    test {|node \|};
    [%expect {| Error: :1:7-1:7: Unexpected EOF after the '\' line continuation |}]

  let%expect_test "the escline_comment_node.kdl test case" =
    (* TODO: What to do about it?
       See https://github.com/kdl-org/kdl/issues/223 *)
    test {|node1
      \// hey
      node2|};
    [%expect {| ((node1) (node2)) |}]
end)

let%test_module "unicode" = (module struct
  let%expect_test "emoji inside a string" =
    test {|smile "😁"|};
    [%expect {| (smile (string "\240\159\152\129")) |}]

  let%expect_test "unicode characters in identifiers and strings" =
    (* The space below is Ideographic Space U+3000 *)
    test {|ノード　お名前="☜(ﾟヮﾟ☜)"|};
    [%expect {|
      ("\227\131\142\227\131\188\227\131\137"
       (prop "\227\129\138\229\144\141\229\137\141"
        (string "\226\152\156(\239\190\159\227\131\174\239\190\159\226\152\156)"))) |}]

  let%expect_test "LS U+2028 and PS U+2029 can be used as a node separator" =
    (* When written literally, this sometimes confuses the lsp server *)
    test "node1\u{2028}node2\u{2029}node3";
    [%expect {| ((node1) (node2) (node3)) |}]

  let%expect_test "NEL U+0085 and FF U+000C correctly update locations" =
    test "node1\u{85}\u{85}\u{0C}\u{0C}\u{85}\u{85}😁 __error__\n";
    [%expect {| Error: :7:3-7:12: Expected a value, got an identifier |}]

  let%expect_test "BOM is ignored" =
    test "\u{FEFF}node";
    [%expect {| (node) |}]
end)

let%expect_test "the type annotations example" =
  test {|
    numbers (u8)10 (i32)20 myfloat=(f32)1.5 {
      strings (uuid)"123e4567-e89b-12d3-a456-426614174000" (date)"2021-02-03" filter=(regex)r"$\d+"
      (author)person name="Alex"
    }
  |};
  [%expect {|
    (numbers (number-int 10 u8) (number-int 20 i32)
     (prop myfloat (number-decimal 1.5 f32))
     (children
      (strings (string 123e4567-e89b-12d3-a456-426614174000 uuid)
       (string 2021-02-03 date) (prop filter (string "$\\d+" regex)))
      (person (type author) (prop name (string Alex))))) |}]

let%expect_test "the ci example" =
  test {|
    name "CI"

    on "push" "pull_request"

    env {
      RUSTFLAGS "-Dwarnings"
    }

    jobs {
      fmt_and_docs "Check fmt & build docs" {
        runs-on "ubuntu-latest"
        steps {
          step uses="actions/checkout@v1"
          step "Install Rust" uses="actions-rs/toolchain@v1" {
            profile "minimal"
            toolchain "stable"
            components "rustfmt"
            override true
          }
          step "rustfmt" run="cargo fmt --all -- --check"
          step "docs" run="cargo doc --no-deps"
        }
      }
      build_and_test "Build & Test" {
        runs-on "${{ matrix.os }}"
        strategy {
          matrix {
            rust "1.46.0" "stable"
            os "ubuntu-latest" "macOS-latest" "windows-latest"
          }
        }

        steps {
          step uses="actions/checkout@v1"
          step "Install Rust" uses="actions-rs/toolchain@v1" {
            profile "minimal"
            toolchain "${{ matrix.rust }}"
            components "clippy"
            override true
          }
          step "Clippy" run="cargo clippy --all -- -D warnings"
          step "Run tests" run="cargo test --all --verbose"
        }
      }
    }
  |};
  [%expect {|
    ((name (string CI)) (on (string push) (string pull_request))
     (env (children (RUSTFLAGS (string -Dwarnings))))
     (jobs
      (children
       (fmt_and_docs (string "Check fmt & build docs")
        (children (runs-on (string ubuntu-latest))
         (steps
          (children (step (prop uses (string actions/checkout@v1)))
           (step (string "Install Rust")
            (prop uses (string actions-rs/toolchain@v1))
            (children (profile (string minimal)) (toolchain (string stable))
             (components (string rustfmt)) (override (bool true))))
           (step (string rustfmt)
            (prop run (string "cargo fmt --all -- --check")))
           (step (string docs) (prop run (string "cargo doc --no-deps")))))))
       (build_and_test (string "Build & Test")
        (children (runs-on (string "${{ matrix.os }}"))
         (strategy
          (children
           (matrix
            (children (rust (string 1.46.0) (string stable))
             (os (string ubuntu-latest) (string macOS-latest)
              (string windows-latest))))))
         (steps
          (children (step (prop uses (string actions/checkout@v1)))
           (step (string "Install Rust")
            (prop uses (string actions-rs/toolchain@v1))
            (children (profile (string minimal))
             (toolchain (string "${{ matrix.rust }}"))
             (components (string clippy)) (override (bool true))))
           (step (string Clippy)
            (prop run (string "cargo clippy --all -- -D warnings")))
           (step (string "Run tests")
            (prop run (string "cargo test --all --verbose")))))))))) |}]
