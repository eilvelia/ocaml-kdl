let test str =
  match Kdl.of_string str with
  | Ok result -> Kdl.pp Format.std_formatter result
  | Error err -> Format.printf "Error: %a\n" Kdl.pp_error err

let test_compact str =
  match Kdl.of_string str with
  | Ok result -> Kdl.pp_compact Format.std_formatter result
  | Error err -> Format.printf "Error: %a\n" Kdl.pp_error err

let%expect_test "basic pretty-printing" =
  test {|node 1e3/**/#true key="prop" #null { inner1 2; inner2 1; }|};
  [%expect {|
    node 1e3 #true #null key=prop {
      inner1 2
      inner2 1
    }
    |}]

let%expect_test "should correctly quote identifiers" =
  test {|("true")"h\ni" #"key\n""#="value" "foo bar"=#null ("\t")0 ">"="<" \
         not'quoted=0 "-5"|};
  [%expect {| ("true")"h\ni" ("\t")0 "-5" "key\\n\""=value "foo bar"=#null >=< not'quoted=0 |}]

let%expect_test "should correctly quote identifiers starting with a digit and dash-digit" =
  test {|- "-9"=-9 "92"=92|};
  [%expect {| - "-9"=-9 "92"=92 |}]

let%expect_test "should correctly quote empty identifiers" =
  test {|"" ""=""|};
  [%expect {| "" ""="" |}]

let%expect_test "should correctly quote keyword identifiers" =
  test {|- "true"="true" "#true"="#true" "nan"="#nan"|};
  [%expect {| - "true"="true" "#true"="#true" "nan"="#nan" |}]

let%expect_test "should correctly handle escapes in strings" =
  test {|- "\n" "\"" #" " "#
        """
        foo
        bar
        """|};
  [%expect {|
    - "\n" "\"" " \" "
    "foo\nbar"
    |}]

let%expect_test "should escape disallowed characters" =
  test {|node "foo\u{FEFF}bar\u{2067}"|};
  [%expect {| node "foo\u{FEFF}bar\u{2067}" |}]

let%expect_test "a very long node" =
  test {|long_node 1 2 3 4 #"key"#=value #true #null #false 3.2 3e+4 \
         90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 {
          inner_node key=(u16)0xff 0xffffffffffffffffffffffffffff \
            0x101 0x102 0x103 0x104 0x105 0x106 0x107 0x108 0x109 0x110 0x111
          inner_node_2 #null #-inf #inf (foo-bar)#nan
         }|};
  [%expect {|
    long_node 1 2 3 4 #true #null #false 3.2 3e+4 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 key=value {
      inner_node 0xffffffffffffffffffffffffffff 257 258 259 260 261 262 263 264 265 272 273 key=(u16)255
      inner_node_2 #null #-inf #inf (foo-bar)#nan
    }
    |}]

let%expect_test "redefinition of a property" =
  test {|- prop=1 prop=2 prop=3|};
  [%expect {| - prop=3 |}]

let%expect_test "a complex example" =
  test {|
    name CI

    on push pull_request

    env {
      RUSTFLAGS -Dwarnings
    }

    jobs {
      fmt_and_docs "Check fmt & build docs" {
        runs-on ubuntu-latest
        steps {
          step uses="actions/checkout@v1"
          step "Install Rust" uses="actions-rs/toolchain@v1" {
            profile minimal
            toolchain stable
            components rustfmt
            override #true
          }
          step rustfmt { run cargo fmt --all -- --check }
          step docs { run cargo doc --no-deps }
        }
      }
      build_and_test "Build & Test" {
        runs-on "${{ matrix.os }}"
        strategy {
          matrix {
            rust "1.46.0" stable
            os ubuntu-latest macOS-latest windows-latest
          }
        }

        steps {
          step uses="actions/checkout@v1"
          step "Install Rust" uses="actions-rs/toolchain@v1" {
            profile minimal
            toolchain "${{ matrix.rust }}"
            components clippy
            override #true
          }
          step Clippy { run cargo clippy --all -- -D warnings }
          step "Run tests" { run cargo test --all --verbose }
          step "Other Stuff" run="""
            echo foo
            echo bar
            echo baz
            """
        }
      }
    }
  |};
  [%expect {|
    name CI
    on push pull_request
    env {
      RUSTFLAGS -Dwarnings
    }
    jobs {
      fmt_and_docs "Check fmt & build docs" {
        runs-on ubuntu-latest
        steps {
          step uses="actions/checkout@v1"
          step "Install Rust" uses="actions-rs/toolchain@v1" {
            profile minimal
            toolchain stable
            components rustfmt
            override #true
          }
          step rustfmt {
            run cargo fmt --all -- --check
          }
          step docs {
            run cargo doc --no-deps
          }
        }
      }
      build_and_test "Build & Test" {
        runs-on "${{ matrix.os }}"
        strategy {
          matrix {
            rust "1.46.0" stable
            os ubuntu-latest macOS-latest windows-latest
          }
        }
        steps {
          step uses="actions/checkout@v1"
          step "Install Rust" uses="actions-rs/toolchain@v1" {
            profile minimal
            toolchain "${{ matrix.rust }}"
            components clippy
            override #true
          }
          step Clippy {
            run cargo clippy --all -- -D warnings
          }
          step "Run tests" {
            run cargo test --all --verbose
          }
          step "Other Stuff" run="echo foo\necho bar\necho baz"
        }
      }
    }
    |}]

let%expect_test "pp_compact: a complex example" =
  test_compact {|
    name CI

    on push pull_request

    env {
      RUSTFLAGS -Dwarnings
    }

    jobs {
      fmt_and_docs "Check fmt & build docs" {
        runs-on ubuntu-latest
        steps {
          step uses="actions/checkout@v1"
          step "Install Rust" uses="actions-rs/toolchain@v1" {
            profile minimal
            toolchain stable
            components rustfmt
            override #true
          }
          step rustfmt { run cargo fmt --all -- --check }
          step docs { run cargo doc --no-deps }
        }
      }
      build_and_test "Build & Test" {
        runs-on "${{ matrix.os }}"
        strategy {
          matrix {
            rust "1.46.0" stable
            os ubuntu-latest macOS-latest windows-latest
          }
        }

        steps {
          step uses="actions/checkout@v1"
          step "Install Rust" uses="actions-rs/toolchain@v1" {
            profile minimal
            toolchain "${{ matrix.rust }}"
            components clippy
            override #true
          }
          step Clippy { run cargo clippy --all -- -D warnings }
          step "Run tests" { run cargo test --all --verbose }
          step "Other Stuff" run="""
            echo foo
            echo bar
            echo baz
            """
        }
      }
    }
  |};
  [%expect {| name CI;on push pull_request;env{RUSTFLAGS -Dwarnings;};jobs{fmt_and_docs "Check fmt & build docs"{runs-on ubuntu-latest;steps{step uses="actions/checkout@v1";step "Install Rust" uses="actions-rs/toolchain@v1"{profile minimal;toolchain stable;components rustfmt;override #true;};step rustfmt{run cargo fmt --all -- --check;};step docs{run cargo doc --no-deps;};};};build_and_test "Build & Test"{runs-on "${{ matrix.os }}";strategy{matrix{rust "1.46.0" stable;os ubuntu-latest macOS-latest windows-latest;};};steps{step uses="actions/checkout@v1";step "Install Rust" uses="actions-rs/toolchain@v1"{profile minimal;toolchain "${{ matrix.rust }}";components clippy;override #true;};step Clippy{run cargo clippy --all -- -D warnings;};step "Run tests"{run cargo test --all --verbose;};step "Other Stuff" run="echo foo\necho bar\necho baz";};};}; |}]
