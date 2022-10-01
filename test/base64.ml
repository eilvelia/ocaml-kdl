let test encoded =
  match Kdl.base64 (Some "base64", `String encoded) with
  | Some bytes -> print_bytes bytes; print_newline ()
  | None -> failwith "Expected `Base64"

let%expect_test "4 bytes without padding" =
  test {|TWFu|};
  [%expect {| Man |}]

let%expect_test "3 bytes and one byte of padding" =
  test {|TWE=|};
  [%expect {| Ma |}]

let%expect_test "3 bytes without explicit padding" =
  test {|TWE|};
  [%expect {| Ma |}]

let%expect_test "'a' encoded with and without padding" =
  test {|YQ|};
  test {|YQ=|};
  test {|YQ==|};
  [%expect {|
    a
    a
    a |}]

let%expect_test "arbitrary number of = is allowed" =
  test {|TWE======|};
  test {|TWE==|};
  [%expect {|
    Ma
    Ma |}]

let%expect_test "'YA' correctly decodes to a backtick" =
  test {|YA|};
  [%expect {| ` |}]

let%expect_test "a larger sentence" =
  test {|VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==|};
  test {|VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw|};
  test {|VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw=|};
  test {|VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw===|};
  [%expect {|
    The quick brown fox jumps over the lazy dog
    The quick brown fox jumps over the lazy dog
    The quick brown fox jumps over the lazy dog
    The quick brown fox jumps over the lazy dog |}]

let%expect_test "padding cannot be >= 3" =
  try test {|Y|} with Failure msg -> Printf.printf "Failure: %s\n" msg;
  try test {|TWFuY|} with Failure msg -> Printf.printf "Failure: %s\n" msg;
  [%expect {|
    Failure: Invalid base64: a padding of 3 sextets is invalid
    Failure: Invalid base64: a padding of 3 sextets is invalid |}]

let%expect_test "empty string" =
  test {||};
  test {|=|};
  test {|==|};
  test {|===|};
  [%expect {| |}]
