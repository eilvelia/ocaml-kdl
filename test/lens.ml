(* Lenses are not extensively tested for now, this contains some basic tests
   for complicated cases *)

let doc =
  try Kdl.of_string_exn {|
  contents #null {
    section "First section" {
      paragraph "This is the first paragraph"
      paragraph "This is the second paragraph"
    }
  }; node1 #true; node2 #false|}
  with Kdl.Parse_error err ->
    failwith (Format.asprintf "%a" Kdl.pp_error err)

open Kdl.L

let print = function
  | Some kdl -> Format.printf "%a@.@." Kdl.pp kdl
  | None -> print_endline "<None>"

let%expect_test "each set 1" =
  let lens = top // child "section"
                 // child_many "paragraph" // each (arg 0 // value) in
  print @@ doc.@(lens) <- [];
  print @@ doc.@(lens) <- [`String "foo"];
  print @@ doc.@(lens) <- [`String "foo"; `String "bar"];
  print @@ doc.@(lens) <- [`String "foo"; `String "bar"; `String "baz"];
  [%expect {|
    contents #null {
      section "First section" {
        paragraph "This is the first paragraph"
        paragraph "This is the second paragraph"
      }
    }
    node1 #true
    node2 #false

    contents #null {
      section "First section" {
        paragraph foo
        paragraph "This is the second paragraph"
      }
    }
    node1 #true
    node2 #false

    contents #null {
      section "First section" {
        paragraph foo
        paragraph bar
      }
    }
    node1 #true
    node2 #false

    contents #null {
      section "First section" {
        paragraph foo
        paragraph bar
      }
    }
    node1 #true
    node2 #false
    |}]

let%expect_test "each set 2" =
  print @@ doc.@(each @@ arg 0 // value) <- [`String "foo"];
  print @@ doc.@(each @@ args // each value) <- [[`String "foo"]; [`Null]];
  [%expect {|
    contents foo {
      section "First section" {
        paragraph "This is the first paragraph"
        paragraph "This is the second paragraph"
      }
    }
    node1 #true
    node2 #false

    contents foo {
      section "First section" {
        paragraph "This is the first paragraph"
        paragraph "This is the second paragraph"
      }
    }
    node1 #null
    node2 #false
    |}]

let%expect_test "the string lens" =
  print_endline
    doc.@!(top // child_nth 0 // child "paragraph" // arg 0 // value // string);
  [%expect {| This is the first paragraph |}]

let%expect_test "the number lens" =
  let doc = Kdl.of_string_exn {|- 3.14|} in
  Format.printf "%a\n" Kdl.pp_value
    doc.@!(top // arg 0 // value // number);
  print @@ doc.@(top // arg 0 // value // number) <- `Float_raw "4.15";
  [%expect {|
    3.14
    - 4.15 |}]
