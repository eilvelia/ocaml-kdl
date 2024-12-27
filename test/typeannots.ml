let test ?prefix ?(quiet = false) val_string =
  let source = Printf.sprintf "- %s" val_string in
  let node = List.hd @@ Kdl.of_string_exn source in
  let annot_value = List.hd node.args in
  (* Kdl.sexp_of_annot_value annot_value
    |> Sexplib0.Sexp.pp_hum_indent 2 Format.std_formatter; *)
  let msg =
    match Kdl.interpret annot_value with
    | _ when quiet -> ""
    | avalue -> Format.asprintf "%a" Kdl.pp_typed_value avalue
    | exception Kdl.Invalid_annotation desc ->
      Printf.sprintf "Invalid_annotation: %s" desc
  in
  match prefix with
  | Some p -> Printf.printf "%s: %s\n" p msg
  | None -> print_endline msg

let tests l =
  List.iteri (fun i x -> test ~prefix:(string_of_int (i + 1)) x) l

open Printf

let%expect_test "i8" =
  tests [
    "(i8)120";
    "(i8)-30";
    "(i8)130";
    "(i8)0x82";
    "(i8)30.0";
    "(i8)30.5";
  ];
  [%expect {|
    1: `I8 120
    2: `I8 -30
    3: Invalid_annotation: 130 is not valid i8
    4: Invalid_annotation: 130 is not valid i8
    5: `I8 30
    6: Invalid_annotation: 30.5 is not valid i8
    |}]

let%expect_test "u8" =
  tests [
    "(u8)0";
    "(u8)250";
    "(u8)-0";
    "(u8)256";
    "(u8)-30";
    "(u8)30.0";
    "(u8)30.5";
  ];
  [%expect {|
    1: `U8 0
    2: `U8 250
    3: `U8 0
    4: Invalid_annotation: 256 is not valid u8
    5: Invalid_annotation: -30 is not valid u8
    6: `U8 30
    7: Invalid_annotation: 30.5 is not valid u8
    |}]

let%expect_test "unsigned values that are in the negative range of ocaml int" =
  test ~quiet:true @@ sprintf "(usize)%u" (1 lsl (Sys.int_size - 1));
  test ~quiet:true @@ sprintf "(usize)0x%x" (1 lsl (Sys.int_size - 1));
  test ~quiet:true @@ sprintf "(isize)0x%x" (1 lsl (Sys.int_size - 1));
  [%expect {| |}]

let%expect_test "u32" =
  tests [
    "(u32)0x7f_ff_ff_ff";
    "(u32)0x81_ab_cd_ef";
    "(u32)0";
    "(u32)-0";
    "(u32)-40";
    "(u32)0.0";
    "(u32)50.6";
  ];
  [%expect {|
    1: `U32 2147483647
    2: `U32 -2119447057
    3: `U32 0
    4: `U32 0
    5: Invalid_annotation: -40 is not valid u32
    6: `U32 0
    7: Invalid_annotation: 50.6 is not valid u32
    |}]
