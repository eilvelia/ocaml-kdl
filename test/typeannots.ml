open Printf

(* TODO: Should we include this in kdl? *)
let show_typed_value : Kdl.typed_value -> string = function
  | `I8 x -> sprintf "I8 %d" x
  | `I16 x -> sprintf "I16 %d" x
  | `I32 x -> sprintf "I32 %ld" x
  | `I64 x -> sprintf "I64 %Ld" x
  | `U8 x -> sprintf "U8 %d" x
  | `U16 x -> sprintf "U16 %d" x
  | `U32 x -> sprintf "U32 %ld" x
  | `U64 x -> sprintf "U64 %Ld" x
  | `Isize x -> sprintf "Isize %nd" x
  | `Usize x -> sprintf "Usize %nd" x
  | `F32 x -> sprintf "F32 %f" x
  | `F64 x -> sprintf "F64 %f" x
  | `Base64 x -> sprintf "Base64 %s" @@ Bytes.to_string (Bytes.escaped x)
  | `Other (annot, _) -> sprintf "Other (%s)" annot
  | `Unannotated _ -> "Unannotated"

let test ?prefix ?(quiet = false) val_string =
  let source = sprintf "- %s" val_string in
  let node = List.hd @@ Kdl.from_string_exn source in
  let annot_value = List.hd node.args in
  (* Kdl.sexp_of_annot_value annot_value
    |> Sexplib0.Sexp.pp_hum_indent 2 Format.std_formatter; *)
  let show = if quiet then fun _ -> "" else show_typed_value in
  let msg =
    try show @@ Kdl.interpret annot_value with
    | Kdl.Invalid_annotation desc -> sprintf "Invalid_annotation: %s" desc in
  match prefix with
  | Some p -> printf "%s: %s\n" p msg
  | None -> print_endline msg

let tests l = List.iteri (fun i x -> test ~prefix:(string_of_int (i + 1)) x) l

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
    1: I8 120
    2: I8 -30
    3: Invalid_annotation: 130 is not valid i8
    4: Invalid_annotation: 130 is not valid i8
    5: I8 30
    6: Invalid_annotation: 30.5 is not valid i8 |}]

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
    1: U8 0
    2: U8 250
    3: U8 0
    4: Invalid_annotation: 256 is not valid u8
    5: Invalid_annotation: -30 is not valid u8
    6: U8 30
    7: Invalid_annotation: 30.5 is not valid u8 |}]

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
    1: U32 2147483647
    2: U32 -2119447057
    3: U32 0
    4: U32 0
    5: Invalid_annotation: -40 is not valid u32
    6: U32 0
    7: Invalid_annotation: 50.6 is not valid u32 |}]
