open Ast
open Printf

type typed_value = [
  | `I8 of int
  | `I16 of int
  | `I32 of int32
  | `I64 of int64
  | `U8 of int
  | `U16 of int
  | `U32 of int32
  | `U64 of int64
  | `Isize of nativeint
  | `Usize of nativeint
  | `F32 of float
  | `F64 of float
  | `Base64 of bytes
  | `Other of string * value
  | `Unannotated of value
]

exception Invalid_annotation of string

let interpret : annot_value -> [> typed_value] =
  let error descr = raise (Invalid_annotation descr) in
  let to_unsigned istr = (* 42 -> 0u42, 0x42 -> 0x42, etc. *)
    let with_prefix = String.length istr >= 3
      && (istr.[1] == 'x' || istr.[1] == 'o' || istr.[1] == 'b') in
    if with_prefix then istr else "0u" ^ istr in
  (* These values are only used in paths guarded by Sys.int_size checks, so we
     could theoretically write them as int literals instead of using *.to_int,
     but the compiler would probably complain on 32-bit systems *)
  let max_int32 = Int32.to_int Int32.max_int in
  let min_int32 = Int32.to_int Int32.min_int in
  let max_uint32 = Int64.to_int 0xFF_FF_FF_FFL in
  function
  | Some "i8", `Int i when i >= -0x80 && i < 0x80 -> `I8 i
  | Some "i8", `Int i -> error @@ sprintf "%d is not a valid i8" i
  | Some "i8", _ -> error "i8 expects an integer value"

  | Some "i16", `Int i when i >= -0x80_00 && i < 0x80_00 -> `I16 i
  | Some "i16", `Int i -> error @@ sprintf "%d is not a valid i16" i
  | Some "i16", _ -> error "i16 expects an integer value"

  | Some "i32", `Int i when Sys.int_size >= 33 ->
    if i >= min_int32 && i <= max_int32 then
      `I32 (Int32.of_int i)
    else
      error @@ sprintf "%d is not a valid i32" i
  | Some "i32", `Int i -> `I32 (Int32.of_int i)
  | Some "i32", `RawInt istr -> (match Int32.of_string_opt istr with
    | Some i -> `I32 i
    | None -> error @@ sprintf "%s is not a valid i32" istr)
  | Some "i32", _ -> error "i32 expects an integer value"

  | Some "i64", `Int i -> `I64 (Int64.of_int i)
  | Some "i64", `RawInt istr -> (match Int64.of_string_opt istr with
    | Some i -> `I64 i
    | None -> error @@ sprintf "%s is not a valid i64" istr)
  | Some "i64", _ -> error "i64 expects an integer value"

  | Some "u8", `Int i when i >= 0 && i <= 0xff -> `U8 i
  | Some "u8", `Int i -> error @@ sprintf "%d is not a valid u8" i
  | Some "u8", _ -> error "u8 expects an integer value"

  | Some "u16", `Int i when i >= 0 && i <= 0xff_ff -> `U16 i
  | Some "u16", `Int i -> error @@ sprintf "%d is not a valid u16" i
  | Some "u16", _ -> error "u16 expects an integer value"

  | Some "u32", `Int i when Sys.int_size >= 33 ->
    if i >= 0 && i <= max_uint32 then
      `U32 (Int32.of_int i)
    else
      error @@ sprintf "%d is not a valid u32" i
  | Some "u32", `Int i ->
    (* Note that [i] can be negative *)
    `U32 (Int32.of_int i)
  | Some "u32", `RawInt istr -> (match Int32.of_string_opt (to_unsigned istr) with
    | Some i -> `U32 i
    | None -> error @@ sprintf "%s is not a valid u32" istr)
  | Some "u32", _ -> error "u32 expects an integer value"

  | Some "u64", `Int i -> `U64 (Int64.of_int i)
  | Some "u64", `RawInt istr -> (match Int64.of_string_opt (to_unsigned istr) with
    | Some i -> `U64 i
    | None -> error @@ sprintf "%s is not a valid u64" istr)
  | Some "u64", _ -> error "u64 expects an integer value"

  | Some "isize", `Int i -> `Isize (Nativeint.of_int i)
  | Some "isize", `RawInt istr -> (match Nativeint.of_string_opt istr with
    | Some i -> `Isize i
    | None -> error @@ sprintf "%s is not a valid isize" istr)
  | Some "isize", _ -> error "isize expects an integer value"

  | Some "usize", `Int i -> `Usize (Nativeint.of_int i)
  | Some "usize", `RawInt istr -> (match Nativeint.of_string_opt (to_unsigned istr) with
    | Some i -> `Usize i
    | None -> error @@ sprintf "%s is not a valid usize" istr)
  | Some "usize", _ -> error "usize expects an integer value"

  (* There is currently no difference between f32 and f64 *)
  | Some "f32", `Float f -> `F32 f
  | Some "f32", _ -> error "f32 expects a float value"

  | Some "f64", `Float f -> `F64 f
  | Some "f64", _ -> error "f64 expects a float value"

  | Some "base64", `String base64 ->
    (try `Base64 (Base64.decode base64)
    with Base64.Error descr -> error descr)
  | Some "base64", _ -> error "base64 expects a string value"

  | Some annot, value -> `Other (annot, value)
  | None, value -> `Unannotated value

let i8 annot_value =
  match interpret annot_value with
  | `I8 i -> Some i
  | _ -> None

let i16 annot_value =
  match interpret annot_value with
  | `I16 i -> Some i
  | _ -> None

let i32 annot_value =
  match interpret annot_value with
  | `I32 i -> Some i
  | _ -> None

let i64 annot_value =
  match interpret annot_value with
  | `I64 i -> Some i
  | _ -> None

let u8 annot_value =
  match interpret annot_value with
  | `U8 i -> Some i
  | _ -> None

let u16 annot_value =
  match interpret annot_value with
  | `U16 i -> Some i
  | _ -> None

let u32 annot_value =
  match interpret annot_value with
  | `U32 i -> Some i
  | _ -> None

let u64 annot_value =
  match interpret annot_value with
  | `U64 i -> Some i
  | _ -> None

let isize annot_value =
  match interpret annot_value with
  | `Isize i -> Some i
  | _ -> None

let usize annot_value =
  match interpret annot_value with
  | `Usize i -> Some i
  | _ -> None

let f32 annot_value =
  match interpret annot_value with
  | `F32 i -> Some i
  | _ -> None

let f64 annot_value =
  match interpret annot_value with
  | `F64 i -> Some i
  | _ -> None

let base64 annot_value =
  match interpret annot_value with
  | `Base64 b -> Some b
  | _ -> None
