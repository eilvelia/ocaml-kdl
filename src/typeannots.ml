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

let pp_typed_value fmt : [< typed_value ] -> unit = function
  | `I8 x -> Format.fprintf fmt "`I8 %d" x
  | `I16 x -> Format.fprintf fmt "`I16 %d" x
  | `I32 x -> Format.fprintf fmt "`I32 %ld" x
  | `I64 x -> Format.fprintf fmt "`I64 %Ld" x
  | `U8 x -> Format.fprintf fmt "`U8 %d" x
  | `U16 x -> Format.fprintf fmt "`U16 %d" x
  | `U32 x -> Format.fprintf fmt "`U32 %ld" x
  | `U64 x -> Format.fprintf fmt "`U64 %Ld" x
  | `Isize x -> Format.fprintf fmt "`Isize %nd" x
  | `Usize x -> Format.fprintf fmt "`Usize %nd" x
  | `F32 x -> Format.fprintf fmt "`F32 %f" x
  | `F64 x -> Format.fprintf fmt "`F64 %f" x
  | `Base64 x -> Format.fprintf fmt "`Base64 \"%s\"" (String.escaped (Bytes.to_string x))
  | `Other (annot, _) -> Format.fprintf fmt "`Other %s" annot
  | `Unannotated _ -> Format.fprintf fmt "`Unannotated"

exception Invalid_annotation of string

let interpret : annot_value -> [> typed_value] =
  let error descr = raise (Invalid_annotation descr) in
  function
  | Some "i8", (#number as num) ->
    (match Num.to_int num with
    | Some x when x >= -0x80 && x < 0x80 -> `I8 x
    | _ -> error @@ sprintf "%s is not valid i8" (Num.to_string num))
  | Some "i8", _ -> error "i8 expects a numeric value"

  | Some "i16", (#number as num) ->
    (match Num.to_int num with
    | Some x when x >= -0x80_00 && x < 0x80_00 -> `I16 x
    | _ -> error @@ sprintf "%s is not valid i16" (Num.to_string num))
  | Some "i16", _ -> error "i16 expects a numeric value"

  | Some "i32", (#number as num) ->
    (match Num.to_int32 num with
    | Some x -> `I32 x
    | None -> error @@ sprintf "%s is not valid i32" (Num.to_string num))
  | Some "i32", _ -> error "i32 expects a numeric value"

  | Some "i64", (#number as num) ->
    (match Num.to_int64 num with
    | Some x -> `I64 x
    | None -> error @@ sprintf "%s is not valid i64" (Num.to_string num))
  | Some "i64", _ -> error "i64 expects a numeric value"

  | Some "u8", (#number as num) ->
    (match Num.to_int_unsigned num with
    | Some x when x >= 0 && x <= 0xff -> `U8 x
    | _ -> error @@ sprintf "%s is not valid u8" (Num.to_string num))
  | Some "u8", _ -> error "u8 expects a numeric value"

  | Some "u16", (#number as num) ->
    (match Num.to_int_unsigned num with
    | Some x when x >= 0 && x <= 0xff_ff -> `U16 x
    | _ -> error @@ sprintf "%s is not valid u16" (Num.to_string num))
  | Some "u16", _ -> error "u16 expects a numeric value"

  | Some "u32", (#number as num) ->
    (match Num.to_int32_unsigned num with
    | Some x -> `U32 x
    | None -> error @@ sprintf "%s is not valid u32" (Num.to_string num))
  | Some "u32", _ -> error "u32 expects a numeric value"

  | Some "u64", (#number as num) ->
    (match Num.to_int64_unsigned num with
    | Some x -> `U64 x
    | None -> error @@ sprintf "%s is not valid u64" (Num.to_string num))
  | Some "u64", _ -> error "u64 expects a numeric value"

  | Some "isize", (#number as num) ->
    (match Num.to_nativeint num with
    | Some x -> `Isize x
    | None -> error @@ sprintf "%s is not valid isize" (Num.to_string num))
  | Some "isize", _ -> error "isize expects a numeric value"

  | Some "usize", (#number as num) ->
    (match Num.to_nativeint_unsigned num with
    | Some x -> `Usize x
    | None -> error @@ sprintf "%s is not valid usize" (Num.to_string num))
  | Some "usize", _ -> error "usize expects a numeric value"

  (* There is currently no difference between f32 and f64 *)
  | Some "f32", (#number as num) -> `F32 (Num.to_float num)
  | Some "f32", _ -> error "f32 expects a numeric value"

  | Some "f64", (#number as num) -> `F64 (Num.to_float num)
  | Some "f64", _ -> error "f64 expects a numeric value"

  | Some "base64", `String base64 ->
    (try `Base64 (Base64.decode base64) with
    | Base64.Error descr -> error descr)
  | Some "base64", _ -> error "base64 expects a string value"

  | Some annot, value -> `Other (annot, value)
  | None, value -> `Unannotated value
