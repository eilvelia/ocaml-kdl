open Sexplib0

(** [signed_int_of_string_opt str] is similar to [int_of_string_opt str],
    but prohibits unsigned ranges in case the prefix of the number is one of
    [0x 0b 0o]. This function assumes that negative numbers begin with [-]. *)
let signed_int_of_string_opt str =
  let negative = String.length str >= 1 && String.unsafe_get str 0 = '-' in
  match int_of_string str with
  | int when int < 0 && not negative -> None
  | int when int > 0 && negative -> None
  | int -> Some int
  | exception Failure _ -> None

module Num = struct
  type t = [
    | `Int of int
    | `Int_raw of string
    | `Float_raw of string
  ]

  (* Note: Float.to_string outputs "inf", "-inf", "nan" for the infinity,
     neg_infinity, and nan values respectively; Float.of_string can parse those
     strings. That makes the functions suitable for KDL's #inf, #-inf, #nan
     as-is. *)

  let to_string : [< t ] -> string = function
    | `Int int -> Int.to_string int
    | `Int_raw str
    | `Float_raw str -> str

  let to_float : [< t ] -> float = function
    | `Int int -> Float.of_int int
    | `Int_raw str
    | `Float_raw str -> float_of_string str

  open struct
    let check_safe_int_float_bounds =
      let min = -9007199254740991. and max = 9007199254740991. in
      fun f -> f >= min && f <= max

    let safe_int_float_of_string str =
      match float_of_string str with
      | f when Float.is_integer f && check_safe_int_float_bounds f -> Some f
      | _ -> None
      | exception Failure _ -> None

    let check_int_bounds =
      let min = Int.to_float Int.min_int and max = Int.to_float Int.max_int in
      fun f -> f >= min && f <= max

    let check_unsigned_int_bounds =
      let max = Int.to_float Int.max_int *. 2. +. 1. in
      fun f -> f >= 0. && f <= max

    let check_int32_bounds =
      let min = Int32.to_float Int32.min_int
      and max = Int32.to_float Int32.max_int in
      fun f -> f >= min && f <= max

    let check_unsigned_int32_bounds =
      let max = Int32.to_float Int32.max_int *. 2. +. 1. in
      fun f -> f >= 0. && f <= max

    let check_nativeint_bounds =
      let min = Nativeint.to_float Nativeint.min_int
      and max = Nativeint.to_float Nativeint.max_int in
      fun f -> f >= min && f <= max

    let check_unsigned_nativeint_bounds =
      let max = Nativeint.to_float Nativeint.max_int *. 2. +. 1. in
      fun f -> f >= 0. && f <= max

    let[@inline] to_unsigned_literal lit =
      (* Somewhat hacky. Makes sure decimal numbers are prefixed with 0u.
         Examples: 42 -> 0u42, 0x42 -> 0x42.
         Fails for negative non-zero numbers. *)
      let len = String.length lit in
      if len >= 1 && lit.[0] = '-' && not (len = 2 && lit.[1] = '0') then
        None
      else if len >= 3 then
        match lit.[1] with
        | 'x' | 'o' | 'b' | 'u' -> Some lit
        | _ -> Some ("0u" ^ lit)
      else
        Some lit

    let (>>=) = Option.bind
  end

  let to_int : [< t ] -> int option = function
    | `Int int -> Some int
    | `Int_raw str -> int_of_string_opt str
    | `Float_raw str ->
      match safe_int_float_of_string str with
      | Some f when check_int_bounds f -> Some (Int.of_float f)
      | _ -> None

  let to_int_unsigned : [< t ] -> int option = function
    | `Int int -> if int >= 0 then Some int else None
    | `Int_raw str -> to_unsigned_literal str >>= int_of_string_opt
    | `Float_raw str ->
      match safe_int_float_of_string str with
      | Some f when check_unsigned_int_bounds f -> Some (Int.of_float f)
      | _ -> None

  let to_int32 : [< t ] -> int32 option = function
    | `Int int when Sys.int_size > 32 && int > Int32.to_int Int32.max_int ->
      None
    | `Int int -> Some (Int32.of_int int)
    | `Int_raw str -> Int32.of_string_opt str
    | `Float_raw str ->
      match safe_int_float_of_string str with
      | Some f when check_int32_bounds f -> Some (Int32.of_float f)
      | _ -> None

  let to_int32_unsigned : [< t ] -> int32 option = function
    | `Int int when int < 0 -> None
    | `Int int when Sys.int_size > 32 && int > Int32.to_int Int32.max_int lsl 1 + 1 ->
      None
    | `Int int -> Some (Int32.of_int int)
    | `Int_raw str -> to_unsigned_literal str >>= Int32.of_string_opt
    | `Float_raw str ->
      match safe_int_float_of_string str with
      | Some f when check_unsigned_int32_bounds f -> Some (Int32.of_float f)
      | _ -> None

  let to_int64 : [< t ] -> int64 option = function
    | `Int int -> Some (Int64.of_int int)
    | `Int_raw str -> Int64.of_string_opt str
    | `Float_raw str ->
      match safe_int_float_of_string str with
      | Some f -> Some (Int64.of_float f)
      | None -> None

  let to_int64_unsigned : [< t ] -> int64 option = function
    | `Int int when int < 0 -> None
    | `Int int -> Some (Int64.of_int int)
    | `Int_raw str -> to_unsigned_literal str >>= Int64.of_string_opt
    | `Float_raw str ->
      match safe_int_float_of_string str with
      | Some f when f >= 0.0 -> Some (Int64.of_float f)
      | _ -> None

  let to_nativeint : [< t ] -> nativeint option = function
    | `Int int when Nativeint.size < Sys.int_size && int > Nativeint.(to_int max_int) ->
      None
    | `Int int -> Some (Nativeint.of_int int)
    | `Int_raw str -> Nativeint.of_string_opt str
    | `Float_raw str ->
      match safe_int_float_of_string str with
      | Some f when check_nativeint_bounds f -> Some (Nativeint.of_float f)
      | _ -> None

  let to_nativeint_unsigned : [< t ] -> nativeint option = function
    | `Int int when int < 0 -> None
    | `Int int when Nativeint.size < Sys.int_size ->
      (match Nativeint.(unsigned_to_int (neg 1n)) with
      | Some max when int <= max -> Some (Nativeint.of_int int)
      | Some _ | None -> None)
    | `Int int -> Some (Nativeint.of_int int)
    | `Int_raw str -> to_unsigned_literal str >>= Nativeint.of_string_opt
    | `Float_raw str ->
      match safe_int_float_of_string str with
      | Some f when check_unsigned_nativeint_bounds f -> Some (Nativeint.of_float f)
      | _ -> None

  let to_int_exn num =
    match to_int num with Some x -> x | None -> failwith "Kdl.Num.to_int_exn"

  let to_int32_exn num =
    match to_int32 num with Some x -> x | None -> failwith "Kdl.Num.to_int32_exn"

  let to_int64_exn num =
    match to_int64 num with Some x -> x | None -> failwith "Kdl.Num.to_int64_exn"

  let to_nativeint_exn num =
    match to_nativeint num with
    | Some x -> x
    | None -> failwith "Kdl.Num.to_nativeint_exn"

  let of_string : string -> [> t ] option = fun input ->
    (* Note: Does not check that the literal is a valid KDL number *)
    match signed_int_of_string_opt input with
    | Some x -> Some (`Int x)
    | None ->
      match float_of_string_opt input with
      | Some _ -> Some (`Float_raw input)
      | None -> None

  let of_float : float -> [> t ] = fun x ->
    if Float.is_integer x && check_safe_int_float_bounds x && check_int_bounds x then
      `Int (Float.to_int x)
    else `Float_raw (Float.to_string x)

  let of_int : int -> [> t ] = fun x -> `Int x

  let of_int32 : int32 -> [> t ] =
    if Sys.int_size >= 32 then
      fun x -> `Int (Int32.to_int x)
    else fun x ->
      let min = Int32.of_int Int.min_int and max = Int32.of_int Int.max_int in
      let fits = x >= min && x <= max in
      if fits then `Int (Int32.to_int x) else `Int_raw (Int32.to_string x)

  let of_int64 : int64 -> [> t ] = fun x ->
    let min = Int64.of_int Int.min_int and max = Int64.of_int Int.max_int in
    let fits = x >= min && x <= max in
    if fits then `Int (Int64.to_int x) else `Int_raw (Int64.to_string x)

  let of_nativeint : nativeint -> [> t ] =
    if Nativeint.size <= Sys.int_size then
      fun x -> `Int (Nativeint.to_int x)
    else fun x ->
      let min = Nativeint.of_int Int.min_int
      and max = Nativeint.of_int Int.max_int in
      let fits = x >= min && x <= max in
      if fits then `Int (Nativeint.to_int x) else `Int_raw (Nativeint.to_string x)

  let equal (x : [< t ]) (y : [< t ]) =
    match x, y with
    | `Int i1, `Int i2 -> Int.equal i1 i2
    (* The strings are not necessarily normalized *)
    | `Int_raw s1, `Int_raw s2 -> String.equal s1 s2
    | `Float_raw d1, `Float_raw d2 -> String.equal d1 d2
    | `Int i, `Int_raw s | `Int_raw s, `Int i -> String.equal (Int.to_string i) s
    | `Int i, `Float_raw d | `Float_raw d, `Int i ->
      (match safe_int_float_of_string d with
      | Some f -> Int.equal (Float.to_int f) i
      | None -> false)
    | `Int_raw ilit, `Float_raw d | `Float_raw d, `Int_raw ilit ->
      (match safe_int_float_of_string d with
      | Some f -> String.equal (Float.to_string f) ilit
      | None -> false)
end

type number = Num.t

type value = [
  | `String of string
  | number
  | `Bool of bool
  | `Null
]

type annot_value = string option * value

type prop = string * annot_value

type node = {
  name : string;
  annot : string option;
  args : annot_value list;
  props : prop list; (** [props] is an assoc list; the order is unspecified *)
  children : node list;
}

type t = node list

let equal_value (v1 : value) (v2 : value) =
  match v1, v2 with
  | `String s1, `String s2 -> String.equal s1 s2
  | (#number as n1), (#number as n2) -> Num.equal n1 n2
  | `Bool true, `Bool true -> true
  | `Bool false, `Bool false -> true
  | `Null, `Null -> true
  | _ -> false

let equal_annot_value (annot1, v1 : annot_value) (annot2, v2 : annot_value) =
  Option.equal String.equal annot1 annot2 && equal_value v1 v2

let equal_prop (name1, annot_value1 : prop) (name2, annot_value2 : prop) =
  String.equal name1 name2 && equal_annot_value annot_value1 annot_value2

let equal_node n1 n2 =
  String.equal n1.name n2.name
  && Option.equal String.equal n1.annot n2.annot
  && List.equal equal_annot_value n1.args n2.args
  && List.equal equal_prop n1.props n2.props

let equal nodes1 nodes2 = List.equal equal_node nodes1 nodes2

let sexp_of_value : [< value ] -> Sexp.t = function
  | `String str -> Sexp.List [Atom "string"; Atom str]
  | #number as num ->
    let tag =
      match num with
      | `Int _ -> "int"
      | `Int_raw _ -> "int-raw"
      | `Float_raw _ -> "float-raw" in
    Sexp.List [Atom (Printf.sprintf "number-%s" tag); Atom (Num.to_string num)]
  | `Bool true -> Sexp.List [Atom "bool"; Atom "true"]
  | `Bool false -> Sexp.List [Atom "bool"; Atom "false"]
  | `Null -> Sexp.List [Atom "null"]

let sexp_of_annot_value = function
  | None, v -> sexp_of_value v
  | Some annot, v -> match sexp_of_value v with
    | Sexp.List [x1; x2] -> Sexp.List [x1; x2; Atom annot]
    | Sexp.List [x] -> Sexp.List [x; Atom annot]
    | Sexp.List xs -> Sexp.List (xs @ [Atom annot])
    | Sexp.Atom _ as atom -> Sexp.List [atom; Atom annot]

let sexp_of_prop (key, value) =
  Sexp.List [Atom "prop"; Atom key; sexp_of_annot_value value]

let rec sexp_of_node node =
  let children = List.rev @@ List.rev_map sexp_of_node node.children in
  let list = match children with
    | [] -> []
    | _ -> [Sexp.List (Atom "children" :: children)] in
  let props = List.rev_map sexp_of_prop node.props in
  let list = List.rev_append props list in
  let args = List.rev_map sexp_of_annot_value node.args in
  let list = List.rev_append args list in
  let list = match node.annot with
    | Some a -> Sexp.List [Atom "type"; Atom a] :: list
    | None -> list
  in
  let list = Sexp.Atom node.name :: list in
  Sexp.List list

let sexp_of_t = function
  | [node] -> sexp_of_node node
  | nodes -> Sexp_conv.sexp_of_list sexp_of_node nodes

let node ?annot name ?(args = []) ?(props = []) children =
  { name; annot; args; props; children }

let arg ?(annot : string option) value = annot, value

let prop ?(annot : string option) name value = name, (annot, value)
