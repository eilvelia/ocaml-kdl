open Ast

let is_keyword = function
  | "true" | "false" | "null" -> true
  | _ -> false

let digits = "0123456789"

let[@inline] escape str result =
  let n = ref 0 in
  String.iter (function
    | '\\' | '"' as ch ->
      Bytes.set result !n '\\'; Bytes.set result (!n + 1) ch; n := !n + 2
    | ch -> Bytes.set result !n ch; incr n
  ) str

let pp_ident fmt str =
  let escape_len = ref 0 in
  let contains_invalid_identchar = ref false in
  String.iter (function
    | '\\' | '"' -> incr escape_len; contains_invalid_identchar := true
    | '/' | '(' | ')' | '{' | '}' | '<' | '>' | ';' | '[' | ']' | '=' | ','
    (* TODO: This should also include other unicode spaces/newlines *)
    | '\000'..'\x20' ->
      contains_invalid_identchar := true
    | _ -> ()
  ) str;
  let dash_digit =
    String.length str >= 2 && str.[0] = '-' && String.contains digits str.[1] in
  let empty = String.length str <= 0 in
  let digit_start = not empty && String.contains digits str.[0] in
  let quoted = !contains_invalid_identchar || is_keyword str || empty
               || dash_digit || digit_start in
  if quoted then Format.pp_print_char fmt '"';
  let result = Bytes.create (String.length str + !escape_len) in
  escape str result;
  Format.pp_print_bytes fmt result;
  if quoted then Format.pp_print_char fmt '"'

let pp_string_value fmt str =
  let result_len = String.fold_left
    (fun l -> function '\\' | '"' -> l + 1 | _ -> l)
    (String.length str) str in
  let result = Bytes.create result_len in
  escape str result;
  Format.pp_print_char fmt '"';
  Format.pp_print_bytes fmt result;
  Format.pp_print_char fmt '"'

let pp_value fmt : [< value] -> _ = function
  | `String s -> pp_string_value fmt s
  | `Int i -> Format.pp_print_int fmt i
  | `RawInt i -> Format.pp_print_string fmt i
  | `Float f -> Format.pp_print_float fmt f
  | `Bool b -> Format.pp_print_bool fmt b
  | `Null -> Format.pp_print_string fmt "null"

let pp_annot_value fmt = function
  | Some annot, v -> Format.fprintf fmt "(%a)%a" pp_ident annot pp_value v
  | None, v -> pp_value fmt v

let pp_prop fmt (key, value) =
  Format.fprintf fmt "%a=%a" pp_ident key pp_annot_value value

let space fmt () = Format.pp_print_string fmt " "
let semi fmt () = Format.pp_print_string fmt ";"

let pp_entity_list f fmt = function
  | [] -> ()
  | xs ->
    space fmt ();
    (* TODO: We can probably use pp_print_custom_break to inject \ as line
       separators on breaks *)
    Format.pp_print_list ~pp_sep:space f fmt xs

let indent = ref 2

let pp_node_annot fmt annot =
  let pp fmt str = Format.fprintf fmt "(%a)" pp_ident str in
  Format.pp_print_option pp fmt annot

let rec pp_node fmt n =
  Format.pp_open_vbox fmt !indent;
  pp_node_annot fmt n.annot;
  pp_ident fmt n.name;
  pp_entity_list pp_annot_value fmt n.args;
  pp_entity_list pp_prop fmt n.props;
  match n.children with
  | _ :: _ as children ->
    Format.pp_print_string fmt " {";
    Format.pp_print_cut fmt ();
    pp_nodes fmt children;
    Format.pp_print_break fmt 0 ~-(!indent); (* I think it's ok *)
    Format.pp_close_box fmt ();
    Format.pp_print_string fmt "}"
  | [] -> Format.pp_close_box fmt ()

and pp_nodes fmt xs =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_node fmt xs

let pp fmt t =
  Format.pp_open_vbox fmt 0;
  pp_nodes fmt t;
  Format.pp_close_box fmt ()

let rec pp_node_compact fmt n =
  pp_node_annot fmt n.annot;
  pp_ident fmt n.name;
  pp_entity_list pp_annot_value fmt n.args;
  pp_entity_list pp_prop fmt n.props;
  match n.children with
  | _ :: _ as children ->
    Format.pp_print_string fmt "{";
    pp_nodes_compact fmt children;
    Format.pp_print_string fmt "}"
  | [] -> ()

and pp_nodes_compact fmt = function
  | [] -> ()
  | xs ->
    Format.pp_open_hbox fmt ();
    Format.pp_print_list ~pp_sep:semi pp_node_compact fmt xs;
    semi fmt ();
    Format.pp_close_box fmt ()

let pp_compact = pp_nodes_compact

let to_string t =
  let buf = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt t;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let show = to_string
