open Ast

let pp_string fmt str =
  if Re2c_lexer.is_valid_ident str then
    Format.pp_print_string fmt str
  else begin
    Format.pp_print_char fmt '"';
    Format.pp_print_string fmt (Re2c_lexer.escape_string str);
    Format.pp_print_char fmt '"';
  end

let pp_value fmt : [< value] -> _ = function
  | `String s -> pp_string fmt s
  | #number as num -> Format.pp_print_string fmt (match Num.to_string num with
    | "inf" -> "#inf"
    | "-inf" -> "#-inf"
    | "nan" -> "#nan"
    | str -> str)
  | `Bool true -> Format.pp_print_string fmt "#true"
  | `Bool false -> Format.pp_print_string fmt "#false"
  | `Null -> Format.pp_print_string fmt "#null"

let pp_annot_value fmt = function
  | Some annot, v -> Format.fprintf fmt "(%a)%a" pp_string annot pp_value v
  | None, v -> pp_value fmt v

let pp_prop fmt (key, value) =
  Format.fprintf fmt "%a=%a" pp_string key pp_annot_value value

let space fmt () = Format.pp_print_string fmt " "
let semi fmt () = Format.pp_print_string fmt ";"

let pp_entity_list f fmt = function
  | [] -> ()
  | xs ->
    space fmt ();
    (* TODO: We can perhaps use pp_print_custom_break to inject \ as line
       separators on breaks *)
    Format.pp_print_list ~pp_sep:space f fmt xs

let indent = ref 2

let pp_node_annot fmt annot =
  let pp fmt str = Format.fprintf fmt "(%a)" pp_string str in
  Format.pp_print_option pp fmt annot

let rec pp_node fmt n =
  Format.pp_open_vbox fmt !indent;
  pp_node_annot fmt n.annot;
  pp_string fmt n.name;
  pp_entity_list pp_annot_value fmt n.args;
  pp_entity_list pp_prop fmt n.props;
  match n.children with
  | _ :: _ as children ->
    Format.pp_print_string fmt " {";
    Format.pp_print_cut fmt ();
    pp_nodes fmt children;
    Format.pp_print_break fmt 0 ~-(!indent); (* x_x i think it's ok *)
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
  pp_string fmt n.name;
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
