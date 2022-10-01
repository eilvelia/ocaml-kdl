open Sexplib0

type value = [
  | `String of string
  | `Int of int
  | `RawInt of string
  | `Float of float
  | `Bool of bool
  | `Null
]

type annot_value = string option * value

type prop = string * annot_value

type node = {
  name : string; (** An identifier or a quoted string *)
  annot : string option;
  args : annot_value list;
  props : prop list; (** [props] is an assoc list; the order of [props] is unspecified *)
  children : node list;
}

type t = node list

let equal_value (v1 : value) (v2 : value) =
  match v1, v2 with
  | `String s1, `String s2 -> String.equal s1 s2
  | `Int i1, `Int i2 -> Int.equal i1 i2
  | `RawInt ri1, `RawInt ri2 -> String.equal ri1 ri2
  | `Float f1, `Float f2 -> Float.equal f1 f2
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

let sexp_of_value = function
  | `String str -> Sexp.List [Atom "string"; Atom str]
  | `Int int -> Sexp.List [Atom "int"; Sexp_conv.sexp_of_int int]
  | `RawInt rint -> Sexp.List [Atom "raw-int"; Atom rint]
  | `Float float -> Sexp.List [Atom "float"; Sexp_conv.sexp_of_float float]
  | `Bool true -> Sexp.Atom "true"
  | `Bool false -> Sexp.Atom "false"
  | `Null -> Sexp.Atom "null"

let sexp_of_annot_value = function
  | None, v -> sexp_of_value v
  | Some annot, v -> match sexp_of_value v with
    | Sexp.List xs -> Sexp.List (Atom "<type>" :: Atom annot :: xs)
    | Sexp.Atom _ as atom -> Sexp.List [Atom "<type>"; Atom annot; atom]

let sexp_of_prop = Sexp_conv.(sexp_of_pair sexp_of_string sexp_of_annot_value)

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

(* TODO: *)
(* module Raw = struct
  type t
end *)
