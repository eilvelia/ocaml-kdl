%{
open Ast

let[@inline] cons_if_some x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

let error loc msg = raise @@ Err.CustomParsingError (msg, loc)
%}

%token NEWLINE
%token BOM
%token SEMI ";"
%token <string> IDENT_STRING
%token <string> QUOTED_STRING
%token <string> RAW_STRING
%token <string> INTEGER
%token <string> FLOAT
%token TRUE
%token FALSE
%token NULL
%token DISABLE "/-"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token EQ
%token EOF

// The following tokens are used in KQL only
// %token LBRACKET "["
// %token RBRACKET "]"
// %token GT ">"
// %token GTE ">="
// %token LT "<"
// %token LTE "<="
// %token PLUS "+"
// %token TILDE "~"
// %token PARALLEL "||"
// %token CARET_EQ "^="
// %token DOLLAR_EQ "$="
// %token STAR_EQ "*="

%start <Ast.t> document
%%

document: BOM? xs=nodes EOF { xs } ;

%inline any_string:
  | id=IDENT_STRING {
    match id with
    | "true" | "false" | "null" | "inf" | "-inf" | "nan" ->
      error $loc(id) @@
        Printf.sprintf "A keyword must be quoted. Did you mean \"%s\" or #%s?"
          id id
    | _ -> id
  }
  | str=QUOTED_STRING { str }
  | rstr=RAW_STRING { rstr }
;

nodes:
  | NEWLINE* { [] }
  | NEWLINE* x=node EOF { cons_if_some x [] }
  | NEWLINE* x=node ";" xs=nodes { cons_if_some x xs }
  | NEWLINE* x=node NEWLINE xs=nodes { cons_if_some x xs }
;

children_nodes:
  | NEWLINE* { [] }
  | NEWLINE* x=node ";" xs=children_nodes { cons_if_some x xs }
  | NEWLINE* x=node NEWLINE xs=children_nodes { cons_if_some x xs }
  | NEWLINE* x=node { cons_if_some x [] }
;

node:
  | "/-" type_annot? any_string entity*
    { None }
  | annot=type_annot? name=any_string entities=entity* {
    let known_props : (string, prop) Hashtbl.t = Hashtbl.create 4 in
    let f (args, props, children) entity =
      match entity, children with
      | `Disabled, _ -> args, props, children
      | `Children (_, loc), Some _ ->
        error loc "A children block must be defined only once"
      | `Children c, None -> args, props, Some c
      | (`Prop _ | `Arg _), Some (_, loc) ->
        error loc "A children block must be the last element of a node"
      | `Arg a, _ -> a :: args, props, children
      (* This is not optimal, but should be fine for most cases.
         Every prop is checked against the known_props hash table, this
         operates in O(1) on average. If the prop has been already defined,
         it is removed from the list in O(n) time. It is assumed that
         redefinitions of properties should be rare. *)
      | `Prop (name, _ as p), _ when Hashtbl.mem known_props name ->
        args, p :: List.filter (fun (n, _) -> n <> name) props, children
      | `Prop (name, _ as p), _ ->
        Hashtbl.add known_props name p;
        args, p :: props, children
    in
    let args, props, children_opt = List.fold_left f ([], [], None) entities in
    let children = match children_opt with None -> [] | Some (xs, _loc) -> xs in
    let args = List.rev args in
    let props = List.rev props in
    Some { name; annot; args; props; children }
  }
  | type_annot? _x=TRUE error { error $loc(_x) "A keyword is not a valid node name" }
  | type_annot? _x=FALSE error { error $loc(_x) "A keyword is not a valid node name" }
  | type_annot? _x=NULL error { error $loc(_x) "A keyword is not a valid node name" }
  | type_annot? _x=INTEGER error { error $loc(_x) "A number is not a valid node name" }
  | type_annot? _x=FLOAT error { error $loc(_x) "A number is not a valid node name" }
  | type_annot _x=error { error $loc(_x) "Expected a node name" }
  | "/-" _x=error { error $loc(_x) "Expected a node after a '/-'"}
  | _x=")" error { error $loc(_x) "Unexpected token ')'" }
  | _x=EQ error { error $loc(_x) "Unexpected token '='" }
;

entity:
  | "/-" prop_or_arg { `Disabled }
  | "/-" children { `Disabled }
  | x=prop_or_arg { x }
  | x=children { `Children (x, $sloc) }
  | "/-" _x="}" error { error $loc(_x) "Unexpected token '}'" }
;

prop_or_arg:
  (* (requires lookahead to the = token to differentiate from string values) *)
  | key=any_string _eq=EQ aval=annot_value { `Prop (key, aval) }
  | aval=annot_value { `Arg aval }
;

%inline annot_value:
  | annot=type_annot value=value { Some annot, value }
  | value=value { None, value }
;

value:
  | value=any_string { `String value }
  | value=INTEGER {
    (* Even if the input is positive, int_of_string_opt can return a
       negative integer for 0x / 0o / 0b prefixes. We probably want to avoid
       this behavior and be consistent regardless of the prefix. *)
    let negative = value.[0] = '-' in
    match int_of_string_opt value with
    | Some int when int < 0 && not negative -> `Int_raw value
    | Some int when int > 0 && negative -> `Int_raw value
    | Some int -> `Int int
    | None -> `Int_raw value
  }
  | value=FLOAT { `Decimal value }
  | TRUE { `Bool true }
  | FALSE { `Bool false }
  | NULL { `Null }
  | error { error $sloc "Expected a value" }
;

type_annot:
  | "(" typ=any_string ")" { typ }
  | "(" any_string _x=error { error $loc(_x) "Expected ')'" }
  | "(" _x=TRUE error { error $loc(_x) "A keyword is not a valid type annotation name" }
  | "(" _x=FALSE error { error $loc(_x) "A keyword is not a valid type annotation name" }
  | "(" _x=NULL error { error $loc(_x) "A keyword is not a valid type annotation name" }
  | "(" _x=INTEGER error { error $loc(_x) "A number is not a valid type annotation name" }
  | "(" _x=FLOAT error { error $loc(_x) "A number is not a valid type annotation name" }
  | "(" _x=error { error $loc(_x) "Expected a type annotation name" }
;

children:
  | "{" xs=children_nodes "}" { xs }
;
