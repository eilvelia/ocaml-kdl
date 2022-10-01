%{
open Ast

let[@inline] cons_if_some x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

let error loc msg = raise @@ Err.CustomParsingError (msg, loc)
%}

%token NEWLINE
%token SEMI ";"
%token <string> IDENT
%token <string> STRING
%token <string> RAW_STRING
%token <string> INTEGER
%token <float> FLOAT
%token TRUE
%token FALSE
%token NULL
%token DISABLE "/-"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token EQ "="
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

document: xs=nodes EOF { xs } ;

name:
  | id=IDENT { id }
  | str=STRING { str }
  | rstr=RAW_STRING { rstr }
;

nodes:
  | NEWLINE* { [] }
  | NEWLINE* x=node EOF { cons_if_some x [] }
  | NEWLINE* x=node ";" xs=nodes { cons_if_some x xs }
  | NEWLINE* x=node NEWLINE xs=nodes { cons_if_some x xs }
  | NEWLINE* node _x="}" error
    { error $loc(_x) "A node should be terminated with a semicolon or newline" }
;

node:
  | "/-" type_annot? name entity*
    { None }
  | annot=type_annot? name=name entities=entity* {
    let ws_after_annot = Option.is_some annot
      && Lexing.($endpos(annot).pos_cnum <> $startpos(name).pos_cnum) in
    if ws_after_annot then
      error ($endpos(annot), $startpos(name))
        "Whitespace after a type annotation is not allowed";
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
  | type_annot? _x=TRUE error { error $loc(_x) "'true' is not a valid node name" }
  | type_annot? _x=FALSE error { error $loc(_x) "'false' is not a valid node name" }
  | type_annot? _x=NULL error { error $loc(_x) "'null' is not a valid node name" }
  | type_annot? _x=INTEGER error { error $loc(_x) "A number is not a valid node name" }
  | type_annot? _x=FLOAT error { error $loc(_x) "A number is not a valid node name" }
  | type_annot _x=error { error $loc(_x) "Expected a node name" }
  | "/-" _x=error { error $loc(_x) "Expected a node after a '/-'"}
  | _x=")" error { error $loc(_x) "Unexpected token ')'" }
  | _x="=" error { error $loc(_x) "Unexpected token '='" }
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
  | key=name _eq="=" aval=annot_value {
    if Lexing.($endpos(key).pos_cnum <> $startpos(_eq).pos_cnum) then
      error ($endpos(key), $startpos(_eq)) "Whitespace before '=' is not allowed";
    if Lexing.($endpos(_eq).pos_cnum <> $startpos(aval).pos_cnum) then
      error ($endpos(_eq), $startpos(aval)) "Whitespace after '=' is not allowed";
    `Prop (key, aval)
  }
  | aval=annot_value { `Arg aval }
;

%inline annot_value:
  | annot=type_annot value=value {
    if Lexing.($endpos(annot).pos_cnum <> $startpos(value).pos_cnum) then
      error ($endpos(annot), $startpos(value))
        "Whitespace after a type annotation is not allowed";
    Some annot, value
  }
  | value=value { None, value }
;

value:
  | str=STRING { `String str }
  | str=RAW_STRING { `String str }
  | int_str=INTEGER {
    (* TODO: Even if the input is positive, int_of_string_opt can return a
       negative integer for 0x / 0o / 0b prefixes instead of using `RawInt as
       the fallback. We probably want to avoid this behavior and be consistent
       regardless of the prefix. *)
    match int_of_string_opt int_str with
    | Some int -> `Int int
    | None -> `RawInt int_str
  }
  | float=FLOAT { `Float float }
  | TRUE { `Bool true }
  | FALSE { `Bool false }
  | NULL { `Null }
  | _x=IDENT error { error $loc(_x) "Expected a value, got an identifier" }
  | error { error $sloc "Expected a value" }
;

type_annot:
  | "(" typ=name ")" { typ }
  | "(" name _x=error { error $loc(_x) "Expected a ')'" }
  | "(" _x=TRUE error { error $loc(_x) "'true' is not a valid type annotation name" }
  | "(" _x=FALSE error { error $loc(_x) "'false' is not a valid type annotation name" }
  | "(" _x=NULL error { error $loc(_x) "'null' is not a valid type annotation name" }
  | "(" _x=INTEGER error { error $loc(_x) "A number is not a valid type annotation name" }
  | "(" _x=FLOAT error { error $loc(_x) "A number is not a valid type annotation name" }
  | "(" _x=error { error $loc(_x) "Expected a type annotation name" }
;

children:
  | "{" xs=nodes "}" { xs }
  | "{" nodes error { error $sloc "Unterminated children block, expected a '}'" }
;
