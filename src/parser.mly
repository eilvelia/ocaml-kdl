%{
open Ast

let cons_if_some x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

let error loc msg = raise @@ Err.Custom_parsing_error (msg, loc)
%}

%token NEWLINE
%token BOM
%token SEMI ";"
%token <string> IDENT_STRING
%token <string> QUOTED_STRING
%token <string> RAW_STRING
%token <string> INTEGER
%token <string> FLOAT
%token TRUE "#true"
%token FALSE "#false"
%token NULL "#null"
%token SLASHDASH "/-"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token EQ "="
%token EOF

%start <Ast.t> document
%%

document: BOM? xs=nodes EOF { xs } ;

%inline any_string:
  | id=IDENT_STRING {
    match id with
    | "inf" | "-inf" | "nan" | "true" | "false" | "null" ->
      error $loc(id) @@
        Printf.sprintf
          "%s is not a valid identifier. Did you mean \"%s\" (quoted) or #%s?"
          id id id
    | _ -> id
  }
  | str=QUOTED_STRING { str }
  | rstr=RAW_STRING { rstr }
;

(* Used in error reporting *)
%inline keyword: x="#true" { x } | x="#false" { x } | x="#null" { x } ;

nodes:
  | NEWLINE* { [] }
  | NEWLINE* x=node NEWLINE xs=nodes { cons_if_some x xs }
  | NEWLINE* x=node ";" xs=nodes { cons_if_some x xs }
  | NEWLINE* x=node { cons_if_some x [] }
;

node:
  | "/-" NEWLINE* type_annot? any_string entity*
    { None }
  | annot=type_annot? name=any_string entities=entity* {
    (* Require at least one whitespace between the node name and 1st entity *)
    begin match entities with
    | [] -> ()
    | (entity1_loc, _) :: _ ->
      let start_pos = $endpos(name) and end_pos = fst entity1_loc in
      if Lexing.(start_pos.pos_cnum >= end_pos.pos_cnum) then
        error (start_pos, end_pos) "Expected a whitespace delimiter";
    end;
    (* Require at least one whitespace between entities *)
    let rec check_whitespace = function
      | [] | _ :: [] -> ()
      | (entity1_loc, _) :: ((entity2_loc, _) :: _ as rest) ->
        let start_pos = snd entity1_loc and end_pos = fst entity2_loc in
        if Lexing.(start_pos.pos_cnum >= end_pos.pos_cnum) then
          error (start_pos, end_pos) "Expected a whitespace delimiter";
        check_whitespace rest
    in
    check_whitespace entities;
    let known_props : (string, prop) Hashtbl.t = Hashtbl.create 4 in
    let args = ref [] and props = ref [] and children = ref [] in
    let meet_children = ref false in
    let meet_slashdashed_children = ref false in
    List.iter (fun entity ->
      let loc, entity = entity in
      match entity, !meet_children, !meet_slashdashed_children with
      | `Slashdashed_prop_or_arg, false, false -> ()
      | `Slashdashed_children, _, _ -> meet_slashdashed_children := true;
      | (`Prop_or_arg _ | `Slashdashed_prop_or_arg), true, _
      | (`Prop_or_arg _ | `Slashdashed_prop_or_arg), _, true ->
        error loc "A property or argument cannot be defined after a children block"
      | `Children _, true, _ ->
        error loc "A children block cannot be defined twice"
      | `Children c, false, _ ->
        meet_children := true;
        children := c
      | `Prop_or_arg x, false, false ->
        match x with
        | `Arg a ->
          args := a :: !args
        (* This is not optimal, but should be fine for most cases.
           Every prop is checked against the known_props hash table, this
           operates in O(1) on average. If the prop has been already defined,
           it is removed from the list in O(n) time. It is assumed that
           redefinitions of properties should be rare. *)
        | `Prop (name, _ as p) when Hashtbl.mem known_props name ->
          props := p :: List.filter (fun (n, _) -> n <> name) !props
        | `Prop (name, _ as p) ->
          Hashtbl.add known_props name p;
          props := p :: !props
    ) entities;
    let args = List.rev !args and props = List.rev !props in
    let children = !children in
    Some { name; annot; args; props; children }
  }
  | type_annot? _x=keyword error { error $loc(_x) "A keyword is not a valid node name" }
  | type_annot? _x=INTEGER error { error $loc(_x) "A number is not a valid node name" }
  | type_annot? _x=FLOAT error { error $loc(_x) "A number is not a valid node name" }
  | type_annot _x=error { error $loc(_x) "Expected a node name" }
  | "/-" NEWLINE* _x=error { error $loc(_x) "Expected a node after '/-'"}
  | _x=")" error { error $loc(_x) "Unexpected token ')'" }
  | _x="=" error { error $loc(_x) "Unexpected token '='" }
;

entity:
  | "/-" NEWLINE* prop_or_arg { $sloc, `Slashdashed_prop_or_arg }
  | "/-" NEWLINE* children { $sloc, `Slashdashed_children }
  | x=prop_or_arg { $sloc, `Prop_or_arg x }
  | x=children { $sloc, `Children x }
  | "/-" NEWLINE* _x="}" error { error $loc(_x) "Unexpected token '}'" }
;

prop_or_arg:
  (* (requires lookahead to the = token to differentiate from string values) *)
  | key=any_string "=" aval=annot_value { `Prop (key, aval) }
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
       this behavior and be consistent regardless of the prefix. This uses
       signed representation only. *)
    match signed_int_of_string_opt value with
    | Some x -> `Int x
    | None -> `Int_raw value
  }
  | value=FLOAT { `Float_raw value }
  | "#true" { `Bool true }
  | "#false" { `Bool false }
  | "#null" { `Null }
  | error { error $sloc "Expected a value" }
;

type_annot:
  | "(" typ=any_string ")" { typ }
  | "(" any_string _x=error { error $loc(_x) "Expected ')'" }
  | "(" _x=keyword error { error $loc(_x) "A keyword is not a valid type annotation name" }
  | "(" _x=INTEGER error { error $loc(_x) "A number is not a valid type annotation name" }
  | "(" _x=FLOAT error { error $loc(_x) "A number is not a valid type annotation name" }
  | "(" _x=error { error $loc(_x) "Expected a type annotation name" }
;

children: "{" xs=nodes "}" { xs } ;
