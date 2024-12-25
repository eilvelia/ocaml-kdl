(* $ make generate-re2c *)
(* vim: set filetype=ocaml: *)

open Parser

let sprintf = Printf.sprintf

(* NOTE: re2c can request a character one position greater than the end of
   the string ("the sentinel"). In OCaml, strings are always represented with
   \000 at the end, so to avoid copying, we can use
   [String.unsafe_get str (String.length str)], which should in fact be safe.
   The [pos <= String.length str] check is not done here; it is assumed that
   re2c generates correct code. *)
let get = String.unsafe_get

let error msg = raise @@ Err.Custom_lexing_error msg

let malformed_utf8 () = error "Malformed UTF-8"

type 'a yyrecord = {
  info : 'a;
  yyinput : string;
  yylimit : int;
  mutable yystart : int;
  mutable yycursor : int;
  mutable yyaccept : int;
  mutable yymarker : int;
  %{svars format = "\n  mutable @@{tag} : int;"; %}
  %{stags format = "\n  mutable @@{tag} : int;"; %}
}

type simple_yyrecord = unit yyrecord

let sub str l r = String.sub str l (r - l) [@@inline]

let group0 yyrecord = sub yyrecord.yyinput yyrecord.yytl0 yyrecord.yytr0
[@@inline]

(** Rollback to the start of an automaton *)
let rollback yyrecord = yyrecord.yycursor <- yyrecord.yystart [@@inline]

let make_yyrecord ?(offset = 0) ~info input = {
  info;
  yyinput = input;
  yylimit = String.length input;
  yycursor = offset;
  yystart = -1;
  yyaccept = 0;
  yymarker = 0;
  %{svars format = "\n  @@{tag} = 0;"; %}
  %{stags format = "\n  @@{tag} = 0;"; %}
}

type tokenizer_state = {
  fname : string;
  mutable lnum : int;
  mutable bol : int;
  mutable start_cnum : int; (* token start, not necessarily automaton start *)
  mutable start_bol : int;
  mutable start_lnum : int;
  strbuf : Buffer.t;
}

type tokenizer_yyrecord = tokenizer_state yyrecord

let make_tokenizer_yyrecord ?(fname = "") input =
  make_yyrecord ~info:{
    fname;
    lnum = 1;
    bol = 0;
    start_cnum = 0;
    start_bol = 0;
    start_lnum = 1;
    strbuf = Buffer.create 32;
  } input

let newline yyrecord =
  yyrecord.info.lnum <- yyrecord.info.lnum + 1;
  yyrecord.info.bol <- yyrecord.yycursor

let save_position yyrecord =
  yyrecord.info.start_cnum <- yyrecord.yycursor;
  yyrecord.info.start_bol <- yyrecord.info.bol;
  yyrecord.info.start_lnum <- yyrecord.info.lnum

let make_lexing_start_pos yyrecord =
  Lexing.{
    pos_fname = yyrecord.info.fname;
    pos_lnum = yyrecord.info.start_lnum;
    pos_bol = yyrecord.info.start_bol;
    pos_cnum = yyrecord.info.start_cnum;
  }

let make_lexing_end_pos yyrecord =
  Lexing.{
    pos_fname = yyrecord.info.fname;
    pos_lnum = yyrecord.info.lnum;
    pos_bol = yyrecord.info.bol;
    pos_cnum = yyrecord.yycursor;
  }

let get_location yyrecord =
  make_lexing_start_pos yyrecord, make_lexing_end_pos yyrecord

let lexeme yyrecord =
  sub yyrecord.yyinput yyrecord.yystart yyrecord.yycursor

let add_lexeme_substring yyrecord strbuf =
  let len = yyrecord.yycursor - yyrecord.yystart in
  Buffer.add_substring strbuf yyrecord.yyinput yyrecord.yystart len

(* no unused variables *)
[@@@warning "-27"]

%{
  re2c:encoding:utf8 = 1;
  re2c:encoding-policy = fail; // forbid surrogates
  re2c:yyfill:enable = 0;
  re2c:captvars = 1;
  re2c:invert-captures = 1;
  re2c:indent:string = "  ";
  re2c:eof = 0;

  whitespace_char =
      [\u0009] // Character Tabulation U+0009
    | [\u0020] // Space U+0020
    | [\u00A0] // No-Break-Space U+00A0
    | [\u1680] // Ogham Space Mark U+1680
    | [\u2000] // En Quad	U+2000
    | [\u2001] // Em Quad U+2001
    | [\u2002] // En Space U+2002
    | [\u2003] // Em Space U+2003
    | [\u2004] // Three-Per-Em Space U+2004
    | [\u2005] // Four-Per-Em Space U+2005
    | [\u2006] // Six-Per-Em Space U+2006
    | [\u2007] // Figure Space U+2007
    | [\u2008] // Punctuation Space U+2008
    | [\u2009] // Thin Space U+2009
    | [\u200A] // Hair Space U+200A
    | [\u202F] // Narrow No-Break Space U+202F
    | [\u205F] // Medium Mathematical Space U+205F
    | [\u3000] // Ideographic Space U+3000
  ;

  newline_char =
      [\r]     // CR   Carriage Return U+000D
    | [\n]     // LF   Line Feed U+000A
    | [\u0085] // NEL  Next Line U+0085
    | [\u000B] // VT   Vertical Tab U+000B
    | [\u000C] // FF   Form Feed U+000C
    | [\u2028] // LS   Line Separator U+2028
    | [\u2029] // PS   Paragraph Separator U+2029
  ;

  newline = "\r\n" | newline_char;
  ws = whitespace_char+;

  hex_digit = [0-9a-fA-F];

  sign = [+\-];

  decimal_nat = [0-9] [0-9_]*;

  dec = sign? decimal_nat;
  hex = sign? "0x" hex_digit (hex_digit | [_])*;
  oct = sign? "0o" [0-7] [0-7_]*;
  bin = sign? "0b" [01] [01_]*;
  integer = (dec | hex | oct | bin);

  exponent = [eE] sign? decimal_nat;
  float = sign? decimal_nat ("." decimal_nat exponent? | exponent);

  disallowed_char =
      [\x00-\x08\x0E-\x1F]
    | [\x7F] // Delete
    | [\u200E-\u200F\u202A-\u202E\u2066-\u2069] // Direction control
    | [\uFEFF] // BOM/ZWNBSP
  ;

  nonident_char =
      [(){}[\]/\\#;=] | "\""
    | disallowed_char
    | whitespace_char
    | newline_char
  ;
  identchar = [^] \ nonident_char;
  startident = identchar \ [0-9];
%}

let resolve_unicode_escape yyrecord =
  let len = yyrecord.yytr0 - yyrecord.yytl0 in
  if len > 6 then
    error "Invalid unicode scalar value";
  let code_str = group0 yyrecord in
  let code = int_of_string ("0x" ^ code_str) in
  if not @@ Uchar.is_valid code then
    error "Invalid unicode scalar value";
  Uchar.unsafe_of_int code

%{local
  re2c:YYFN = ["resolve_escapes_body;string", "yyrecord;simple_yyrecord", "strbuf;Buffer.t"];

  "\\n" { Buffer.add_char strbuf '\n'; resolve_escapes yyrecord strbuf }
  "\\r" { Buffer.add_char strbuf '\r'; resolve_escapes yyrecord strbuf }
  "\\t" { Buffer.add_char strbuf '\t'; resolve_escapes yyrecord strbuf }
  "\\\\" { Buffer.add_char strbuf '\\'; resolve_escapes yyrecord strbuf }
  "\\\"" { Buffer.add_char strbuf '"'; resolve_escapes yyrecord strbuf }
  "\\b" { Buffer.add_char strbuf '\b'; resolve_escapes yyrecord strbuf }
  "\\f" { Buffer.add_char strbuf '\012'; resolve_escapes yyrecord strbuf }
  "\\s" { Buffer.add_char strbuf ' '; resolve_escapes yyrecord strbuf }
  "\\u{" (!hex_digit+) "}" {
    let uchar = resolve_unicode_escape yyrecord in
    Buffer.add_utf_8_uchar strbuf uchar;
    resolve_escapes yyrecord strbuf
  }
  "\\" (newline | whitespace_char)+ { resolve_escapes yyrecord strbuf }
  "\\" [^] { error "Invalid escape sequence" }
  $ { Buffer.contents strbuf }
  [^] {
    add_lexeme_substring yyrecord strbuf;
    resolve_escapes yyrecord strbuf
  }
  * { malformed_utf8 () }
%}

and resolve_escapes yyrecord strbuf =
  yyrecord.yystart <- yyrecord.yycursor;
  resolve_escapes_body yyrecord strbuf

%{local
  re2c:YYFN = ["skip_whitespace_line;int", "yyrecord;simple_yyrecord"];

  ws { skip_whitespace_line yyrecord }
  newline { yyrecord.yycursor }
  $ { yyrecord.yycursor }
  * { ~-1 }
%}

%{local
  re2c:YYFN = ["is_fully_whitespace;bool", "yyrecord;simple_yyrecord"];

  ws { is_fully_whitespace yyrecord }
  $ { true }
  * { false }
%}

%{local
  // Used in pretty-printing
  re2c:YYFN = ["is_valid_ident;bool", "yyrecord;simple_yyrecord"];

  "true" | "false" | "null" | "inf" | "-inf" | "nan" {
    not (yyrecord.yycursor >= yyrecord.yylimit)
  }
  sign? "." [0-9] { false }
  sign (startident identchar*)? | (startident \ sign) identchar* {
    yyrecord.yycursor >= yyrecord.yylimit
  }
  $ { false }
  * { false }
%}

%{local
  // Used in pretty-printing
  re2c:YYFN = ["escape_string_body;string", "yyrecord;simple_yyrecord", "strbuf;Buffer.t"];

  [\n] { Buffer.add_string strbuf "\\n"; escape_string yyrecord strbuf }
  [\r] { Buffer.add_string strbuf "\\r"; escape_string yyrecord strbuf }
  [\t] { Buffer.add_string strbuf "\\t"; escape_string yyrecord strbuf }
  [\\] { Buffer.add_string strbuf "\\\\"; escape_string yyrecord strbuf }
  "\"" { Buffer.add_string strbuf "\\\""; escape_string yyrecord strbuf }
  [\b] { Buffer.add_string strbuf "\\b"; escape_string yyrecord strbuf }
  [\f] { Buffer.add_string strbuf "\\f"; escape_string yyrecord strbuf }
  disallowed_char {
    let udecode = String.get_utf_8_uchar yyrecord.yyinput yyrecord.yystart in
    if not (Uchar.utf_decode_is_valid udecode) then
      failwith "Malformed UTF-8";
    let code = Uchar.to_int (Uchar.utf_decode_uchar udecode) in
    Buffer.add_string strbuf (Printf.sprintf "\\u{%X}" code);
    escape_string yyrecord strbuf
  }
  $ { Buffer.contents strbuf }
  [^] {
    add_lexeme_substring yyrecord strbuf;
    escape_string yyrecord strbuf
  }
  * { malformed_utf8 () }
%}

and escape_string yyrecord strbuf =
  yyrecord.yystart <- yyrecord.yycursor;
  escape_string_body yyrecord strbuf

let resolve_escapes str =
  resolve_escapes (make_yyrecord ~info:() str) (Buffer.create 32)

let skip_whitespace_line ?(offset = 0) str =
  skip_whitespace_line (make_yyrecord ~offset ~info:() str)

let is_fully_whitespace = function
  | "" -> true
  | str -> is_fully_whitespace (make_yyrecord ~info:() str)

let is_valid_ident str = is_valid_ident (make_yyrecord ~info:() str)
[@@inline]

let escape_string = function
  | "" as empty -> empty
  | str -> escape_string (make_yyrecord ~info:() str) (Buffer.create 32)

let[@inline] check_hashlen ~exp (got : int) =
  if got > exp then
    error (sprintf "Expected %d hash symbol(s), got %d" exp got)
  else got = exp

let dedent str =
  let strlen = String.length str in
  if strlen < 1 || str.[0] <> '\n' then
    error "A multiline string must start with newline";
  let prefix =
    let prefix_start = String.rindex str '\n' + 1 in
    String.sub str prefix_start (strlen - prefix_start)
  in
  let prefix_len = String.length prefix in
  if not (is_fully_whitespace prefix) then
    error "Invalid multiline string: non-whitespace prefix";
  let result = Buffer.create 32 in
  (* Skip the first newline *)
  let i = ref 1 in
  let line_start = ref 1 in
  (* Exclude prefix and the last newline *)
  let limit = strlen - prefix_len - 1 in
  while !i < limit do
    let ch = String.unsafe_get str !i in
    let line_i = !i - !line_start in
    let old_i = !i in
    if line_i = 0 then begin
      (* If the line fully consists of any whitespace, we skip the contents
         of it without prefix checks *)
      let next_line_start = skip_whitespace_line ~offset:!i str in
      (* -1 if the line contains content *)
      if next_line_start >= 0 then begin
        i := next_line_start;
        line_start := next_line_start;
        if next_line_start - 1 < limit then
          Buffer.add_char result '\n'
      end
    end;
    if !i = old_i then begin
      if line_i < prefix_len then begin
        (* Validating whitespace prefix *)
        if ch <> String.unsafe_get prefix line_i then
          error "Invalid multiline string: unmatched whitespace prefix"
      end else begin
        Buffer.add_char result ch;
        if ch = '\n' then
          line_start := !i + 1;
      end;
      incr i
    end;
  done;
  Buffer.contents result

%{local
  re2c:YYFN = ["multiline_comment;unit", "yyrecord;tokenizer_yyrecord", "depth;int"];

  newline { newline yyrecord; multiline_comment yyrecord depth }
  "/*" { multiline_comment yyrecord (depth + 1) }
  "*/" { if depth <= 0 then () else multiline_comment yyrecord (depth - 1) }
  $ { error "Unterminated comment" }
  [^] { multiline_comment yyrecord depth }
  * { malformed_utf8 () }
%}

%{local
  re2c:YYFN = ["singleline_comment;unit", "yyrecord;tokenizer_yyrecord"];

  newline { newline yyrecord }
  $ { () }
  ([^] \ newline_char)+ { singleline_comment yyrecord }
  * { malformed_utf8 () }
%}

%{local
  re2c:YYFN = ["line_cont_body;unit", "yyrecord;tokenizer_yyrecord"];

  newline { newline yyrecord }
  ws { line_cont yyrecord }
  "//" { singleline_comment yyrecord }
  "/*" { multiline_comment yyrecord 0; line_cont yyrecord }
  $ { () }
  [^] {
    error @@ sprintf "Illegal character '%s' after the '\\' line continuation"
      (lexeme yyrecord)
  }
  * { malformed_utf8 () }
%}

and line_cont yyrecord =
  yyrecord.yystart <- yyrecord.yycursor;
  line_cont_body yyrecord

%{local
  re2c:YYFN = ["raw_string_body;token", "yyrecord;tokenizer_yyrecord", "exp_hashlen;int"];

  newline { newline yyrecord; error "Unterminated raw string" }
  "\"" (![#]+) {
    let hashlen = yyrecord.yytr0 - yyrecord.yytl0 in
    if check_hashlen ~exp:exp_hashlen hashlen then
      RAW_STRING (Buffer.contents yyrecord.info.strbuf)
    else begin
      Buffer.add_string yyrecord.info.strbuf (lexeme yyrecord);
      raw_string yyrecord exp_hashlen
    end
  }
  disallowed_char { error "Illegal character" }
  $ { error "Unterminated raw string" }
  [^] {
    add_lexeme_substring yyrecord yyrecord.info.strbuf;
    raw_string yyrecord exp_hashlen
  }
  * { malformed_utf8 () }
%}

and raw_string yyrecord exp_hashlen =
  yyrecord.yystart <- yyrecord.yycursor;
  raw_string_body yyrecord exp_hashlen

%{local
  re2c:YYFN = ["raw_string_multiline_body;token", "yyrecord;tokenizer_yyrecord", "exp_hashlen;int"];

  newline {
    newline yyrecord;
    (* Any newline is normalized to \n *)
    Buffer.add_char yyrecord.info.strbuf '\n';
    raw_string_multiline yyrecord exp_hashlen
  }
  "\"\"\"" (![#]+) {
    let hashlen = yyrecord.yytr0 - yyrecord.yytl0 in
    if check_hashlen ~exp:exp_hashlen hashlen then
      RAW_STRING (dedent (Buffer.contents yyrecord.info.strbuf))
    else begin
      Buffer.add_string yyrecord.info.strbuf (lexeme yyrecord);
      raw_string_multiline yyrecord exp_hashlen
    end
  }
  disallowed_char { error "Illegal character" }
  $ { error "Unterminated multiline raw string" }
  [^] {
    add_lexeme_substring yyrecord yyrecord.info.strbuf;
    raw_string_multiline yyrecord exp_hashlen
  }
  * { malformed_utf8 () }
%}

and raw_string_multiline yyrecord exp_hashlen =
  yyrecord.yystart <- yyrecord.yycursor;
  raw_string_multiline_body yyrecord exp_hashlen

%{
  re2c:YYFN = ["whitespace_escape_body;unit", "yyrecord;tokenizer_yyrecord"];

  newline { newline yyrecord; whitespace_escape yyrecord }
  ws { whitespace_escape yyrecord }
  $ { () }
  * { rollback yyrecord }
%}

and whitespace_escape yyrecord =
  yyrecord.yystart <- yyrecord.yycursor;
  whitespace_escape_body yyrecord

%{
  re2c:YYFN = ["string_body;token", "yyrecord;tokenizer_yyrecord"];

  newline { newline yyrecord; error "Unterminated string" }
  "\\n" { Buffer.add_char yyrecord.info.strbuf '\n'; string yyrecord }
  "\\r" { Buffer.add_char yyrecord.info.strbuf '\r'; string yyrecord }
  "\\t" { Buffer.add_char yyrecord.info.strbuf '\t'; string yyrecord }
  "\\\\" { Buffer.add_char yyrecord.info.strbuf '\\'; string yyrecord }
  "\\\"" { Buffer.add_char yyrecord.info.strbuf '"'; string yyrecord }
  "\\b" { Buffer.add_char yyrecord.info.strbuf '\b'; string yyrecord }
  "\\f" { Buffer.add_char yyrecord.info.strbuf '\012'; string yyrecord }
  "\\s" { Buffer.add_char yyrecord.info.strbuf ' '; string yyrecord }
  "\\u{" (!hex_digit+) '}' {
    let uchar = resolve_unicode_escape yyrecord in
    Buffer.add_utf_8_uchar yyrecord.info.strbuf uchar;
    string yyrecord
  }
  '\\' ws { whitespace_escape yyrecord; string yyrecord }
  '\\' newline {
    newline yyrecord;
    whitespace_escape yyrecord;
    string yyrecord
  }
  '\\' [^] { error "Invalid escape sequence" }
  '"' { QUOTED_STRING (Buffer.contents yyrecord.info.strbuf) }
  disallowed_char { error "Illegal character" }
  [^] { add_lexeme_substring yyrecord yyrecord.info.strbuf; string yyrecord }
  $ { error "Unterminated string" }
  * { malformed_utf8 () }
%}

and string yyrecord =
  yyrecord.yystart <- yyrecord.yycursor;
  string_body yyrecord

%{local
  re2c:YYFN = ["string_multiline_body;token", "yyrecord;tokenizer_yyrecord"];

  newline {
    newline yyrecord;
    (* Any newline is normalized to \n *)
    Buffer.add_char yyrecord.info.strbuf '\n';
    string_multiline yyrecord
  }
  "\\\\" { Buffer.add_string yyrecord.info.strbuf "\\\\"; string_multiline yyrecord }
  "\\\"" { Buffer.add_string yyrecord.info.strbuf "\\\""; string_multiline yyrecord }
  '\\' ws { whitespace_escape yyrecord; string_multiline yyrecord }
  '\\' newline {
    newline yyrecord;
    whitespace_escape yyrecord;
    string_multiline yyrecord
  }
  "\"\"\"" {
    QUOTED_STRING (resolve_escapes (dedent (Buffer.contents yyrecord.info.strbuf)))
  }
  disallowed_char { error "Illegal character" }
  $ { error "Unterminated multiline string" }
  [^] {
    add_lexeme_substring yyrecord yyrecord.info.strbuf;
    string_multiline yyrecord
  }
  * { malformed_utf8 () }
%}

and string_multiline yyrecord =
  yyrecord.yystart <- yyrecord.yycursor;
  string_multiline_body yyrecord

%{local
  re2c:YYFN = ["main_body;token", "yyrecord;tokenizer_yyrecord"];

  ws { main yyrecord }
  newline { newline yyrecord; NEWLINE }
  "//" { singleline_comment yyrecord; NEWLINE }
  "/*" { multiline_comment yyrecord 0; main yyrecord }
  "\\" { line_cont yyrecord; main yyrecord }
  "/-" { SLASHDASH }
  ";" { SEMI }
  "(" { LPAREN }
  ")" { RPAREN }
  "{" { LBRACE }
  "}" { RBRACE }
  "=" { EQ }
  [\uFEFF] { BOM }
  "#" identchar+ {
    begin match lexeme yyrecord with
    | "#true" -> TRUE
    | "#false" -> FALSE
    | "#null" -> NULL
    | "#inf" -> FLOAT "inf"
    | "#-inf" -> FLOAT "-inf"
    | "#nan" -> FLOAT "nan"
    | k -> error ("Unknown keyword " ^ k)
    end
  }
  integer { INTEGER (lexeme yyrecord) }
  float { FLOAT (lexeme yyrecord) }
  (integer | float) identchar+ {
    error @@ sprintf "Invalid number literal %s" (lexeme yyrecord)
  }
  (![#]+) "\"\"\"" {
    let hashlen = yyrecord.yytr0 - yyrecord.yytl0 in
    Buffer.reset yyrecord.info.strbuf;
    raw_string_multiline yyrecord hashlen
  }
  (![#]+) "\"" {
    let hashlen = yyrecord.yytr0 - yyrecord.yytl0 in
    Buffer.reset yyrecord.info.strbuf;
    raw_string yyrecord hashlen
  }
  "\"\"\"" { Buffer.reset yyrecord.info.strbuf; string_multiline yyrecord }
  "\"" { Buffer.reset yyrecord.info.strbuf; string yyrecord }
  sign? "." [0-9] identchar* {
    error "Number-like identifiers are invalid and must be quoted"
  }
  sign (startident identchar*)? | (startident \ sign) identchar* {
    IDENT_STRING (lexeme yyrecord)
  }
  $ { EOF }
  [^] { error "Illegal character" }
  * { malformed_utf8 () }
%}

and main (yyrecord : tokenizer_yyrecord) =
  save_position yyrecord;
  yyrecord.yystart <- yyrecord.yycursor;
  main_body yyrecord

let main_tokenizer yyrecord =
  let lexer () =
    let token = main yyrecord in
    let start_pos, end_pos = get_location yyrecord in
    token, start_pos, end_pos
  in
  lexer
