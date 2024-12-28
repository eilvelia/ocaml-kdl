(* $ make generate-re2c *)
(* vim: set filetype=ocaml: *)

open Parser

let sprintf = Printf.sprintf

%{conditions %}

type 'a state = {
  info : 'a;
  refill : 'a state -> bool;
  mutable reached_eof : bool;
  mutable absolute_offset : int; (* offset of current yyinput *)
  mutable yyinput : bytes;
  mutable yylimit : int;
  mutable yystart : int; (* automaton start *)
  mutable yycursor : int;
  mutable yyaccept : int;
  mutable yymarker : int;
  mutable yycond : yycondtype;
  %{svars format = "\n  mutable @@{tag} : int;"; %}
  %{stags format = "\n  mutable @@{tag} : int;"; %}
}

type simple_state = unit state

let substr st l r = Bytes.sub_string st.yyinput l (r - l) [@@inline]

let make_state ~info ~refill ~limit input = {
  info;
  refill;
  reached_eof = false;
  absolute_offset = 0;
  yyinput = input;
  yylimit = limit;
  yycursor = 0;
  yystart = 0;
  yyaccept = 0;
  yymarker = 0;
  yycond = YYC_single;
  %{svars format = "\n  @@{tag} = 0;"; %}
  %{stags format = "\n  @@{tag} = 0;"; %}
}

let state_of_string ~info str =
  let refill st = st.reached_eof <- true; false in
  let limit = String.length str in
  make_state ~info ~refill ~limit (Bytes.unsafe_of_string str)

type tokenizer_info = {
  (* filename *)
  fname : string;

  (* below, *lnum and *bol positions are absolute, whereas token_cnum (similar
     to yystart and yycursor) is relative to the current buffer *)

  (* line number and beginning of line offset for yycursor *)
  mutable lnum : int;
  mutable bol : int;

  (* same, for yystart. these are needed only in case error is raised from
     a semantic action *)
  mutable start_lnum : int;
  mutable start_bol : int;

  (* this indicates start of a (more high-level) token we're lexing *)
  mutable token_cnum : int;
  mutable token_lnum : int;
  mutable token_bol : int;

  strbuf : Buffer.t;
}

let make_tokenizer_info ?(fname = "") () =
  {
    fname;
    lnum = 1;
    bol = 0;
    start_lnum = 1;
    start_bol = 0;
    token_cnum = 0;
    token_lnum = 1;
    token_bol = 0;
    strbuf = Buffer.create 32;
  }

type tokenizer_state = tokenizer_info state

let make_refiller f =
  let min_avail = 1024 in
  let refill st =
    (* Bytes before the token start can be discarded *)
    let start = st.info.token_cnum in
    if st.reached_eof then false else begin
      if Bytes.length st.yyinput - start >= min_avail then
        (* Shift the buffer *)
        Bytes.blit st.yyinput start st.yyinput 0 (st.yylimit - start)
      else begin
        (* Too long token, grow the buffer *)
        let newlen = Int.min (Bytes.length st.yyinput lsl 1) Sys.max_string_length in
        if newlen <= Bytes.length st.yyinput then failwith "Cannot grow buffer";
        let newbuf = Bytes.create newlen in
        Bytes.blit st.yyinput start newbuf 0 (st.yylimit - start);
        st.yyinput <- newbuf;
      end;
      (* Update positions *)
      st.yycursor <- st.yycursor - start;
      st.yymarker <- st.yymarker - start;
      st.yylimit <- st.yylimit - start;
      st.yystart <- st.yystart - start;
      st.info.token_cnum <- 0;
      st.absolute_offset <- st.absolute_offset + start;
      let read =
        let len = Bytes.length st.yyinput - st.yylimit in
        f st.yyinput ~offset:st.yylimit ~len
      in
      st.yylimit <- st.yylimit + read;
      if st.yylimit < Bytes.length st.yyinput then
        Bytes.unsafe_set st.yyinput st.yylimit '\x00';
      if read <= 0 then
        (st.reached_eof <- true; false)
      else true
    end
  in refill

let tokenizer_state_of_string ?fname str =
  state_of_string ~info:(make_tokenizer_info ?fname ()) str

let tokenizer_state_of_fun ?fname f =
  let len = 2048 in
  let refill = make_refiller f in
  let initial_buf = Bytes.create len in
  (* read initial chunk *)
  let limit = f initial_buf ~offset:0 ~len in
  if limit < len then Bytes.unsafe_set initial_buf limit '\x00';
  make_state ~info:(make_tokenizer_info ?fname ()) ~refill ~limit initial_buf

let tokenizer_state_of_channel ?fname ch =
  let f buf ~offset ~len = input ch buf offset len in
  tokenizer_state_of_fun ?fname f

let newline ?(pos = -2) st =
  let bol = if pos >= 0 then pos else st.yycursor in
  st.info.lnum <- st.info.lnum + 1;
  st.info.bol <- bol + st.absolute_offset

let save_start_position st =
  st.yystart <- st.yycursor;
  st.info.start_lnum <- st.info.lnum;
  st.info.start_bol <- st.info.bol
[@@inline]

let save_token_position st =
  st.info.token_cnum <- st.yycursor;
  st.info.token_lnum <- st.info.lnum;
  st.info.start_bol <- st.info.bol
[@@inline]

let make_lexing_token_pos st =
  Lexing.{
    pos_fname = st.info.fname;
    pos_lnum = st.info.token_lnum;
    pos_bol = st.info.token_bol;
    pos_cnum = st.info.token_cnum + st.absolute_offset;
  }

let make_lexing_start_pos st =
  Lexing.{
    pos_fname = st.info.fname;
    pos_lnum = st.info.start_lnum;
    pos_bol = st.info.start_bol;
    pos_cnum = st.yystart + st.absolute_offset;
  }

let make_lexing_end_pos st =
  Lexing.{
    pos_fname = st.info.fname;
    pos_lnum = st.info.lnum;
    pos_bol = st.info.bol;
    pos_cnum = st.yycursor + st.absolute_offset;
  }

let get_location st = make_lexing_token_pos st, make_lexing_end_pos st

let lexeme st = substr st st.yystart st.yycursor

let add_lexeme_to_buf st strbuf =
  let len = st.yycursor - st.yystart in
  Buffer.add_subbytes strbuf st.yyinput st.yystart len

let error st msg =
  let start_pos = make_lexing_start_pos st and end_pos = make_lexing_end_pos st in
  raise (Err.Custom_lexing_error (msg, (start_pos, end_pos)))

let malformed_utf8 st = error st "Malformed UTF-8"

(** Note: rollback can only be guaranteed to work if the previous data is still
    available, i.e., if we are still lexing the same token *)
let rollback_to_pos st (rollback_pos : Lexing.position) =
  let yycursor = rollback_pos.pos_cnum - st.absolute_offset in
  assert (yycursor >= 0);
  st.yycursor <- yycursor;
  st.info.bol <- rollback_pos.pos_bol;
  st.info.lnum <- rollback_pos.pos_lnum

(** Move yystart back to the start of the line *)
let rollback_start_to_newline st =
  st.yystart <- st.info.start_bol - st.absolute_offset

[@@@warning "-unused-var-strict"]

(* a bit dangerous, but disabled because of the conditionals codegen *)
[@@@warning "-partial-match"]

%{
  re2c:YYFILL = "st.refill st";
  re2c:YYPEEK = "Bytes.unsafe_get";
  re2c:encoding-policy = fail; // forbid surrogates
  re2c:encoding:utf8 = 1;
  re2c:eof = 0;
  re2c:indent:string = "  ";
  re2c:tags = 1;
  re2c:yyrecord = "st";

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

  // any character except newline; similar to . but with our newlines
  any = [^] \ newline_char;

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

%{local
  // Used in pretty-printing
  re2c:YYFN = ["is_valid_ident;bool", "st;simple_state"];

  "true" | "false" | "null" | "inf" | "-inf" | "nan" {
    not (st.yycursor >= st.yylimit)
  }
  sign? "." [0-9] { false }
  sign (startident identchar*)? | (startident \ sign) identchar* {
    st.yycursor >= st.yylimit
  }
  $ { false }
  * { false }
%}

%{local
  // Used in pretty-printing
  re2c:YYFN = ["escape_string_body;string", "st;simple_state", "strbuf;Buffer.t"];

  [\n] { Buffer.add_string strbuf "\\n"; escape_string st strbuf }
  [\r] { Buffer.add_string strbuf "\\r"; escape_string st strbuf }
  [\t] { Buffer.add_string strbuf "\\t"; escape_string st strbuf }
  [\\] { Buffer.add_string strbuf "\\\\"; escape_string st strbuf }
  "\"" { Buffer.add_string strbuf "\\\""; escape_string st strbuf }
  [\b] { Buffer.add_string strbuf "\\b"; escape_string st strbuf }
  [\f] { Buffer.add_string strbuf "\\f"; escape_string st strbuf }
  disallowed_char {
    let udecode = Bytes.get_utf_8_uchar st.yyinput st.yystart in
    if not (Uchar.utf_decode_is_valid udecode) then
      failwith "Malformed UTF-8";
    let code = Uchar.to_int (Uchar.utf_decode_uchar udecode) in
    Buffer.add_string strbuf (Printf.sprintf "\\u{%X}" code);
    escape_string st strbuf
  }
  $ { Buffer.contents strbuf }
  [^] {
    add_lexeme_to_buf st strbuf;
    escape_string st strbuf
  }
  * { failwith "Malformed UTF-8" }
%}

and escape_string st strbuf =
  st.yystart <- st.yycursor;
  escape_string_body st strbuf

let is_valid_ident str =
  is_valid_ident (state_of_string ~info:() str) [@@inline]

let escape_string = function
  | "" as empty -> empty
  | str -> escape_string (state_of_string ~info:() str) (Buffer.create 32)

(* Tokenizer *)

%{local
  re2c:YYFN = ["multiline_comment;unit", "st;tokenizer_state", "depth;int"];

  newline { newline st; multiline_comment st depth }
  "/*" { multiline_comment st (depth + 1) }
  "*/" { if depth <= 0 then () else multiline_comment st (depth - 1) }
  $ { error st "Unterminated comment" }
  any { multiline_comment st depth }
  * { malformed_utf8 st }
%}

%{local
  re2c:YYFN = ["singleline_comment;unit", "st;tokenizer_state"];

  newline { newline st }
  $ { () }
  any+ { singleline_comment st }
  * { malformed_utf8 st }
%}

%{local
  re2c:YYFN = ["line_cont_body;unit", "st;tokenizer_state"];

  newline { newline st }
  ws { line_cont st }
  "//" { singleline_comment st }
  "/*" { multiline_comment st 0; line_cont st }
  $ { () }
  any {
    error st @@
      sprintf "Illegal character '%s' after the '\\' line continuation" (lexeme st)
  }
  * { malformed_utf8 st }
%}

and line_cont st = save_start_position st; line_cont_body st

%{
  re2c:YYFN = ["whitespace_escape;unit", "st;tokenizer_state"];

  newline { newline st; whitespace_escape st }
  ws { whitespace_escape st }
  $ { () }
  "" / (any \ whitespace_char) { () }
  * { malformed_utf8 st }
%}

%{local
  re2c:YYFN = ["validate_multiline_start;unit", "st;tokenizer_state"];
  "" / newline { () }
  $ { error st "A multiline string must start with newline" }
  * { error st "A multiline string must start with newline" }
%}

let validate_multiline_start st =
  save_start_position st;
  validate_multiline_start st

%{local
  // This little time traveling oracle goes to the end of the string, finds
  // the whitespace prefix, and then rollbacks
  re2c:YYFN = ["detect_multiline_string_prefix_body;string", "st;tokenizer_state", "rollback_pos;Lexing.position"];

  [\\] ws { whitespace_escape st; detect_multiline_string_prefix st rollback_pos }
  [\\] newline {
    newline st;
    whitespace_escape st;
    detect_multiline_string_prefix st rollback_pos
  }
  [\\] ("\"" | "\\") { detect_multiline_string_prefix st rollback_pos }
  newline @t1 ws? @t2 ("\\" (ws | newline)+)? "\"\"\"" {
    rollback_to_pos st rollback_pos;
    substr st st.t1 st.t2
  }
  newline { newline st; detect_multiline_string_prefix st rollback_pos }
  // note: this may not fully show the prefix in the error location in case
  // of whitespace escapes
  "\"\"\"" {
    rollback_start_to_newline st;
    error st "Invalid multiline string: non-whitespace prefix"
  }
  $ { error st "Unterminated multiline string" }
  * { detect_multiline_string_prefix st rollback_pos }
%}

and detect_multiline_string_prefix st rollback_pos =
  save_start_position st;
  detect_multiline_string_prefix_body st rollback_pos

%{local
  // Same, but for raw strings
  re2c:YYFN = ["detect_raw_multiline_string_prefix_body;string",
               "st;tokenizer_state",
               "exp_hashlen;int",
               "rollback_pos;Lexing.position"];

  newline @t1 ws? @t2 "\"\"\"" @t3 [#]+ {
    if st.yycursor - st.t3 >= exp_hashlen then begin
      rollback_to_pos st rollback_pos;
      substr st st.t1 st.t2
    end else detect_raw_multiline_string_prefix st exp_hashlen rollback_pos
  }
  newline { newline st; detect_raw_multiline_string_prefix st exp_hashlen rollback_pos }
  "\"\"\"" @t1 [#]+ {
    if st.yycursor - st.t1 >= exp_hashlen then begin
      rollback_start_to_newline st;
      error st "Invalid multiline string: non-whitespace prefix"
    end else detect_raw_multiline_string_prefix st exp_hashlen rollback_pos
  }
  $ { error st "Unterminated raw multiline string" }
  * { detect_raw_multiline_string_prefix st exp_hashlen rollback_pos }
%}

and detect_raw_multiline_string_prefix st exp_hashlen rollback_pos =
  save_start_position st;
  detect_raw_multiline_string_prefix_body st exp_hashlen rollback_pos

let multiline_prefix_check st prefix =
  let { t1; t2; yyinput; _ } = st in
  newline ~pos:t1 st;
  let ws_len = t2 - t1 in
  for i = t1 to t1 + String.length prefix - 1 do
    let ch = Bytes.unsafe_get yyinput i in
    let prefix_i = i - t1 in
    if i >= t2 || ch != String.unsafe_get prefix prefix_i then
      error st "Invalid multiline string: unmatched whitespace prefix"
  done;
  Buffer.add_char st.info.strbuf '\n';
  let rest_len = ws_len - String.length prefix in
  let rest_start = t1 + String.length prefix in
  if rest_len > 0 then
    Buffer.add_subbytes st.info.strbuf yyinput rest_start rest_len

let multiline_contents strbuf =
  let len = Buffer.length strbuf in
  (* Strip the first and the last \n *)
  if len <= 1 then "" else Buffer.sub strbuf 1 (len - 2)

%{local
  re2c:YYFN = ["string_body;token", "st;tokenizer_state", "prefix;string"];

  <multi> newline @t1 ws? / newline {
    newline ~pos:st.t1 st;
    (* Any newline is normalized to \n *)
    Buffer.add_char st.info.strbuf '\n';
    (* Skip lines consisting of any whitespace only (do not check the prefix) *)
    yyfnmulti' st prefix
  }
  <multi> newline @t1 ws? @t2 / (any \ whitespace_char) {
    multiline_prefix_check st prefix;
    yyfnmulti' st prefix
  }
  <multi> newline { newline st; yyfnmulti' st prefix }
  <single> newline { newline st; error st "Unterminated string" }
  <single, multi> "\\n" { Buffer.add_char st.info.strbuf '\n'; string st prefix }
  <single, multi> "\\r" { Buffer.add_char st.info.strbuf '\r'; string st prefix }
  <single, multi> "\\t" { Buffer.add_char st.info.strbuf '\t'; string st prefix }
  <single, multi> "\\\\" { Buffer.add_char st.info.strbuf '\\'; string st prefix }
  <single, multi> "\\\"" { Buffer.add_char st.info.strbuf '"'; string st prefix }
  <single, multi> "\\b" { Buffer.add_char st.info.strbuf '\b'; string st prefix }
  <single, multi> "\\f" { Buffer.add_char st.info.strbuf '\012'; string st prefix }
  <single, multi> "\\s" { Buffer.add_char st.info.strbuf ' '; string st prefix }
  <single, multi> "\\u{" @t1 hex_digit+ @t2 '}' {
    let len = st.t2 - st.t1 in
    if len > 6 then
      error st "Invalid unicode scalar value (too many digits)";
    let code_str = substr st st.t1 st.t2 in
    let code = Scanf.sscanf code_str "%X%!" (fun x -> x) in
    if not @@ Uchar.is_valid code then
      error st "Invalid unicode scalar value";
    Buffer.add_utf_8_uchar st.info.strbuf (Uchar.unsafe_of_int code);
    string st prefix
  }
  <single, multi> [\\] ws { whitespace_escape st; string st prefix }
  <single, multi> [\\] newline {
    newline st;
    whitespace_escape st;
    string st prefix
  }
  <single, multi> [\\] [^] { error st "Invalid escape sequence" }
  <multi> "\"\"\"" { QUOTED_STRING (multiline_contents st.info.strbuf) }
  <single> "\"" { QUOTED_STRING (Buffer.contents st.info.strbuf) }
  <single, multi> disallowed_char { error st "Illegal character" }
  <single> any { add_lexeme_to_buf st st.info.strbuf; yyfnsingle' st prefix }
  <multi> any { add_lexeme_to_buf st st.info.strbuf; yyfnmulti' st prefix }
  <single, multi> $ { error st "Unterminated string" }
  <single, multi> * { malformed_utf8 st }
%}

and yyfnsingle' st prefix = save_start_position st; yyfnsingle st prefix
and yyfnmulti' st prefix = save_start_position st; yyfnmulti st prefix
and string st prefix = save_start_position st; string_body st prefix

let string_multiline st =
  validate_multiline_start st;
  let prefix = detect_multiline_string_prefix st (make_lexing_end_pos st) in
  st.yycond <- YYC_multi;
  string st prefix

let string_singleline st = st.yycond <- YYC_single; string st ""

let check_hashlen st exp_hashlen =
  let got_hashlen = st.yycursor - st.t1 in
  if got_hashlen > exp_hashlen then
    error st (sprintf "Expected %d hash symbol(s), got %d" exp_hashlen got_hashlen)
  else if got_hashlen < exp_hashlen then begin
    add_lexeme_to_buf st st.info.strbuf;
    false
  end else
    true

%{local
  re2c:YYFN = ["raw_string_body;token",
               "st;tokenizer_state",
               "exp_hashlen;int",
               "prefix;string"];

  <rmulti> newline @t1 ws? / newline {
    newline ~pos:st.t1 st;
    Buffer.add_char st.info.strbuf '\n';
    yyfnrmulti' st exp_hashlen prefix
  }
  <rmulti> newline @t1 ws? @t2 / (any \ whitespace_char) {
    multiline_prefix_check st prefix;
    yyfnrmulti' st exp_hashlen prefix
  }
  <rmulti> newline { newline st; yyfnrmulti' st exp_hashlen prefix }
  <rsingle> newline { newline st; error st "Unterminated raw string" }
  <rmulti> "\"\"\"" @t1 [#]+ {
    if check_hashlen st exp_hashlen then begin
      RAW_STRING (multiline_contents st.info.strbuf)
    end else yyfnrmulti' st exp_hashlen prefix
  }
  <rsingle> "\"" @t1 [#]+ {
    if check_hashlen st exp_hashlen then
      RAW_STRING (Buffer.contents st.info.strbuf)
    else yyfnrsingle' st exp_hashlen prefix
  }
  <rsingle, rmulti> disallowed_char { error st "Illegal character" }
  <rmulti> any { add_lexeme_to_buf st st.info.strbuf; yyfnrmulti' st exp_hashlen prefix }
  <rsingle> any { add_lexeme_to_buf st st.info.strbuf; yyfnrsingle' st exp_hashlen prefix }
  <rsingle, rmulti> $ { error st "Unterminated raw string" }
  <rsingle, rmulti> * { malformed_utf8 st }
%}

and yyfnrmulti' st a b = save_start_position st; yyfnrmulti st a b
and yyfnrsingle' st a b = save_start_position st; yyfnrsingle st a b
and raw_string st exp_hashlen prefix =
  save_start_position st;
  raw_string_body st exp_hashlen prefix

let raw_string_multiline st exp_hashlen =
  validate_multiline_start st;
  let prefix = detect_raw_multiline_string_prefix st exp_hashlen (make_lexing_end_pos st) in
  st.yycond <- YYC_rmulti;
  raw_string st exp_hashlen prefix

let raw_string_singleline st exp_hashlen =
  st.yycond <- YYC_rsingle;
  raw_string st exp_hashlen ""

%{local
  re2c:YYFN = ["main_body;token", "st;tokenizer_state"];

  ws { main st }
  newline { newline st; NEWLINE }
  "//" { singleline_comment st; NEWLINE }
  "/*" { multiline_comment st 0; main st }
  "\\" { line_cont st; main st }
  "/-" { SLASHDASH }
  ";" { SEMI }
  "(" { LPAREN }
  ")" { RPAREN }
  "{" { LBRACE }
  "}" { RBRACE }
  "=" { EQ }
  [\uFEFF] { BOM }
  "#" identchar+ {
    begin match lexeme st with
    | "#true" -> TRUE
    | "#false" -> FALSE
    | "#null" -> NULL
    | "#inf" -> FLOAT "inf"
    | "#-inf" -> FLOAT "-inf"
    | "#nan" -> FLOAT "nan"
    | k -> error st ("Unknown keyword " ^ k)
    end
  }
  integer { INTEGER (lexeme st) }
  float { FLOAT (lexeme st) }
  (integer | float) identchar+ {
    error st @@ sprintf "Invalid number literal %s" (lexeme st)
  }
  [#]+ @t1 "\"\"\"" {
    let hashlen = st.t1 - st.yystart in
    Buffer.reset st.info.strbuf;
    raw_string_multiline st hashlen
  }
  [#]+ @t1 "\"" {
    let hashlen = st.t1 - st.yystart in
    Buffer.reset st.info.strbuf;
    raw_string_singleline st hashlen
  }
  "\"\"\"" { Buffer.reset st.info.strbuf; string_multiline st }
  "\"" { Buffer.reset st.info.strbuf; string_singleline st }
  sign? "." [0-9] identchar* {
    error st "Number-like identifiers are invalid and must be quoted"
  }
  sign (startident identchar*)? | (startident \ sign) identchar* {
    IDENT_STRING (lexeme st)
  }
  $ { EOF }
  [^] { error st "Illegal character" }
  * { malformed_utf8 st }
%}

and main st =
  save_token_position st;
  save_start_position st;
  main_body st

let main_tokenizer st =
  let lexer () =
    let token = main st in
    let start_pos, end_pos = get_location st in
    token, start_pos, end_pos
  in
  lexer
