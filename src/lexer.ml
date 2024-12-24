open Parser
open Printf

let error msg = raise @@ Err.Custom_lexing_error msg

let whitespace_char = [%sedlex.regexp?
    '\t'   (* Character Tabulation U+0009 *)
  | ' '    (* Space U+0020 *)
  | 0x00A0 (* No-Break-Space U+00A0 *)
  | 0x1680 (* Ogham Space Mark U+1680 *)
  | 0x2000 (* En Quad	U+2000 *)
  | 0x2001 (* Em Quad U+2001 *)
  | 0x2002 (* En Space U+2002 *)
  | 0x2003 (* Em Space U+2003 *)
  | 0x2004 (* Three-Per-Em Space U+2004 *)
  | 0x2005 (* Four-Per-Em Space U+2005 *)
  | 0x2006 (* Six-Per-Em Space U+2006 *)
  | 0x2007 (* Figure Space U+2007 *)
  | 0x2008 (* Punctuation Space U+2008 *)
  | 0x2009 (* Thin Space U+2009 *)
  | 0x200A (* Hair Space U+200A *)
  | 0x202F (* Narrow No-Break Space U+202F *)
  | 0x205F (* Medium Mathematical Space U+205F *)
  | 0x3000 (* Ideographic Space U+3000 *)
]

let ws = [%sedlex.regexp? Plus whitespace_char]

let newline_char = [%sedlex.regexp?
    '\r'   (* CR   Carriage Return U+000D *)
  | '\n'   (* LF   Line Feed U+000A *)
  | 0x0085 (* NEL  Next Line U+0085 *)
  | 0x000B (* VT   Vertical Tab U+000B *)
  | 0x000C (* FF   Form Feed U+000C *)
  | 0x2028 (* LS   Line Separator U+2028 *)
  | 0x2029 (* PS   Paragraph Separator U+2029 *)
]

let newline = [%sedlex.regexp? "\r\n" | newline_char]

let hex_digit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']

let sign = [%sedlex.regexp? Chars "-+"]

let decimal_nat = [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_')]
let exponent = [%sedlex.regexp? Chars "eE", Opt sign, decimal_nat]

let dec = [%sedlex.regexp? Opt sign, decimal_nat]
let hex = [%sedlex.regexp? Opt sign, "0x", hex_digit, Star (hex_digit | '_')]
let octal = [%sedlex.regexp? Opt sign, "0o", '0'..'7', Star ('0'..'7' | '_')]
let binary = [%sedlex.regexp? Opt sign, "0b", Chars "01", Star (Chars "01_")]

let integer = [%sedlex.regexp? dec | hex | octal | binary]

let float = [%sedlex.regexp?
  Opt sign, decimal_nat, ('.', decimal_nat, Opt exponent
                         | exponent) ]

let disallowed_char = [%sedlex.regexp?
    0 .. 0x8 | 0x0E .. 0x1F
  | 0x7F (* Delete *)
  (* Direction control *)
  | 0x200E .. 0x200F | 0x202A .. 0x202E | 0x2066 .. 0x2069
  | 0xFEFF (* BOM/ZWNBSP *)
]

let nonident_char = [%sedlex.regexp? Chars {|(){}[]/\"#;=|}
                                     | disallowed_char
                                     | whitespace_char
                                     | newline_char ]
let identchar = [%sedlex.regexp? Sub (any, nonident_char)]
let startident = [%sedlex.regexp? Sub (identchar, '0'..'9')]

let[@inline] new_line lexbuf =
  (* Add newline to the position info if the lexeme doesn't end with \n *)
  let u = Sedlexing.lexeme_char lexbuf (Sedlexing.lexeme_length lexbuf - 1) in
  if Uchar.to_int u <> Char.code '\n' then
    Sedlexing.new_line lexbuf

let string_start_pos = ref Lexing.dummy_pos

let[@inline] set_string_start lexbuf =
  let start_pos, _ = Sedlexing.lexing_bytes_positions lexbuf in
  string_start_pos := start_pos

let rec comment depth lexbuf =
  match%sedlex lexbuf with
  | newline -> new_line lexbuf; comment depth lexbuf
  | "/*" -> comment (depth + 1) lexbuf
  | "*/" -> if depth <= 0 then () else comment (depth - 1) lexbuf
  | eof -> error "Unterminated comment"
  | any -> comment depth lexbuf
  | _ -> assert false

let rec line_comment lexbuf =
  match%sedlex lexbuf with
  | newline -> new_line lexbuf
  | eof -> ()
  | any -> line_comment lexbuf
  | _ -> assert false

let rec whitespace_escape lexbuf =
  match%sedlex lexbuf with
  | newline -> new_line lexbuf; whitespace_escape lexbuf
  | ws -> whitespace_escape lexbuf
  | _ -> ()

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
  if not (Re2c_lexer.is_fully_whitespace prefix) then
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
      let next_line_start = Re2c_lexer.skip_whitespace_line ~offset:!i str in
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

let rec raw_string exp_hashlen strbuf lexbuf =
  match%sedlex lexbuf with
  | newline ->
    new_line lexbuf;
    error "Unterminated raw string"
  | '"', Plus '#' ->
    let hashlen = Sedlexing.lexeme_length lexbuf - 1 in
    if check_hashlen ~exp:exp_hashlen hashlen then
      RAW_STRING (Buffer.contents strbuf)
    else begin
      Buffer.add_string strbuf (Sedlexing.Utf8.lexeme lexbuf);
      raw_string exp_hashlen strbuf lexbuf
    end
  | disallowed_char -> error "Illegal character"
  | eof -> error "Unterminated raw string"
  | any ->
    Buffer.add_string strbuf (Sedlexing.Utf8.lexeme lexbuf);
    raw_string exp_hashlen strbuf lexbuf
  | _ -> assert false

let rec raw_string_multiline exp_hashlen strbuf lexbuf =
  match%sedlex lexbuf with
  | newline ->
    new_line lexbuf;
    (* note: any newline is normalized to \n *)
    Buffer.add_char strbuf '\n';
    raw_string_multiline exp_hashlen strbuf lexbuf
  | {|"""|}, Plus '#' ->
    let hashlen = Sedlexing.lexeme_length lexbuf - 3 in
    if check_hashlen ~exp:exp_hashlen hashlen then
      RAW_STRING (dedent (Buffer.contents strbuf))
    else begin
      Buffer.add_string strbuf (Sedlexing.Utf8.lexeme lexbuf);
      raw_string_multiline exp_hashlen strbuf lexbuf
    end
  | disallowed_char -> error "Illegal character"
  | eof -> error "Unterminated multiline raw string"
  | any ->
    Buffer.add_string strbuf (Sedlexing.Utf8.lexeme lexbuf);
    raw_string_multiline exp_hashlen strbuf lexbuf
  | _ -> assert false

let rec string_multiline strbuf lexbuf =
  match%sedlex lexbuf with
  | newline ->
    new_line lexbuf;
    Buffer.add_char strbuf '\n';
    string_multiline strbuf lexbuf
  | {|\\|} -> Buffer.add_string strbuf {|\\|}; string_multiline strbuf lexbuf
  | {|\"|} -> Buffer.add_string strbuf {|\"|}; string_multiline strbuf lexbuf
  | '\\', ws -> whitespace_escape lexbuf; string_multiline strbuf lexbuf
  | '\\', newline ->
    new_line lexbuf;
    whitespace_escape lexbuf;
    string_multiline strbuf lexbuf
  | {|"""|} ->
    QUOTED_STRING (Re2c_lexer.resolve_escapes (dedent (Buffer.contents strbuf)))
  | disallowed_char -> error "Illegal character"
  | eof -> error "Unterminated multiline string"
  | any ->
    Buffer.add_string strbuf (Sedlexing.Utf8.lexeme lexbuf);
    string_multiline strbuf lexbuf
  | _ -> assert false

let rec string strbuf lexbuf =
  match%sedlex lexbuf with
  | newline ->
    new_line lexbuf;
    error "Unterminated string"
  | "\\n" -> Buffer.add_char strbuf '\n'; string strbuf lexbuf
  | "\\r" -> Buffer.add_char strbuf '\r'; string strbuf lexbuf
  | "\\t" -> Buffer.add_char strbuf '\t'; string strbuf lexbuf
  | "\\\\" -> Buffer.add_char strbuf '\\'; string strbuf lexbuf
  | "\\\"" -> Buffer.add_char strbuf '"'; string strbuf lexbuf
  | "\\b" -> Buffer.add_char strbuf '\b'; string strbuf lexbuf
  | "\\f" -> Buffer.add_char strbuf '\012'; string strbuf lexbuf
  | "\\s" -> Buffer.add_char strbuf ' '; string strbuf lexbuf
  | "\\u{", Plus hex_digit, '}' ->
    let code_str =
      Sedlexing.Utf8.sub_lexeme lexbuf 3 (Sedlexing.lexeme_length lexbuf - 4) in
    if String.length code_str > 6 then
      error "Invalid unicode scalar value";
    let code = int_of_string @@ "0x" ^ code_str in
    if not @@ Uchar.is_valid code then
      error "Invalid unicode scalar value";
    Buffer.add_utf_8_uchar strbuf (Uchar.unsafe_of_int code);
    string strbuf lexbuf
  | '\\', ws -> whitespace_escape lexbuf; string strbuf lexbuf
  | '\\', newline ->
    new_line lexbuf;
    whitespace_escape lexbuf;
    string strbuf lexbuf
  | '\\', any -> error "Invalid escape sequence"
  | '"' ->
    QUOTED_STRING (Buffer.contents strbuf)
  | disallowed_char -> error "Illegal character"
  | eof -> error "Unterminated string"
  | any ->
    Buffer.add_string strbuf (Sedlexing.Utf8.lexeme lexbuf);
    string strbuf lexbuf
  | _ -> assert false

let rec line_cont lexbuf =
  match%sedlex lexbuf with
  | ws -> line_cont lexbuf
  | newline -> new_line lexbuf
  | "//" -> line_comment lexbuf
  | "/*" -> comment 0 lexbuf; line_cont lexbuf
  | eof -> ()
  | any -> error
    @@ sprintf "Illegal character '%s' after the '\\' line continuation"
      (Sedlexing.Utf8.lexeme lexbuf)
  | _ -> assert false

let rec main lexbuf =
  match%sedlex lexbuf with
  | ws -> main lexbuf
  | newline -> new_line lexbuf; NEWLINE
  | "//" -> line_comment lexbuf; NEWLINE
  | "/*" -> comment 0 lexbuf; main lexbuf
  | '\\' -> line_cont lexbuf; main lexbuf
  | "/-" -> SLASHDASH
  | ';' -> SEMI
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '=' -> EQ
  | '#', Plus identchar ->
    begin match Sedlexing.Utf8.lexeme lexbuf with
    | "#true" -> TRUE
    | "#false" -> FALSE
    | "#null" -> NULL
    | "#inf" -> FLOAT "inf"
    | "#-inf" -> FLOAT "-inf"
    | "#nan" -> FLOAT "nan"
    | k -> error ("Unknown keyword " ^ k)
    end
  | integer -> INTEGER (Sedlexing.Utf8.lexeme lexbuf)
  | float -> FLOAT (Sedlexing.Utf8.lexeme lexbuf)
  | (integer | float), Plus identchar ->
    error @@ sprintf "Invalid number literal %s" (Sedlexing.Utf8.lexeme lexbuf)
  | Plus '#', {|"""|} ->
    let hashlen = Sedlexing.lexeme_length lexbuf - 3 in
    set_string_start lexbuf;
    raw_string_multiline hashlen (Buffer.create 32) lexbuf
  | Plus '#', '"' ->
    let hashlen = Sedlexing.lexeme_length lexbuf - 1 in
    set_string_start lexbuf;
    raw_string hashlen (Buffer.create 32) lexbuf
  | {|"""|} ->
    set_string_start lexbuf;
    string_multiline (Buffer.create 32) lexbuf
  | '"' ->
    set_string_start lexbuf;
    string (Buffer.create 32) lexbuf
  | 0xFEFF -> BOM
  | Opt sign, '.', '0'..'9', Star identchar ->
    error "Number-like identifiers are invalid and must be quoted"
  | sign, Opt (startident, Star identchar)
  | Sub (startident, sign), Star identchar ->
    IDENT_STRING (Sedlexing.Utf8.lexeme lexbuf)
  | eof -> EOF
  | any -> error @@ sprintf "Illegal character '%s'" (Sedlexing.Utf8.lexeme lexbuf)
  | _ -> assert false

(** This function should be used instead of [Sedlexing.with_tokenizer main] *)
let main_tokenizer lexbuf =
  let lexer () =
    let token = main lexbuf in
    let start_pos, end_pos = Sedlexing.lexing_bytes_positions lexbuf in
    let start_pos = match token with
      | QUOTED_STRING _ | RAW_STRING _ -> !string_start_pos
      | _ -> start_pos
    in
    token, start_pos, end_pos
  in
  lexer
