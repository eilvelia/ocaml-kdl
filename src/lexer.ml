open Parser
open Printf

let error msg = raise @@ Err.CustomLexingError msg

(* Note: [Compl] doesn't seem to work *)
let space_chars = [%sedlex.regexp?
    '\t'   (* Character Tabulation U+0009 *)
  | 0x000B (* Line Tabulation U+000B *)
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
  | 0xFEFF (* BOM U+FEFF *)
]

let ws = [%sedlex.regexp? Plus space_chars]

let newline_chars = [%sedlex.regexp?
    '\r'   (* CR   Carriage Return U+000D *)
  | '\n'   (* LF   Line Feed U+000A *)
  | 0x0085 (* NEL  Next Line U+0085 *)
  | 0x000C (* FF   Form Feed U+000C *)
  | 0x2028 (* LS   Line Separator U+2028 *)
  | 0x2029 (* PS   Paragraph Separator U+2029 *)
]

let newline = [%sedlex.regexp? "\r\n" | newline_chars]

(* With this defined as [%sedlex.regexp? ascii_hex_digit], the
   [(integer | float) identchar+] case surprisingly doesn't work correctly *)
let hex_digit = [%sedlex.regexp? Chars "0123456789abcdefABCDEF"]

let sign = [%sedlex.regexp? Chars "-+"]

let decimal_nat = [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_')]
let exponent = [%sedlex.regexp? Chars "eE", Opt sign, decimal_nat]

let decimal_int = [%sedlex.regexp? Opt sign, decimal_nat]
let decimal_float = [%sedlex.regexp?
  Opt sign, decimal_nat, ('.', decimal_nat, Opt exponent
                         | exponent) ]
let hex = [%sedlex.regexp? Opt sign, "0x", hex_digit, Star (hex_digit | '_')]
let octal = [%sedlex.regexp? Opt sign, "0o", '0'..'7', Star ('0'..'7' | '_')]
let binary = [%sedlex.regexp? Opt sign, "0b", Chars "01", Star (Chars "01_")]

let integer = [%sedlex.regexp? decimal_int | hex | octal | binary]
let float = [%sedlex.regexp? decimal_float]

(* Disallowed:
   Any codepoint with hexadecimal value 0x20 or below
   Any codepoint with hexadecimal value higher than 0x10FFFF
   Any of {| \/(){}<>;[]=," |} *)
let disallowed_chars = [%sedlex.regexp? Chars "\\/(){}<>;[]=,\""
                                      | 0 .. 0x20
                                      | space_chars
                                      | newline_chars]
let identchar = [%sedlex.regexp? Sub (any, disallowed_chars)]
let startident = [%sedlex.regexp? Sub (identchar, '0'..'9')]

let[@inline] new_line lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  if lexeme <> "\n" && lexeme <> "\r\n" then
    Sedlexing.new_line lexbuf

let string_buffer = Buffer.create 256
let string_start_pos = ref Lexing.dummy_pos

let[@inline] set_string_start lexbuf =
  let start_pos, _ = Sedlexing.lexing_positions lexbuf in
  string_start_pos := start_pos

(* let[@inline] recover_string_location lexbuf =
  Sedlexing.mark lexbuf 0x1000;
  Sedlexing.set_position lexbuf !string_start_pos;
  Sedlexing.start lexbuf;
  ignore @@ Sedlexing.backtrack lexbuf *)

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

let rec raw_string hashlen lexbuf =
  match%sedlex lexbuf with
  | newline ->
    new_line lexbuf;
    Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
    raw_string hashlen lexbuf
  | '"', Star '#' ->
    let hashes =
      Sedlexing.Utf8.sub_lexeme lexbuf 1 (Sedlexing.lexeme_length lexbuf - 1) in
    let hashlen' = String.length hashes in
    if hashlen = hashlen' then
      RAW_STRING (Buffer.contents string_buffer)
    else begin
      Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
      raw_string hashlen lexbuf
    end
  | eof -> error "Unterminated raw string"
  | any ->
    Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
    raw_string hashlen lexbuf
  | _ -> assert false

let rec string lexbuf =
  match%sedlex lexbuf with
  | newline ->
    new_line lexbuf;
    Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
    string lexbuf
  | "\\n" -> Buffer.add_char string_buffer '\n'; string lexbuf
  | "\\r" -> Buffer.add_char string_buffer '\r'; string lexbuf
  | "\\t" -> Buffer.add_char string_buffer '\t'; string lexbuf
  | "\\\\" -> Buffer.add_char string_buffer '\\'; string lexbuf
  | "\\\"" -> Buffer.add_char string_buffer '"'; string lexbuf
  | "\\b" -> Buffer.add_char string_buffer '\b'; string lexbuf
  | "\\f" -> Buffer.add_char string_buffer '\012'; string lexbuf
  | "\\u{", Rep (hex_digit, 1 .. 6), '}' ->
    (* TODO: Disallow 7+ hex digits inside \u{...}? *)
    let code_str =
      Sedlexing.Utf8.sub_lexeme lexbuf 3 (Sedlexing.lexeme_length lexbuf - 4) in
    let code = int_of_string @@ "0x" ^ code_str in
    if code > 0x10FFFF then
      error "Invalid unicode code point, cannot be greater than 10FFFF";
    if not @@ Uchar.is_valid code then
      error "Invalid unicode code point";
    Buffer.add_utf_8_uchar string_buffer (Uchar.unsafe_of_int code);
    string lexbuf
  | '\\', ws -> whitespace_escape lexbuf; string lexbuf
  | '\\', newline -> new_line lexbuf; whitespace_escape lexbuf; string lexbuf
  (* TODO: Should we error in case of invalid escape sequences? *)
  | '"' ->
    STRING (Buffer.contents string_buffer)
  | eof -> error "Unterminated string"
  | any ->
    Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
    string lexbuf
  | _ -> assert false

let rec line_cont lexbuf =
  match%sedlex lexbuf with
  | ws -> line_cont lexbuf
  | newline -> new_line lexbuf
  | "//" -> line_comment lexbuf
  | "/*" -> comment 0 lexbuf; line_cont lexbuf
  | eof -> error "Unexpected EOF after the '\\' line continuation"
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
  | "/-" -> DISABLE
  | ';' -> SEMI
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '=' -> EQ
  | "true" -> TRUE
  | "false" -> FALSE
  | "null" -> NULL
  | integer -> INTEGER (Sedlexing.Utf8.lexeme lexbuf)
  | float ->
    let float_str = Sedlexing.Utf8.lexeme lexbuf in
    begin match float_of_string_opt float_str with
    | Some f -> FLOAT f
    | None -> error @@ sprintf "Invalid float literal %s" float_str
    end
  | (integer | float), Plus identchar ->
    error @@ sprintf "Invalid number literal %s" (Sedlexing.Utf8.lexeme lexbuf)
  | 'r', Star '#', '"' ->
    let hashes =
      Sedlexing.Utf8.sub_lexeme lexbuf 1 (Sedlexing.lexeme_length lexbuf - 2) in
    Buffer.reset string_buffer;
    set_string_start lexbuf;
    raw_string (String.length hashes) lexbuf
  | '"' ->
    Buffer.reset string_buffer;
    set_string_start lexbuf;
    string lexbuf
  | "r#", Star identchar -> error "An identifier cannot start with r#"
  | '-', startident, Star identchar -> IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | '-' -> IDENT "-"
  | Sub (startident, '-'), Star identchar -> IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | eof -> EOF
  | any -> error @@ sprintf "Illegal character '%s'" (Sedlexing.Utf8.lexeme lexbuf)
  | _ -> assert false

(** This function should be used instead of [Sedlexing.with_tokenizer main] *)
let main_tokenizer lexbuf =
  let lexer () =
    let token = main lexbuf in
    let start_pos', end_pos = Sedlexing.lexing_positions lexbuf in
    let start_pos = match token with
      | STRING _ | RAW_STRING _ -> !string_start_pos
      | _ -> start_pos'
    in
    token, start_pos, end_pos
  in
  lexer

(* let query_identchar = [%sedlex.regexp? Sub (identchar, Chars "+~|^$*")]
let query_startident = [%sedlex.regexp? Sub (query_identchar, '0'..'9')]

(* Lexical analyzer for the KDL Query Language *)
let rec query lexbuf =
  match%sedlex lexbuf with
  | ws -> query lexbuf
  | newline -> new_line lexbuf; query lexbuf
  | '[' -> LBRACKET
  | ']' -> RBRACKET
  | '>' -> GT
  | ">=" -> GTE
  | '<' -> LT
  | "<=" -> LTE
  | '+' -> PLUS
  | '~' -> TILDE
  | "||" -> PARALLEL
  | "^=" -> CARET_EQ
  | "$=" -> DOLLAR_EQ
  | "*=" -> STAR_EQ
  | '-', query_startident, Star query_identchar ->
    IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | '-' -> IDENT "-"
  | Sub (query_startident, '-'), Star query_identchar ->
    IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | any ->
    error @@ sprintf "Illegal character '%s'" (Sedlexing.Utf8.lexeme lexbuf)
  | _ -> assert false *)
