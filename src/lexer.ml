open Parser
open Printf

let error msg = raise @@ Err.CustomLexingError msg

let space_char = [%sedlex.regexp?
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
]

let ws = [%sedlex.regexp? Plus space_char]

let newline_char = [%sedlex.regexp?
    '\r'   (* CR   Carriage Return U+000D *)
  | '\n'   (* LF   Line Feed U+000A *)
  | 0x0085 (* NEL  Next Line U+0085 *)
  | 0x000C (* FF   Form Feed U+000C *)
  | 0x2028 (* LS   Line Separator U+2028 *)
  | 0x2029 (* PS   Paragraph Separator U+2029 *)
]

let newline = [%sedlex.regexp? "\r\n" | newline_char]

let equals_sign = [%sedlex.regexp?
    '=' (* Equals Sign U+003D *)
  | 0xFE66 (* Small Equals Sign U+FE66 *)
  | 0xFF1D (* Fullwidth Equals Sign U+FF1D *)
  | 0x1F7F0 (* Heavy Equals Sign U+1F7F0 *)
]

(* Note: With this defined as [%sedlex.regexp? ascii_hex_digit], the
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

let disallowed_char = [%sedlex.regexp?
    0 .. 0x19
  | 0x7F (* Delete *)
  (* Direction control *)
  | 0x2066 .. 0x2069 | 0x202A .. 0x202E | 0x200E | 0x200F
]

let nonident_char = [%sedlex.regexp? Chars "(){}[]/\\\"#;"
                                     | disallowed_char
                                     | equals_sign
                                     | space_char
                                     | newline_char
                                     | 0xFEFF ]
let identchar = [%sedlex.regexp? Sub (any, nonident_char)]
let startident = [%sedlex.regexp? Sub (identchar, '0'..'9')]

let[@inline] new_line lexbuf =
  (* Add newline to the position info if the lexeme doesn't end with \n *)
  let u = Sedlexing.lexeme_char lexbuf (Sedlexing.lexeme_length lexbuf - 1) in
  if Uchar.to_int u <> Char.code '\n' then
    Sedlexing.new_line lexbuf

let string_buffer = Buffer.create 256
let string_start_pos = ref Lexing.dummy_pos

let[@inline] set_string_start lexbuf =
  let start_pos, _ = Sedlexing.lexing_positions lexbuf in
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

let dedent ~prefix str =
  let prefix_len = String.length prefix in
  Buffer.reset string_buffer;
  let line_i = ref 0 in
  for i = 0 to String.length str - 1 do
    let ch = String.unsafe_get str i in
    if !line_i < prefix_len && (!line_i <> 0 || ch <> '\xFF') then begin
      if ch <> String.get prefix !line_i then
        error "Whitespace prefix does not match"
    end else if ch = '\xFF' then begin
      (* The last newline is trimmed *)
      if i <> String.length str - 1 then
        (* Newline is normalized to \n *)
        (Buffer.add_char string_buffer '\n'; line_i := -1)
    end else begin
      Buffer.add_char string_buffer ch
    end;
    incr line_i
  done;
  Buffer.contents string_buffer

let[@inline] check_hashlen ~exp (got : int) =
  if got > exp then
    error (sprintf "Expected %d hash symbol(s), got %d" exp got)
  else got = exp

let raw_string ~multi exp_hashlen lexbuf =
  let rec recur linestart =
    match%sedlex lexbuf with
    | newline ->
      new_line lexbuf;
      if not multi then
        error "Multi-line string must begin with a newline character";
      (* Mark literal newline with an invalid UTF-8 character *)
      Buffer.add_uint8 string_buffer 0xFF;
      recur true
    | ws, '"', Plus '#' ->
      let quote_pos =
        let rec go pos =
          if Uchar.to_int (Sedlexing.lexeme_char lexbuf pos) = Char.code '"'
          then pos else go (pos + 1)
        in
        go 0
      in
      let hashlen = Sedlexing.lexeme_length lexbuf - quote_pos - 1 in
      if check_hashlen ~exp:exp_hashlen hashlen then begin
        let prefix = Sedlexing.Utf8.sub_lexeme lexbuf 0 quote_pos in
        if multi && linestart then
          RAW_STRING (dedent ~prefix (Buffer.contents string_buffer))
        else if multi then
          error "The final line of a multi-line string must be whitespace"
        else begin
          Buffer.add_string string_buffer prefix;
          RAW_STRING (Buffer.contents string_buffer)
        end
      end else begin
        Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
        recur false
      end
    | '"', Plus '#' ->
      let hashlen = Sedlexing.lexeme_length lexbuf - 1 in
      if check_hashlen ~exp:exp_hashlen hashlen then begin
        if multi && linestart then
          RAW_STRING (dedent ~prefix:"" (Buffer.contents string_buffer))
        else if multi then
          error "The final line of a multi-line string must be whitespace"
        else RAW_STRING (Buffer.contents string_buffer)
      end else begin
        Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
        recur false
      end
    | disallowed_char -> error "Illegal character"
    | eof -> error "Unterminated raw string"
    | any ->
      Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
      recur false
    | _ -> assert false
  in
  recur true

let rec string ~multi linestart lexbuf =
  match%sedlex lexbuf with
  | newline ->
    new_line lexbuf;
    if not multi then
      error "Multi-line string must begin with a newline character";
    (* Mark literal newline with an invalid UTF-8 character *)
    Buffer.add_uint8 string_buffer 0xFF;
    string ~multi true lexbuf
  | "\\n" -> Buffer.add_char string_buffer '\n'; string ~multi false lexbuf
  | "\\r" -> Buffer.add_char string_buffer '\r'; string ~multi false lexbuf
  | "\\t" -> Buffer.add_char string_buffer '\t'; string ~multi false lexbuf
  | "\\\\" -> Buffer.add_char string_buffer '\\'; string ~multi false lexbuf
  | "\\\"" -> Buffer.add_char string_buffer '"'; string ~multi false lexbuf
  | "\\b" -> Buffer.add_char string_buffer '\b'; string ~multi false lexbuf
  | "\\f" -> Buffer.add_char string_buffer '\012'; string ~multi false lexbuf
  | "\\s" -> Buffer.add_char string_buffer ' '; string ~multi false lexbuf
  | "\\u{", Plus hex_digit, '}' ->
    let code_str =
      Sedlexing.Utf8.sub_lexeme lexbuf 3 (Sedlexing.lexeme_length lexbuf - 4) in
    if String.length code_str > 6 then
      error "Invalid unicode scalar value";
    let code = int_of_string @@ "0x" ^ code_str in
    if not @@ Uchar.is_valid code then
      error "Invalid unicode scalar value";
    Buffer.add_utf_8_uchar string_buffer (Uchar.unsafe_of_int code);
    string ~multi false lexbuf
  | '\\', ws -> whitespace_escape lexbuf; string ~multi false lexbuf
  | '\\', newline ->
    new_line lexbuf;
    whitespace_escape lexbuf;
    string ~multi true lexbuf (* TODO: Is this correct? *)
  | '\\', any -> error "Invalid escape sequence"
  | ws, '"' ->
    let prefix =
      Sedlexing.Utf8.sub_lexeme lexbuf 0 (Sedlexing.lexeme_length lexbuf - 1) in
    if multi && linestart then
      QUOTED_STRING (dedent ~prefix (Buffer.contents string_buffer))
    else if multi then
      error "The final line of a multi-line string must be whitespace"
    else begin
      Buffer.add_string string_buffer prefix;
      QUOTED_STRING (Buffer.contents string_buffer)
    end
  | '"' ->
    if multi && linestart then
      QUOTED_STRING (dedent ~prefix:"" (Buffer.contents string_buffer))
    else if multi then
      error "The final line of a multi-line string must be whitespace"
    else QUOTED_STRING (Buffer.contents string_buffer)
  | disallowed_char -> error "Illegal character"
  | eof -> error "Unterminated string"
  | any ->
    Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
    string ~multi false lexbuf
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
  | "/-" -> DISABLE
  | ';' -> SEMI
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | equals_sign -> EQ
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
  | Plus '#', '"', newline ->
    let[@inline] rec count_hashes lexbuf i =
      if Uchar.to_int (Sedlexing.lexeme_char lexbuf i) = Char.code '#' then
        count_hashes lexbuf (i + 1)
      else i
    in
    let hashlen = count_hashes lexbuf 0 in
    Buffer.reset string_buffer;
    set_string_start lexbuf;
    raw_string ~multi:true hashlen lexbuf
  | Plus '#', '"' ->
    let hashlen = Sedlexing.lexeme_length lexbuf - 1 in
    Buffer.reset string_buffer;
    set_string_start lexbuf;
    raw_string ~multi:false hashlen lexbuf
  | '"', newline ->
    new_line lexbuf;
    Buffer.reset string_buffer;
    set_string_start lexbuf;
    string ~multi:true true lexbuf
  | '"' ->
    Buffer.reset string_buffer;
    set_string_start lexbuf;
    string ~multi:false true lexbuf
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
    let start_pos, end_pos = Sedlexing.lexing_positions lexbuf in
    let start_pos = match token with
      | QUOTED_STRING _ | RAW_STRING _ -> !string_start_pos
      | _ -> start_pos
    in
    token, start_pos, end_pos
  in
  lexer
