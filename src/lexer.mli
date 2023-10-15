val main : Sedlexing.lexbuf -> Parser.token
(** Note: Use [main_tokenizer] instead of [Sedlexing.with_tokenizer main]
    to keep the string locations correct. *)

val main_tokenizer
  :  Sedlexing.lexbuf
  -> unit -> Parser.token * Lexing.position * Lexing.position
(** The KDL lexer. *)
