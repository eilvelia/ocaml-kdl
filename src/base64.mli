exception Error of string
(** [Error description]: Base64 decoding error. *)

val decode : string -> bytes
(** Decode a base64-encoded string. This can decode base64 both with and
    without padding. The string can end with arbitrary number of [=] characters.
    @raise Error in case of invalid base64 *)
