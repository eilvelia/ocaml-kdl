type pos = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

(** [file, line, char] *)
let get_pos_info p =
  p.pos_fname, p.pos_lnum, p.pos_cnum - p.pos_bol + 1

(* zero_pos is not exported from Lexing *)
let zero_pos = {
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

let show_pos p =
  let file, line, column = get_pos_info p in
  Printf.sprintf "%s:%d:%d" file line column

(* type t = {
  loc_start : pos;
  loc_end : pos;
} *)
type t = pos * pos

let make loc_start loc_end = loc_start, loc_end

let show (loc_start, loc_end) =
  let (fname, lstart, cstart) = get_pos_info loc_start in
  let (_, lend, cend) = get_pos_info loc_end in
  Printf.sprintf "%s:%d:%d-%d:%d" fname lstart cstart lend cend

let empty = zero_pos, zero_pos

(* type 'node annot = t * 'node *)
