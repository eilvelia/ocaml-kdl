(** {1 The definition of a KDL document} *)

type value =
  [ `String of string
  | `Int of int
  | `RawInt of string
  | `Float of float
  | `Bool of bool
  | `Null ]
(** A KDL value.

    Note: Although the KDL spec does not differentiate integers and floats, a
    number is parsed as [`Float] if it is written in the [e] scientific notation
    or contains a [.], same as in OCaml. If an integer is too large for the
    OCaml [int], the integer is parsed as [`RawInt] instead of [`Int]. *)

(*_ TODO: Change to `Number? *)

type annot_value = string option * value
(** A KDL value with an optional type annotation: [opt_annot * value].
    For example, [(Some "u16", `Int 3201)] is an [annot_value]. *)

type prop = string * annot_value
(** A KDL property is a key-value pair. *)

(** A KDL node.

    [props] is an association list; the order of [props] is unspecified. *)
type node =
  { name : string
  ; annot : string option
  ; args : annot_value list
  ; props : prop list
  ; children : node list }

type t = node list
(** A KDL document is a list of nodes. *)

(** {1 Parsing} *)

type error_loc = Lexing.position * Lexing.position
(** The location generated by [sedlex].
    Note: the positions count unicode code points, not bytes. *)

type error = string * error_loc
(** A parsing error. *)

val show_error : error -> string

exception Parse_error of error

val from_string : ?fname:string -> string -> (t, error) result
(** Parse KDL from a string. The string should be UTF8-encoded. [?fname] is
    an optional filename that is used in locations. *)

val from_string_exn : ?fname:string -> string -> t
(** [from_string_exn] is a raising version of [from_string].

    @raise Parse_error *)

val from_channel : ?fname:string -> in_channel -> (t, error) result
(** Parse KDL from a channel. *)

val from_channel_exn : ?fname:string -> in_channel -> t
(** @raise Parse_error instead of returning a [result]. *)

val of_string : ?fname:string -> string -> (t, error) result
(** Alias for [from_string]. *)

val of_string_exn : ?fname:string -> string -> t
(** Alias for [from_string_exn]. *)

(** {1 Helpers} *)

val node : ?annot:string
        -> string
        -> ?args:annot_value list
        -> ?props:prop list
        -> node list
        -> node
(** [node ?annot name ?args ?props children] constructs a {!type:node}. *)

val arg : ?annot:string -> value -> annot_value
(** [arg ?annot value] constructs an argument (that is, a value with an optional
    annotation). *)

val prop : ?annot:string -> string -> value -> prop
(** [prop ?annot name value] constructs a property. *)

(** {1 Equivalence} *)

val equal_value : value -> value -> bool

val equal_annot_value : annot_value -> annot_value -> bool

val equal_prop : prop -> prop -> bool

val equal_node : node -> node -> bool

val equal : t -> t -> bool

(** {1 Sexp conversions} *)

val sexp_of_value : [< value] -> Sexplib0.Sexp.t

val sexp_of_annot_value : annot_value -> Sexplib0.Sexp.t

val sexp_of_prop : prop -> Sexplib0.Sexp.t

val sexp_of_node : node -> Sexplib0.Sexp.t

val sexp_of_t : t -> Sexplib0.Sexp.t

(** {1 Pretty-printing} *)

val indent : int ref
(** [indent] is the number of spaces used per indentation level.
    By default, [indent] is set to [2]. *)

val pp_value : Format.formatter -> [< value] -> unit
(** Pretty-print a value. *)

val pp_annot_value : Format.formatter -> annot_value -> unit
(** Pretty-print an annotated value. *)

val pp_prop : Format.formatter -> prop -> unit
(** Pretty-print a property. *)

val pp_node : Format.formatter -> node -> unit
(** Pretty-print a node using [!indent] as the indentation width for children
    nodes. *)

val pp_node_compact : Format.formatter -> node -> unit
(** Same as {!pp_node}, but outputs the result in one line. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a list of nodes or a KDL document using [!indent] as the
    indentation width for children nodes. *)

val pp_compact : Format.formatter -> t -> unit
(** Same as {!pp}, but outputs the result in one line. *)

val to_string : t -> string
(** Pretty-print a KDL document into a string using {!pp}. *)

val show : t -> string
(** Alias for [to_string]. *)

(** {1 Type annotations} *)

(** KDL {{:https://github.com/kdl-org/kdl/blob/1.0.0/SPEC.md#type-annotation} defines}
    reserved type annotations. Some of them are supported out of the box in
    ocaml-kdl. An annotated value can be "interpreted" using the {!interpret}
    function. If the type annotation is unknown, [`Other] is returned.

    [typed_value] can be extended with custom type annotations, for example,
    the following way:
    {[
      type typed_value = [ Kdl.typed_value
                         | `Date of (* ... *) ]

      let interpret : _ -> typed_value = function
        | Some "date", value -> `Date ((* ... parse ISO 8601 date ... *))
        | x -> Kdl.interpret x
    ]} *)

type typed_value =
  [ `I8 of int
  | `I16 of int
  | `I32 of int32
  | `I64 of int64
  | `U8 of int
  | `U16 of int
  | `U32 of int32
  | `U64 of int64
  | `Isize of nativeint
  | `Usize of nativeint
  | `F32 of float
  | `F64 of float
  | `Base64 of bytes
  | `Other of string * value
  | `Unannotated of value ]

exception Invalid_annotation of string

val interpret : annot_value -> [> typed_value]
(** Interpret a type-annotated value.

    Note: [f32] is currently the same as [f64].

    @raise Invalid_annotation if the value is invalid. For example, if the value
    is annotated as "u8" but exceeds the range of u8, [Invalid_annotation] is
    raised. *)

(** {1 Lenses} *)

module L : sig
  (** Basic partial lenses for accessing deeply-nested KDL structures.

      Note: These lenses are mostly meant to be used for getting, [set] is
      generally inefficient. *)

  type ('a, 'b) lens =
    { get : 'a -> 'b option
    ; set : 'b -> 'a -> 'a option }

  val compose : ('b, 'c) lens -> ('a, 'b) lens -> ('a, 'c) lens
  (** Lens composition. *)

  val (|--) : ('a, 'b) lens -> ('b, 'c) lens -> ('a, 'c) lens
  (** [l1 |-- l2] is an infix operator for [compose l2 l1]. *)

  val (//) : ('a, 'b) lens -> ('b, 'c) lens -> ('a, 'c) lens
  (** Alias for [(|--)]. *)

  val get : 'a -> ('a, 'b) lens -> 'b option
  (** Run the getter ([lens.get]). Returns [Some value] if the lens
      successfully matches, or [None] otherwise.  *)

  val set : 'a -> 'b -> ('a, 'b) lens -> 'a option
  (** Run the setter ([lens.set]). *)

  val get_exn : 'a -> ('a, 'b) lens -> 'b
  (** [get_exn a l] is [Option.get (get a l)]. *)

  val set_exn : 'a -> 'b -> ('a, 'b) lens -> 'a
  (** [set_exn a v l] is [Option.get (set a v l)]. *)

  val (.@()) : 'a -> ('a, 'b) lens -> 'b option
  (** [(.@())] is an indexing operator for [get]. *)

  val (.@()<-) : 'a -> ('a, 'b) lens -> 'b -> 'a option
  (** [(.@()<-)] is an indexing operator for [set]: it allows one to write
      [document.@(lens) <- value] instead of [set document value lens]. *)

  val update : ('b -> 'b option) -> 'a -> ('a, 'b) lens -> 'a option
  (** The combination of [get] and [set]. *)

  val node_name : (node, string) lens
  (** Lens to [node.name]. *)

  val node_annot : (node, string) lens
  (** Lens to [node.annot]. *)

  val node_annot_opt : (node, string option) lens
  (** Lens to [node.annot] as an [option]. Pass [None] to remove the
      annotation. *)

  val args : (node, annot_value list) lens
  (** Lens to [node.args]. *)

  val props : (node, prop list) lens
  (** Lens to [node.props]. *)

  val children : (node, node list) lens
  (** Lens to [node.children], i.e. all children of a node. *)

  val arg : int -> (node, annot_value) lens
  (** [arg n] is a lens to the [n]-th argument of a node, starting at 0.
      Operates in O(n) time. *)

  val prop : string -> (node, annot_value) lens
  (** Lens to the property with the specific name. Operates in O(n) time. *)

  val node : ?annot:string -> string -> (node list, node) lens
  (** [node ?annot name] is a lens to the first node with the specific name in
      a list. The search optionally can be narrowed by passing [?annot]. *)

  val node_many : ?annot:string -> string -> (node list, node list) lens
  (** Same as {!val:node}, but returns all possible matches instead of the
      first one. *)

  val node_nth : int -> (node list, node) lens
  (** Lens to the [n]-th node in a list, starting at 0. *)

  val child : ?annot:string -> string -> (node, node) lens
  (** [child ?annot name] is [children |-- node ?annot name]. *)

  val child_many : ?annot:string -> string -> (node, node list) lens
  (** [child_many ?annot name] is [children |-- node_many ?annot name]. *)

  val child_nth : int -> (node, node) lens
  (** [child_nth n] is [children |-- node_nth n]. *)

  val value : (annot_value, value) lens
  (** Lens to [value] in the [annot_value] pair. *)

  val annot : (annot_value, string) lens
  (** Lens to [annot] in the [annot_value] pair. *)

  val annot_opt : (annot_value, string option) lens
  (** Lens to [annot] as an [option]. Pass [None] to unset the annotation. *)

  val string : ([> `String of string ], string) lens
  (** Lens to a string value. *)

  val int : ([> `Int of int ], int) lens
  (** Lens to an int value. *)

  val raw_int : ([> `RawInt of string ], string) lens
  (** Lens to a raw int value. *)

  val float : ([> `Float of float ], float) lens
  (** Lens to a float value. *)

  val number : (value, [ `Float of float | `Int of int | `RawInt of string ]) lens
  (** Lens to any numeric KDL value. *)

  val bool : ([> `Bool of bool ], bool) lens
  (** Lens to a boolean value. *)

  val null : ([> `Null ], unit) lens
  (** Lens to a null value. *)

  val string_value : (annot_value, string) lens
  (** [string_value] is [value |-- string]. *)

  val int_value : (annot_value, int) lens
  (** [int_value] is [value |-- int]. *)

  val raw_int_value : (annot_value, string) lens
  (** [raw_int_value] is [value |-- raw_int]. *)

  val float_value : (annot_value, float) lens
  (** [float_value] is [value |-- float]. *)

  val bool_value : (annot_value, bool) lens
  (** [bool_value] is [value |-- bool]. *)

  val null_value : (annot_value, unit) lens
  (** [null_value] is [value |-- null]. *)

  val top : ('a list, 'a) lens
  (** Lens to the first element of a list. *)

  val nth : int -> ('a list, 'a) lens
  (** Lens to the [n]-th element of a list, starting at 0. *)

  val each : ('a, 'b) lens -> ('a list, 'b list) lens
  (** Apply a lens to multiple items. Example:
      [top // child_many "paragraph" // each (arg 0 // value)]. *)

  val filter : ('a -> bool) -> ('a list, 'a list) lens
end
