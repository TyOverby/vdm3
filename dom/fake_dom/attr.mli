open! Core_kernel

type any =
  | Bool of bool
  | String of string
  | Float of float
  | Int of int
[@@deriving sexp_of, equal]

type t =
  | Property of string * any
  | Attribute of string * any

(** [create name value] creates a simple string-only attribute *)
val create : string -> string -> t

val to_any : 'a -> any

(** [string_property name value] creates a simple string-only property *)
val string_property : string -> string -> t

val any_to_string : any -> string

(** [property name value] creates a property with a generic value *)
val property : string -> any -> t

(** For testing Fake_dom *)
val to_table : t list -> string String.Table.t

val bool_to_any : bool -> any
val string_to_any : string -> any
val int_to_any : int -> any
val float_to_any : float -> any
