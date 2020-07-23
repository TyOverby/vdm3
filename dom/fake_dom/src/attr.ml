open Core_kernel

type any =
  | Bool of bool
  | String of string
  | Float of float
  | Int of int
[@@deriving sexp_of, equal]

type t =
  | Property of string * any
  | Attribute of string * any

let create key value = Attribute (key, String value)
let string_property key value = Property (key, String value)
let property key value = Property (key, value)
let to_any _ = failwith "Calling to_any with fake dom"

let any_to_string any =
  match any with
  | Bool b -> string_of_bool b
  | String s -> s
  | Float f -> string_of_float f
  | Int i -> string_of_int i
;;

let to_table attrs =
  let init = String.Table.create () in
  List.fold attrs ~init ~f:(fun accum attr ->
    match attr with
    | Property (name, value) | Attribute (name, value) ->
      String.Table.set accum ~key:name ~data:(any_to_string value);
      accum)
;;

let bool_to_any b = Bool b
let string_to_any s = String s
let float_to_any f = Float f
let int_to_any i = Int i
