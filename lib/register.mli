open! Base

module Out : sig
  module type S = sig
    val create_element : tag:string -> unit
    val create_text : string -> unit
    val set_attribute : string -> string -> unit
    val remove_attribute : string -> unit
    val first_child : unit -> unit
    val next_child : unit -> unit
    val append_child : unit -> unit
    val pop : unit -> unit
  end

  type t = (module S)

  val both : t -> t -> t
end

module type H = sig
  module Pre : sig
    type t [@@deriving sexp_of]
  end

  module Post : sig
    type t [@@deriving sexp_of]
  end

  val mount : Pre.t -> send:Out.t -> Pre.t * Post.t
  val kill : Pre.t -> Post.t -> send:Out.t -> unit
  val disqualify : Pre.t * Post.t -> Pre.t -> bool
  val diff : Pre.t * Post.t -> Pre.t -> send:Out.t -> Pre.t * Post.t
end

module Value : sig
  type t [@@deriving sexp_of]
end

module Post_value : sig
  type t [@@deriving sexp_of]
end

val register
  :  (module H with type Post.t = 'post and type Pre.t = 'pre)
  -> ('pre -> Value.t) Staged.t

val unmount : Post_value.t -> send:Out.t -> unit
val mount : Value.t -> send:Out.t -> Post_value.t
val diff : Post_value.t -> Value.t -> send:Out.t -> Post_value.t
