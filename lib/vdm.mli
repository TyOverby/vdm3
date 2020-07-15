open! Base

module Make (Out : T) : sig
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
end
