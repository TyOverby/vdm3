open! Base

type position = [`Me | `Parent | `Sibiling]

module type H = sig
  module Pre : sig
    type t [@@deriving sexp_of]
  end

  module Post : sig
    type t [@@deriving sexp_of]
  end

  val mount : position -> Pre.t -> send:Out.t -> Post.t * position
  val diff : position -> Pre.t * Post.t -> Pre.t -> send:Out.t -> Post.t * position
  val kill : position -> Pre.t -> Post.t -> send:Out.t -> position
  val disqualify : Pre.t * Post.t -> Pre.t -> bool
end

type ('pre, 'post) h = (module H with type Pre.t = 'pre and type Post.t = 'post)
