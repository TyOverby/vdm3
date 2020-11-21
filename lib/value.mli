open! Base   
open Types

type t [@@deriving sexp_of]

module Post : sig
  type t [@@deriving sexp_of]
end

val unmount : position -> Post.t -> send:Out.t -> position
val mount : position -> t -> send:Out.t -> Post.t * position
val diff : position -> Post.t -> t -> send:Out.t -> Post.t * position

module Private : sig 
  val create : 
  pre_kind:'a Type_equal.Id.t ->
  post_kind:'b Type_equal.Id.t -> behavior:('a, 'b) h -> 'a -> t
end
