open! Base
open! Types

module Kind = struct
  type ('pre, 'post) t =
    { pre_kind : 'pre Type_equal.Id.t
    ; post_kind : 'post Type_equal.Id.t
    ; behavior : ('pre, 'post) h
    }
end

module Value = struct
  type t =
    | T :
        { kind : ('pre, 'post) Kind.t
        ; pre : 'pre
        }
        -> t

  let sexp_of_t (T { kind; pre }) = Type_equal.Id.to_sexp kind.Kind.pre_kind pre
end

module Post = struct
  type t =
    | T :
        { kind : ('pre, 'post) Kind.t
        ; pre : 'pre
        ; post : 'post
        }
        -> t

  let sexp_of_t (T { kind; pre; post }) =
    Sexp.List
      [ Type_equal.Id.to_sexp kind.Kind.pre_kind pre
      ; Type_equal.Id.to_sexp kind.Kind.post_kind post
      ]
  ;;
end

include Value

let unmount position a ~send =
  let (Post.T { kind = kind_a; pre = pre_a; post = post_a }) = a in
  let module M = (val kind_a.Kind.behavior) in
  M.kill position pre_a post_a ~send
;;

let mount position a ~send =
  let (Value.T { kind; pre }) = a in
  let module M = (val kind.Kind.behavior) in
  let post, position =  M.mount position pre ~send in
  Post.T { kind; pre; post }, position
;;

let diff position a b ~send =
  let open Kind in
  let (Post.T { kind = kind_a; pre = pre_a; post = post_a }) = a in
  let (Value.T { kind = kind_b; pre = pre_b }) = b in
  match
    ( Type_equal.Id.same_witness kind_a.post_kind kind_b.post_kind
    , Type_equal.Id.same_witness kind_a.pre_kind kind_b.pre_kind )
  with
  | Some T, Some T ->
    let module M = (val kind_a.Kind.behavior) in
    if phys_equal pre_a pre_b
    then a, position
    else if M.disqualify (pre_a, post_a) pre_b
    then (
      let position = unmount position a ~send in
      mount position b ~send)
    else (
      let post, _position = M.diff position (pre_a, post_a) pre_b ~send in
      Post.T { kind = kind_a; pre=pre_b; post }), position
  | _ ->
    let position = unmount position a ~send in
    mount position b ~send
;;


module Private = struct 
  let create  ~pre_kind ~post_kind ~behavior = 
    let kind = { Kind.pre_kind; post_kind; behavior } in
    fun pre -> Value.T { kind; pre }
end
