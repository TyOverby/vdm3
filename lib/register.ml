open! Base

module Out = struct
  module type S = sig
    val create_element : tag:string -> unit
    val create_text : string -> unit
    val set_attribute : string -> string -> unit
    val remove_attribute : string -> unit
    val append_child : unit -> unit
    val push : unit -> unit
    val pop : unit -> unit
  end

  type t = (module S)

  let both (module A : S) (module B : S) =
    let module O = struct
      let create_element ~tag =
        A.create_element ~tag;
        B.create_element ~tag
      ;;

      let create_text text =
        A.create_text text;
        B.create_text text
      ;;

      let set_attribute key data =
        A.set_attribute key data;
        B.set_attribute key data
      ;;

      let remove_attribute key =
        A.remove_attribute key;
        B.remove_attribute key
      ;;

      let append_child () =
        A.append_child ();
        B.append_child ()
      ;;

      let push () =
        A.push ();
        B.push ()
      ;;

      let pop () =
        A.pop ();
        B.pop ()
      ;;
    end
    in
    (module O : S)
  ;;
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

type ('pre, 'post) h = (module H with type Pre.t = 'pre and type Post.t = 'post)

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

module Post_value = struct
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

let register
    : type pre post.
      (module H with type Pre.t = pre and type Post.t = post)
      -> (pre -> Value.t) Staged.t
  =
 fun (module M : H with type Pre.t = pre and type Post.t = post) ->
  let pre_kind = Type_equal.Id.create ~name:"pre" M.Pre.sexp_of_t in
  let post_kind = Type_equal.Id.create ~name:"post" M.Post.sexp_of_t in
  let kind = { Kind.pre_kind; post_kind; behavior = (module M) } in
  Staged.stage (fun pre -> Value.T { kind; pre })
;;

let unmount a ~send =
  let (Post_value.T { kind = kind_a; pre = pre_a; post = post_a }) = a in
  let module M = (val kind_a.Kind.behavior) in
  M.kill pre_a post_a ~send
;;

let mount a ~send =
  let (Value.T { kind; pre }) = a in
  let module M = (val kind.Kind.behavior) in
  let pre, post = M.mount pre ~send in
  Post_value.T { kind; pre; post }
;;

let diff a b ~send =
  let open Kind in
  let (Post_value.T { kind = kind_a; pre = pre_a; post = post_a }) = a in
  let (Value.T { kind = kind_b; pre = pre_b }) = b in
  match
    ( Type_equal.Id.same_witness kind_a.post_kind kind_b.post_kind
    , Type_equal.Id.same_witness kind_a.pre_kind kind_b.pre_kind )
  with
  | Some T, Some T ->
    let module M = (val kind_a.Kind.behavior) in
    if phys_equal pre_a pre_b
    then a
    else if M.disqualify (pre_a, post_a) pre_b
    then (
      unmount a ~send;
      mount b ~send)
    else (
      let pre, post = M.diff (pre_a, post_a) pre_b ~send in
      Post_value.T { kind = kind_a; pre; post })
  | _ ->
    unmount a ~send;
    mount b ~send
;;
