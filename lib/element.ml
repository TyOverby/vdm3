open! Base

module T = struct
  module Pre = struct
    type t =
      { tag : string
      ; attributes : string Map.M(String).t
      ; children : Value.t
      }
    [@@deriving sexp_of]
  end

  module Post = struct
    type t = { children : Value.Post.t } [@@deriving sexp_of]
  end

  let disqualify
      ({ Pre.tag = tag_a; attributes = attr_a; _ }, _)
      { Pre.tag = tag_b; attributes = attr_b; _ }
    =
    if not (String.equal tag_a tag_b)
    then true
    else if String.equal tag_a "input"
    then (
      let a_kind = Map.find attr_a "kind" in
      let b_kind = Map.find attr_b "kind" in
      not ([%equal: string option] a_kind b_kind))
    else false
  ;;

  let mount _position { Pre.tag; attributes; children } ~send =
    let module Send = (val (send : Out.t)) in
    Send.create_element ~tag;
    Map.iteri attributes ~f:(fun ~key ~data -> Send.set_attribute key data);
    let children, _status = Value.mount `Parent children ~send in
    { Post.children }, `Me
  ;;

  let diff
      position
      ({ Pre.tag = _; attributes = prev_attrs; children = _ }, { Post.children })
      { Pre.tag = _; attributes = next_attrs; children = next_child }
      ~(send : Out.t)
    =
    let module Send = (val send) in
    Map.fold_symmetric_diff
      ~init:()
      ~data_equal:String.equal
      prev_attrs
      next_attrs
      ~f:(fun () (k, d) ->
        match d with
        | `Left l -> Send.remove_attribute l
        | `Right r | `Unequal (_, r) -> Send.set_attribute k r);
    let children, position =
      let prev, next = children, next_child in
      let children, position = Value.diff position prev next ~send in
      children, position
    in
    { Post.children }, position
  ;;

  let kill position _pre { Post.children } ~send =
    (* TODO: remove node from dom here? *)
    let _position = Value.unmount position children ~send in 
    position
  ;;
end

include Register.Make (T)

let create ~tag ~attrs ~children =
  create { T.Pre.tag; attributes = attrs; children }
;;
