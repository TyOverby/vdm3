open! Base

module T = struct
  module Pre = struct
    type t =
      { tag : string
      ; attributes : string Map.M(String).t
      ; children : Register.Value.t
      }
    [@@deriving sexp_of]
  end

  module Post = struct
    type t = { children : Register.Post_value.t } [@@deriving sexp_of]
  end

  let disqualify ({ Pre.tag = tag_a; _ }, _) { Pre.tag = tag_b; _ } =
    not (String.equal tag_a tag_b)
  ;;

  let mount ({ Pre.tag; attributes; children } as pre) ~send =
    let module Send = (val (send : Register.Out.t)) in
    Send.create_element ~tag;
    Map.iteri attributes ~f:(fun ~key ~data -> Send.set_attribute key data);
    let children = Register.mount children ~send in
    Send.append_child ();
    pre, { Post.children }
  ;;

  let diff
      ({ Pre.tag = _; attributes = prev_attrs; children = _ }, { Post.children })
      ({ Pre.tag = _; attributes = next_attrs; children = next_child } as next)
      ~(send : Register.Out.t)
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
    let children =
      let prev, next = children, next_child in
      Register.diff prev next ~send
    in
    next, { Post.children }
  ;;

  let kill _ { Post.children } ~send =
    (* TODO: remove node from dom here? *)
    Register.unmount children ~send
  ;;
end

let make = Staged.unstage (Register.register (module T))

let create ~tag ~attrs ~children =
  make { T.Pre.tag; attributes = attrs; children }
;;
