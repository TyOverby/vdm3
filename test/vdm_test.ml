open! Base

module V = Vdm.Make (struct
  type t = unit
end)

module Element = struct
  module Pre = struct
    type t =
      { tag : string
      ; attrs : string Map.M(String).t
      ; child : V.Value.t option
      }
    [@@deriving sexp_of]
  end

  module Post = struct
    type t = { prev_child : V.Post_value.t option } [@@deriving sexp_of]
  end

  let mount ({ Pre.tag; attrs; child } as pre) ~send:() =
    Stdlib.Printf.printf "making %s\n" tag;
    Map.iteri attrs ~f:(fun ~key ~data ->
        Stdlib.Printf.printf "%s = %s\n" key data);
    let prev_child = Option.map child ~f:(V.mount ~send:()) in
    pre, { Post.prev_child }
  ;;

  let disqualify (a, _) b = not (String.equal a.Pre.tag b.Pre.tag)

  let diff
      ( { Pre.tag = prev_tag; attrs = prev_attrs; child = _ }
      , { Post.prev_child } )
      ({ Pre.tag = next_tag; attrs = next_attrs; child = next_child } as next)
      ~send:()
    =
    if not (String.equal prev_tag next_tag)
    then Stdlib.Printf.printf "%s -> %s\n" prev_tag next_tag;
    Map.fold_symmetric_diff
      ~init:()
      ~data_equal:String.equal
      prev_attrs
      next_attrs
      ~f:(fun () (k, d) ->
        match d with
        | `Left l -> Stdlib.Printf.printf "%s: - %s\n" k l
        | `Right r -> Stdlib.Printf.printf "%s: + %s\n" k r
        | `Unequal (l, r) -> Stdlib.Printf.printf "%s: %s -> %s\n" k l r);
    let prev_child =
      match prev_child, next_child with
      | Some prev, Some next -> Some (V.diff prev next ~send:())
      | None, Some next -> Some (V.mount next ~send:())
      | Some prev, None ->
        V.unmount prev ~send:();
        None
      | None, None -> None
    in
    next, { Post.prev_child }
  ;;

  let kill _ { Post.prev_child } ~send:() =
    Stdlib.Printf.printf "destroying\n";
    Option.iter prev_child ~f:(V.unmount ~send:())
  ;;
end

let make_element = Staged.unstage (V.register (module Element))

let make_element ?child tag ~attrs =
  make_element
    { Element.Pre.tag; attrs = Map.of_alist_exn (module String) attrs; child }
;;

let%expect_test "mount simple" =
  let a = make_element "div" ~attrs:[ "a", "b" ] in
  let _ = V.mount a ~send:() in
  [%expect {|
    making div
    a = b |}]
;;

let%expect_test "diff simple" =
  let a = make_element "div" ~attrs:[ "a", "b" ] in
  let b = make_element "span" ~attrs:[ "a", "c"; "x", "y" ] in
  let a = V.mount a ~send:() in
  let _ = V.diff a b ~send:() in
  [%expect
    {|
    making div
    a = b
    destroying
    making span
    a = c
    x = y |}]
;;
