open! Core_kernel

module Dummy = struct
  let create_element ~tag = print_s [%message "create_element" (tag : string)]
  let create_text text = print_s [%message "create_text" (text : string)]

  let set_attribute key data =
    print_s [%message "set_attribute" (key : string) (data : string)]
  ;;

  let remove_attribute key =
    print_s [%message "remove_attribute" (key : string)]
  ;;

  let append_child () = print_s [%message "append_child"]
  let push () = print_s [%message "push"]
  let pop () = print_s [%message "pop"]
  let first_child () = print_s [%message "first_child"]
  let next_child () = print_s [%message "next_child"]
end

let send = (module Dummy : Vdm.Register.Out.S)

module Element = struct
  module Pre = struct
    type t =
      { tag : string
      ; attrs : string Map.M(String).t
      ; child : Vdm.Value.t option
      }
    [@@deriving sexp_of]
  end

  module Post = struct
    type t = { prev_child : Vdm.Post.t option } [@@deriving sexp_of]
  end

  let mount ({ Pre.tag; attrs; child } as pre) ~send =
    Stdlib.Printf.printf "making %s\n" tag;
    Map.iteri attrs ~f:(fun ~key ~data ->
        Stdlib.Printf.printf "%s = %s\n" key data);
    let prev_child = Option.map child ~f:(Vdm.mount ~send) in
    pre, { Post.prev_child }
  ;;

  let disqualify (a, _) b = not (String.equal a.Pre.tag b.Pre.tag)

  let diff
      ( { Pre.tag = prev_tag; attrs = prev_attrs; child = _ }
      , { Post.prev_child } )
      ({ Pre.tag = next_tag; attrs = next_attrs; child = next_child } as next)
      ~send
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
      | Some prev, Some next -> Some (Vdm.diff prev next ~send)
      | None, Some next -> Some (Vdm.mount next ~send)
      | Some prev, None ->
        Vdm.unmount prev ~send;
        None
      | None, None -> None
    in
    next, { Post.prev_child }
  ;;

  let kill _ { Post.prev_child } ~send =
    Stdlib.Printf.printf "destroying\n";
    Option.iter prev_child ~f:(Vdm.unmount ~send)
  ;;
end

let make_element = Staged.unstage (Vdm.Register.register (module Element))

let make_element ?child tag ~attrs =
  make_element
    { Element.Pre.tag; attrs = Map.of_alist_exn (module String) attrs; child }
;;

let%expect_test "mount simple" =
  let a = make_element "div" ~attrs:[ "a", "b" ] in
  let (_ : _) = Vdm.mount a ~send in
  [%expect {|
    making div
    a = b |}]
;;

let%expect_test "diff simple" =
  let a = make_element "div" ~attrs:[ "a", "b" ] in
  let b = make_element "span" ~attrs:[ "a", "c"; "x", "y" ] in
  let a = Vdm.mount a ~send in
  let (_ : _) = Vdm.diff a b ~send in
  [%expect
    {|
    making div
    a = b
    destroying
    making span
    a = c
    x = y |}]
;;
