open! Core_kernel

let out () =
  let module Fake_dom = Fake_dom.Fake_dom () in
  let stack = ref [] in
  let top = ref Fake_dom.body_node in
  let module Out = struct
    let push e =
      stack := !top :: !stack;
      top := e
    ;;

    let create_element ~tag = push (Fake_dom.create_element tag [])
    let create_text text = push (Fake_dom.create_text text)

    let set_attribute key data =
      Fake_dom.set_attribute (Fake_dom.to_element_exn !top) ~key ~value:data
    ;;

    let remove_attribute key =
      Fake_dom.remove_attribute (Fake_dom.to_element_exn !top) ~key
    ;;

    let append_child () =
      match !stack with
      | [] -> assert false
      | parent :: rest ->
        let (_ : Fake_dom.node) = Fake_dom.append_child !top ~parent in
        stack := rest;
        top := parent
    ;;

    let pop () =
      match !stack with
      | [] -> assert false
      | _ :: rest -> stack := rest
    ;;

    let first_child () =
      match Fake_dom.children !top with
      | hd :: _ -> push hd
      | [] -> assert false
    ;;

    let next_child () =
      let children_of_parent =
        let parent = Fake_dom.parent !top in
        Fake_dom.children parent
      in
      let me_idx =
        List.findi children_of_parent ~f:(fun _ c -> phys_equal c !top)
      in
      let next =
        match me_idx with
        | Some (i, _) -> List.nth_exn children_of_parent (i + 1)
        | None -> assert false
      in
      push next
    ;;
  end
  in
  ( Vdm.Register.Out.both
      (module Out : Vdm.Register.Out.S)
      (module Dumb_test.Dummy)
  , fun () -> Fake_dom.to_html_string Fake_dom.body_node )
;;

let run_test element =
  let out, html_string = out () in
  let module Send = (val out) in
  let (_ : Vdm.Post.t) = Vdm.mount element ~send:out in
  print_endline "------------------------";
  print_endline (html_string ())
;;

let%expect_test "single div" =
  run_test
    (Vdm.Element.create
       ~tag:"div"
       ~attrs:(String.Map.of_alist_exn [ "a", "aaa"; "b", "bbbb" ])
       ~children:Vdm.Nop.nop);
  [%expect
    {|
    (create_element (tag div))
    (set_attribute (key a) (data aaa))
    (set_attribute (key b) (data bbbb))
    append_child
    ------------------------
    <body>
        <div b="bbbb" a="aaa">

        </div>
    </body> |}]
;;

let%expect_test "nested div" =
  run_test
    (Vdm.Element.create
       ~tag:"div"
       ~attrs:(String.Map.of_alist_exn [ "a", "aaa"; "b", "bbbb" ])
       ~children:
         (Vdm.Element.create
            ~tag:"span"
            ~attrs:(String.Map.of_alist_exn [])
            ~children:Vdm.Nop.nop));
  [%expect
    {|
    (create_element (tag div))
    (set_attribute (key a) (data aaa))
    (set_attribute (key b) (data bbbb))
    (create_element (tag span))
    append_child
    append_child
    ------------------------
    <body>
        <div b="bbbb" a="aaa">
            <span>

            </span>
        </div>
    </body> |}]
;;

let%expect_test "more nesting" =
  run_test
    (Vdm.Element.create
       ~tag:"div"
       ~attrs:(String.Map.of_alist_exn [ "a", "aaa"; "b", "bbbb" ])
       ~children:
         (Vdm.Element.create
            ~tag:"span"
            ~attrs:(String.Map.of_alist_exn [])
            ~children:
              (Vdm.Element.create
                 ~tag:"a"
                 ~attrs:(String.Map.of_alist_exn [])
                 ~children:Vdm.Nop.nop)));
  [%expect
    {|
    (create_element (tag div))
    (set_attribute (key a) (data aaa))
    (set_attribute (key b) (data bbbb))
    (create_element (tag span))
    (create_element (tag a))
    append_child
    append_child
    append_child
    ------------------------
    <body>
        <div b="bbbb" a="aaa">
            <span>
                <a>

                </a>
            </span>
        </div>
    </body> |}]
;;

let%expect_test "nesting via children" =
  run_test
    (Vdm.Element.create
       ~tag:"div"
       ~attrs:(String.Map.of_alist_exn [ "a", "aaa"; "b", "bbbb" ])
       ~children:
         (Vdm.List_children.create
            [| Vdm.Element.create
                 ~tag:"span"
                 ~attrs:(String.Map.of_alist_exn [])
                 ~children:Vdm.Nop.nop
            |]));
  [%expect
    {|
    (create_element (tag div))
    (set_attribute (key a) (data aaa))
    (set_attribute (key b) (data bbbb))
    (create_element (tag span))
    append_child
    append_child
    ------------------------
    <body>
        <div b="bbbb" a="aaa">
            <span>

            </span>
        </div>
    </body> |}]
;;

let%expect_test "multiple nesting via children" =
  run_test
    (Vdm.Element.create
       ~tag:"div"
       ~attrs:(String.Map.of_alist_exn [ "a", "aaa"; "b", "bbbb" ])
       ~children:
         (Vdm.List_children.create
            [| Vdm.Element.create
                 ~tag:"span"
                 ~attrs:(String.Map.of_alist_exn [])
                 ~children:Vdm.Nop.nop
             ; Vdm.Element.create
                 ~tag:"a"
                 ~attrs:(String.Map.of_alist_exn [])
                 ~children:Vdm.Nop.nop
            |]));
  [%expect
    {|
    (create_element (tag div))
    (set_attribute (key a) (data aaa))
    (set_attribute (key b) (data bbbb))
    (create_element (tag span))
    append_child
    (create_element (tag a))
    append_child
    append_child
    ------------------------
    <body>
        <div b="bbbb" a="aaa">
            <span>

            </span>
            <a>

            </a>
        </div>
    </body> |}]
;;
