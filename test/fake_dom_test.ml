open! Core_kernel

let out () =
  let module Fake_dom = Fake_dom.Fake_dom () in
  let stack = ref [] in
  let top = ref Fake_dom.body_node in
  let module Out : Vdm.Out.S = struct
    let reset () =
      top := Fake_dom.body_node;
      stack := []
    ;;

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

    let prepend_child () =
      match !stack with
      | [] -> assert false
      | parent :: rest ->
        let (_ : Fake_dom.node) = Fake_dom.prepend_child !top ~parent in
        stack := rest;
        top := parent
    ;;

    let replace () =
      match !stack with
      | replacee :: replace :: rest -> 
          let parent = Fake_dom.parent replace in
          let _:_ = Fake_dom.replace_child ~replace ~parent replacee in
          stack := rest
      | _ -> assert false
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
        List.findi children_of_parent ~f:(fun _ -> phys_equal !top)
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
  let module O = Vdm.Out.Both (Out) (Dumb_test.Dummy) in
  ( (module O : Vdm.Out.S)
  , fun () -> Fake_dom.to_html_string Fake_dom.body_node )
;;

let run_mount_test element =
  let send, html_string = out () in
  let module Send = (val send) in
  let (_ : Vdm.Post.t * _) = Vdm.mount `Parent element ~send in
  print_endline "------------------------";
  print_endline (html_string ())
;;

let run_diff_test e1 e2 =
  let send, html_string = out () in
  let module Send = (val send) in
  let post, _ = Vdm.mount `Parent e1 ~send in
  print_endline "------------------------";
  Send.first_child ();
  let (_ : Vdm.Post.t * _) = Vdm.diff `Parent post e2 ~send in
  print_endline "------------------------";
  print_endline (html_string ())
;;

let ele ?(attrs = []) ?(chdn = Vdm.none) tag =
  Vdm.Element.create ~tag ~attrs:(String.Map.of_alist_exn attrs) ~children:chdn
;;

let lst children = Vdm.List_children.create (Array.of_list children)

let%expect_test "single div" =
  run_mount_test (ele "div" ~attrs:[ "a", "aaa"; "b", "bbbb" ]);
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

let%expect_test "add an attribute" =
  run_diff_test (ele "div") (ele "div" ~attrs:[ "a", "aaa" ]);
  [%expect
    {|
    (create_element (tag div))
    append_child
    ------------------------
    first_child
    (set_attribute (key a) (data aaa))
    ------------------------
    <body>
        <div a="aaa">

        </div>
    </body> |}]
;;

let%expect_test "nested div" =
  run_mount_test
    (ele "div" ~attrs:[ "a", "aaa"; "b", "bbbb" ] ~chdn:(ele "span"));
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
  run_mount_test
    (ele
       "div"
       ~attrs:[ "a", "aaa"; "b", "bbbb" ]
       ~chdn:(ele "span" ~chdn:(ele "a")));
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

let%expect_test "empty nesting via children" =
  run_mount_test
    (ele
       "div"
       ~attrs:[ "a", "aaa"; "b", "bbbb" ]
       ~chdn:(Vdm.List_children.create [||]));
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

let%expect_test "nesting via children" =
  run_mount_test
    (ele "div" ~attrs:[ "a", "aaa"; "b", "bbbb" ] ~chdn:(lst [ ele "span" ]));
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
  run_mount_test
    (ele
       "div"
       ~attrs:[ "a", "aaa"; "b", "bbbb" ]
       ~chdn:(lst [ ele "span"; ele "a" ]));
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

let%expect_test "diff inside children" =
  run_diff_test
    (ele "div" ~chdn:(lst [ ele "span" ]))
    (ele "div" ~chdn:(lst [ ele "span" ~attrs:[ "a", "aa" ] ]));
  [%expect
    {|
    (create_element (tag div))
    (create_element (tag span))
    append_child
    append_child
    ------------------------
    first_child
    first_child
    (set_attribute (key a) (data aa))
    ------------------------
    <body>
        <div>
            <span a="aa">

            </span>
        </div>
    </body> |}]
;;

let%expect_test "no children to some children" =
  run_diff_test (ele "div") (ele "div" ~chdn:(ele "span"));
  [%expect
    {|
    (create_element (tag div))
    append_child
    ------------------------
    first_child
    (create_element (tag span))
    append_child
    ------------------------
    <body>
        <div>
            <span>

            </span>
        </div>
    </body> |}]
;;

let%expect_test "no children to some children via list" =
  run_diff_test (ele "div") (ele "div" ~chdn:(lst [ ele "span" ]));
  [%expect
    {|
    (create_element (tag div))
    append_child
    ------------------------
    first_child
    (create_element (tag span))
    append_child
    ------------------------
    <body>
        <div>
            <span>

            </span>
        </div>
    </body> |}]
;;

(*_
let%expect_test "append to children" =
  run_diff_test
    (ele "div" ~chdn:(lst [ ele "span" ]))
    (ele "div" ~chdn:(lst []));
  [%expect {||}]


let%expect_test "append to children" =
  run_diff_test
    (ele "div" ~chdn:(lst []))
    (ele "div" ~chdn:(lst [ ele "span" ]));
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Invalid_argument "List.nth_exn 1 called on list of length 1")
  Raised at file "stdlib.ml", line 30, characters 20-45
  Called from file "test/fake_dom_test.ml", line 60, characters 25-64
  Called from file "lib/register.ml", line 58, characters 8-24
  Called from file "lib/list_children.ml", line 39, characters 8-26
  Called from file "lib/register.ml", line 174, characters 17-51
  Called from file "lib/element.ml", line 57, characters 6-35
  Called from file "lib/register.ml", line 174, characters 17-51
  Called from file "test/fake_dom_test.ml", line 87, characters 25-51
  Called from file "test/fake_dom_test.ml", line 319, characters 2-87
  Called from file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  (create_element (tag div))
  append_child
  ------------------------
  first_child |}]
;;
*)
