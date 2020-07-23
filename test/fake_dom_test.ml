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

    let push () = assert false

    let pop () =
      match !stack with
      | [] -> assert false
      | _ :: rest -> stack := rest
    ;;
  end
  in
  (module Out : Vdm.Out.S)
;;
