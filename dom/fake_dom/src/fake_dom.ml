open Core_kernel

module Fake_dom () = struct
  module Unpacked_widget = struct
    type ('node, 'state) t =
      { id : ('state * 'node) Type_equal.Id.t
      ; destroy : ('state -> 'node -> unit[@sexp.opaque])
      ; mutable state : ('state[@sexp.opaque])
      }
    [@@deriving sexp_of]
  end

  type node_kind =
    | Element of element
    | Text
    | Document_fragment

  and widget_instance = T : (node, _) Unpacked_widget.t -> widget_instance

  and element =
    { tag : string
    ; attrs : string String.Table.t
    }

  and document_fragment = unit

  and node =
    { kind : node_kind
    ; mutable parent : (node option[@sexp.opaque])
    ; mutable children : node list
    ; props : Attr.any String.Table.t
    ; mutable widget : widget_instance option
    }
  [@@deriving sexp_of]

  let to_html_string node =
    let rec to_string indent_level node =
      let indent = String.init (indent_level * 4) ~f:(fun _ -> ' ') in
      match node with
      | { kind = Text; props; _ } ->
        sprintf
          "%s%s"
          indent
          (String.Table.find_exn props "nodeValue" |> Attr.any_to_string)
      | { kind = Element { tag; attrs }; children; props = _; _ } ->
        let attr_string =
          attrs
          |> String.Table.to_alist
          |> List.map ~f:(fun (k, v) -> sprintf {|%s="%s"|} k v)
          |> String.concat ~sep:" "
        in
        let children =
          children
          |> List.map ~f:(to_string (indent_level + 1))
          |> String.concat ~sep:"\n"
        in
        let attr_sep =
          if String.Table.is_empty attrs then "" else " " ^ attr_string
        in
        sprintf "%s<%s%s>\n%s\n%s</%s>" indent tag attr_sep children indent tag
      | _ -> assert false
    in
    to_string 0 node
  ;;

  let compare_node l r =
    let to_string node = node |> sexp_of_node |> Sexp.to_string in
    let l = to_string l in
    let r = to_string r in
    String.compare l r
  ;;

  module Widget = struct
    let init ~id ~init ~destroy =
      let state, node = init () in
      let t = T { id; destroy; state } in
      node.widget <- Some t;
      node
    ;;

    let same_widget l r =
      match l.widget, r.widget with
      | Some (T l), Some (T r) -> Type_equal.Id.same l.id r.id
      | _ -> false
    ;;

    let is_widget node = Option.is_some node.widget

    let update
        (type state node)
        node
        ~(id : (state * node) Type_equal.Id.t)
        ~(update : state -> node -> state * node)
      =
      match node.widget with
      | Some (T widget) ->
        let T = Type_equal.Id.same_witness_exn id widget.id in
        let state, node = update widget.state node in
        widget.state <- state;
        node
      | None -> failwith "Calling update on a non widget"
    ;;

    let destroy node =
      match node.widget with
      | Some (T widget) ->
        widget.destroy widget.state node;
        node.widget <- None
      | None -> failwith "Calling destroy on a non widget"
    ;;
  end

  module Attr = struct
    include Attr

    let apply node t =
      match t with
      | Property (s, any) -> String.Table.set node.props ~key:s ~data:any
      | Attribute (s, any) ->
        (match node.kind with
        | Element e ->
          let any = any_to_string any in
          String.Table.set e.attrs ~key:s ~data:any
        | _ -> failwith "Only elements have attributes")
    ;;
  end

  let create_document_fragment () =
    { kind = Document_fragment
    ; parent = None
    ; children = []
    ; props = String.Table.create ()
    ; widget = None
    }
  ;;

  let to_element_exn node =
    match node.kind with
    | Element e -> e
    | _ -> failwith "Node is not an element"
  ;;

  let is_child node ~parent =
    match node.parent with
    | Some p -> phys_equal p parent
    | None -> false
  ;;

  let check_is_child_exn ~parent node =
    if not (is_child node ~parent) then failwith "Node not found in parent"
  ;;

  let can_have_children node =
    match node.kind with
    | Element _ | Document_fragment -> true
    | Text -> false
  ;;

  let remove_child remove ~parent =
    assert (can_have_children parent);
    check_is_child_exn remove ~parent;
    let new_children =
      List.filter parent.children ~f:(fun node -> not (phys_equal node remove))
    in
    parent.children <- new_children;
    remove
  ;;

  let clear_previous_parent node =
    match node.parent with
    | Some old_parent -> ignore (remove_child node ~parent:old_parent : node)
    | None -> ()
  ;;

  let set_parent node parent =
    clear_previous_parent node;
    node.parent <- parent
  ;;

  let create_attr ~key ~value = Attr.create key value

  let rec append_document_fragment children ~parent =
    List.iter children ~f:(fun child ->
        ignore (append_child child ~parent : node))

  and append_child node ~parent =
    assert (can_have_children parent);
    (match node.kind with
    | Document_fragment -> append_document_fragment node.children ~parent
    | Text | Element _ ->
      set_parent node (Some parent);
      parent.children <- List.append parent.children [ node ]);
    node
  ;;

  let create_element tag attrs =
    let kind = Element { tag; attrs = String.Table.create () } in
    let node =
      { kind
      ; parent = None
      ; children = []
      ; props = String.Table.create ()
      ; widget = None
      }
    in
    List.iter attrs ~f:(fun attr -> Attr.apply node attr);
    node
  ;;

  (*
  let create_element_with_children tag attrs children =
    let new_node = create_element tag attrs in
    List.iter children ~f:(fun child ->
      ignore (append_child child ~parent:new_node : node));
    new_node
  ;;*)

  let create_text text =
    let props = String.Table.create () in
    String.Table.set props ~key:"nodeValue" ~data:(Attr.String text);
    { kind = Text; parent = None; children = []; props; widget = None }
  ;;

  (* If [a; b] is the parent and [c; d] is the fragment and before is [b]
     We iterate over the fragment left to right and insert each element before b.
     {v
     [c; d]
      ^    -> [a; c; b]  insert c before b
         ^ -> [a; c; d; b] insert d before b
     v}
  *)
  let rec insert_before_fragment ~before children ~parent =
    List.iter children ~f:(fun child ->
        ignore (insert_before ~before ~parent child : node))

  and insert_before ~before node ~parent =
    let rec insert list before =
      match list with
      | [] ->
        raise_s
          [%message
            "Node not found in parent, can't insert before"
              ~before:(before : node)
              ~node:(node : node)
              ~parent:(parent : node)]
      | hd :: tl ->
        if phys_equal hd before
        then node :: hd :: tl
        else hd :: insert tl before
    in
    assert (can_have_children parent);
    let () =
      match node.kind with
      | Document_fragment ->
        insert_before_fragment ~before node.children ~parent
      | Text | Element _ ->
        if phys_equal before node
        then check_is_child_exn ~parent node
        else (
          set_parent node (Some parent);
          parent.children <- insert parent.children before)
    in
    node
  ;;

  let rec replace_child_fragment children ~replace ~parent =
    List.iter children ~f:(fun child ->
        ignore (insert_before child ~before:replace ~parent : node));
    ignore (remove_child replace ~parent : node)

  and replace_child replacement ~replace ~parent =
    (* Raise if [replace] is not a child of [parent] *)
    assert (can_have_children parent);
    check_is_child_exn replace ~parent;
    let () =
      match replacement.kind with
      | Document_fragment ->
        replace_child_fragment replacement.children ~replace ~parent
      | Text | Element _ ->
        set_parent replacement (Some parent);
        let new_children =
          List.map parent.children ~f:(fun node ->
              if phys_equal node replace then replacement else node)
        in
        (* Now remove [replace] from [parent] *)
        set_parent replace None;
        parent.children <- new_children
    in
    replace
  ;;

  let set_attribute element ~key ~value =
    let key = String.lowercase key in
    String.Table.set element.attrs ~key ~data:value
  ;;

  let set_property node ~key ~value =
    String.Table.set node.props ~key ~data:value
  ;;

  let get_property node ~key = String.Table.find_exn node.props key

  let remove_attribute element ~key =
    let key = String.lowercase key in
    String.Table.remove element.attrs key
  ;;

  let attributes element = element.attrs
  let children node = node.children

  let parent node =
    match node.parent with
    | Some x -> x
    | None -> failwith "Child has no parent"
  ;;

  let tag element = element.tag

  (** An empty body node.
      Used to test patching/mounting *)
  let body_node =
    let element = { tag = "body"; attrs = String.Table.create () } in
    { kind = Element element
    ; parent = None
    ; children = []
    ; props = String.Table.create ()
    ; widget = None
    }
  ;;
end
