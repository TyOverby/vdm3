
  module type S = sig
    val create_element : tag:string -> unit
    val create_text : string -> unit
    val set_attribute : string -> string -> unit
    val remove_attribute : string -> unit
    val replace: unit -> unit
    val first_child : unit -> unit
    val next_child : unit -> unit
    val prepend_child: unit -> unit
    val append_child : unit -> unit
    val pop : unit -> unit
    val reset : unit -> unit
  end

type t = (module S)

module Do_nothing : S = struct
  let create_element ~tag:_ = ()
  let create_text _text = ()
  let set_attribute _key _data = ()
  let remove_attribute _key = ()
  let replace () = ()
  let prepend_child () = ()
  let append_child () = ()
  let pop () = ()
  let first_child () = ()
  let next_child () = ()
  let reset () = ()
end

module Both (A : S) (B : S) = struct
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

  let first_child key =
    A.first_child key;
    B.first_child key
  ;;

  let next_child key =
    A.next_child key;
    B.next_child key
  ;;

  let replace () =
    A.replace ();
    B.replace ()
  ;;

  let prepend_child () =
    A.prepend_child ();
    B.prepend_child ()
  ;;

  let append_child () =
    A.append_child ();
    B.append_child ()
  ;;

  let pop () =
    A.pop ();
    B.pop ()
  ;;

  let reset () =
    A.reset ();
    B.reset ()
  ;;
end
