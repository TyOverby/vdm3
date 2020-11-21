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

  module Do_nothing : S
  module Both (A : S) (B : S) : S

  type t = (module S)
