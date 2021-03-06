open! Core_kernel

module Fake_dom () : sig
  type node [@@deriving sexp_of, compare]
  type element
  type document_fragment

  module Attr : sig
    type any [@@deriving sexp_of, equal]
    type t

    val to_any : 'a -> any
    val string_to_any : string -> any
    val any_to_string : any -> string

    (** [create name value] creates a simple string-only attribute *)
    val create : string -> string -> t

    (** [property name value] creates a property with a generic value *)
    val property : string -> any -> t

    val apply : node -> t -> unit
  end

  module Widget : sig
    val init
      :  id:('state * node) Type_equal.Id.t
      -> init:(unit -> 'state * node)
      -> destroy:('state -> node -> unit)
      -> node

    val destroy : node -> unit
    val same_widget : node -> node -> bool
    val is_widget : node -> bool

    val update
      :  node
      -> id:('state * node) Type_equal.Id.t
      -> update:('state -> node -> 'state * node)
      -> node
  end

  (** https://developer.mozilla.org/en-US/docs/Web/API/Document/createAttribute *)
  val create_attr : key:string -> value:string -> Attr.t

  (** https://developer.mozilla.org/en-US/docs/Web/API/Document/createTextNode *)
  val create_text : string -> node

  (** https://developer.mozilla.org/en-US/docs/Web/API/Document/createDocumentFragment *)
  val create_document_fragment : unit -> node

  (** https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement *)
  val create_element : string -> Attr.t list -> node

  (** https://developer.mozilla.org/en-US/docs/Web/API/Node/insertBefore *)
  val insert_before : before:node -> node -> parent:node -> node

  (** https://developer.mozilla.org/en-US/docs/Web/API/Node/replaceChild *)
  val replace_child : node -> replace:node -> parent:node -> node

  (** https://developer.mozilla.org/en-US/docs/Web/API/Node/removeChild *)
  val remove_child : node -> parent:node -> node

  (** https://developer.mozilla.org/en-US/docs/Web/API/Node/appendChild *)
  val append_child : node -> parent:node -> node
  
  (** https://developer.mozilla.org/en-US/docs/Web/API/Node/appendChild *)
  val prepend_child : node -> parent:node -> node

  (** https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute *)
  val set_attribute : element -> key:string -> value:string -> unit

  (** https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute *)
  val set_property : node -> key:string -> value:Attr.any -> unit

  (** https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttribute *)
  val remove_attribute : element -> key:string -> unit

  (** https://developer.mozilla.org/en-US/docs/Web/API/Document/body *)
  val body_node : node

  (** https://developer.mozilla.org/en-US/docs/Web/API/ParentNode/children *)
  val children : node -> node list

  val attributes : element -> string String.Table.t

  (** https://developer.mozilla.org/en-US/docs/Web/API/Node/parentNode *)
  val parent : node -> node

  val get_property : node -> key:string -> Attr.any
  val tag : element -> string
  val to_element_exn : node -> element
  val to_html_string : node -> string
end
