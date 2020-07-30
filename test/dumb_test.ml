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
