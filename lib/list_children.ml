open! Base

module T = struct
  module Pre = struct
    type t = Register.Value.t array [@@deriving sexp_of]
  end

  module Post = struct
    type t = Register.Post_value.t array [@@deriving sexp_of]
  end

  let mount children ~(send : Register.Out.t) =
    let module Send = (val send) in
    children, Array.map children ~f:(Register.mount ~send)
  ;;

  let disqualify _ _ = false
  let diff (_, _previous) _current ~send:_ = assert false

  let kill _ previous ~(send : Register.Out.t) =
    let module Send = (val send) in
    Array.iteri previous ~f:(fun i c ->
        if i = 0 then Send.first_child () else Send.next_child ();
        Register.unmount c ~send)
  ;;
end

let make = Staged.unstage (Register.register (module T))
let create all = make all
