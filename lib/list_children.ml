open! Base

module T = struct
  module Pre = struct
    type t = Value.t array [@@deriving sexp_of]
  end

  module Post = struct
    type t = Value.Post.t array [@@deriving sexp_of]
  end

  let mount position children ~(send : Out.t) =
    let module Send = (val send) in
    let post = Array.map children ~f:(fun child ->
      let p, _pos = Value.mount position child ~send in p) in 
    post, position
  ;;

  let disqualify _ _ = false

  let diff position (pre, post) cur ~(send : Out.t) =
    let module Send = (val send) in
    assert (Array.length pre = Array.length post);
    let before_length = Array.length pre in
    let after_length = Array.length cur in
    let out_post = Array.create ~len:after_length Nop.nop_post in
    for i = 0 to min before_length after_length - 1 do
      if i = 0 then Send.first_child () else Send.next_child ();
      let post, _status = Value.diff position post.(i) cur.(i) ~send in
      out_post.(i) <- post
    done;
    if before_length > after_length
    then
      for i = after_length to before_length - 1 do
        Send.next_child ();
        let _position = Value.unmount position post.(i) ~send in
        ()
      done
    else if after_length > before_length
    then
      for i = before_length to after_length - 1 do
        Send.next_child ();
        let post, _status = Value.mount position cur.(i) ~send in
        out_post.(i) <- post
      done;
    out_post, position
  ;;

  let kill position _ previous ~(send : Out.t) =
    let module Send = (val send) in
    Array.iteri previous ~f:(fun i c ->
        if i = 0 then Send.first_child () else Send.next_child ();
        let _position = Value.unmount position c ~send in ())
    ; position
  ;;
end

include Register.Make (T)
