open! Base

module T = struct
  module Pre = Unit
  module Post = Unit

  let disqualify ((), ()) () = false
  let mount position () ~send:_ = (), position
  let diff position ((), ()) () ~send:_ = (), position
  let kill position () () ~send:_ = position
end

include Register.Make (T)

let nop = create ()
let nop_post, _ = Value.mount `Me nop ~send:(module Out.Do_nothing)
