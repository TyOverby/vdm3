open! Base

module Nop = struct
  module Pre = Unit
  module Post = Unit

  let disqualify ((), ()) () = false
  let mount () ~send:_ = (), ()
  let diff ((), ()) () ~send:_ = (), ()
  let kill () () ~send:_ = ()
end

let make = Staged.unstage (Register.register (module Nop))
let nop = make ()
