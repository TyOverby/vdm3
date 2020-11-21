open! Base

module Make(H : Types.H) : sig
  val create : H.Pre.t -> Value.t
end
