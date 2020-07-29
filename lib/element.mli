open! Base

val create
  :  tag:string
  -> attrs:string Map.M(Base.String).t
  -> children:Register.Value.t
  -> Register.Value.t
