open! Base

val create
  :  tag:string
  -> attrs:string Map.M(Base.String).t
  -> children:Value.t
  -> Value.t
