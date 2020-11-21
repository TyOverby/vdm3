open! Base

module Make (H : Types.H) = struct
  let pre_kind = Type_equal.Id.create ~name:"pre" H.Pre.sexp_of_t
  let post_kind = Type_equal.Id.create ~name:"post" H.Post.sexp_of_t 
  let create = Value.Private.create ~pre_kind ~post_kind ~behavior:(module H)
end
