open! Base
module Register = Register
module Element = Element
module List_children = List_children
module Value = Value
module Post = Value.Post
module Out = Out

let none = Nop.nop
let mount = Value.mount
let diff = Value.diff
let unmount = Value.unmount
