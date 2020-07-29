open! Base
module Register = Register
module Element = Element
module List_children = List_children
module Value = Register.Value
module Post = Register.Post_value
module Nop = Nop

let mount = Register.mount
let diff = Register.diff
let unmount = Register.unmount
