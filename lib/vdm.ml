open! Base
module Register = Register
module Element = Element
module Value = Register.Value
module Post = Register.Post_value

let mount = Register.mount
let diff = Register.diff
let unmount = Register.unmount
