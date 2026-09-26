type id = string
type binop = Plus | Minus | Times | Div
type table = (id * int) list

type stm =
  | CompundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

val prog : stm
val maxargs : stm -> int
val lookup : table * id -> int
val interp : stm -> unit
