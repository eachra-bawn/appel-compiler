type id = string
type binop = Plus | Minus | Times | Div

type stm =
  | CompundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let prog =
  CompundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

(* Write an ML function (maxargs : stm -> int) that tells the maximum
number of arguments of any print statement within any subexpression of
a given statement. For example, maxargs (prog) is 2. *)
(* This was from Copilot *)
let maxargs (statement : stm) : int =
  let rec maxargs_stm = function
    | CompundStm (stm1, stm2) -> max (maxargs_stm stm1) (maxargs_stm stm2)
    | AssignStm (_, expression) -> maxargs_exp expression
    | PrintStm expressions ->
        max (List.length expressions)
          (List.fold_left
             (fun maximum expression -> max maximum (maxargs_exp expression))
             0 expressions)
  and maxargs_exp = function
    | IdExp _ | NumExp _ -> 0
    | OpExp (left, _, right) -> max (maxargs_exp left) (maxargs_exp right)
    | EseqExp (statement, expression) ->
        max (maxargs_stm statement) (maxargs_exp expression)
  in
  maxargs_stm statement

let () =
  let prog_maxargs = maxargs prog in
  Printf.printf "%d\n" prog_maxargs
