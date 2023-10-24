type expr =
  | Int of int
  | Mult of expr * expr
  | Plus of expr * expr
  | Minus of expr * expr
