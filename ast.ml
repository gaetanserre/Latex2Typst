(** Abstract Syntax Tree for the LaTeX → Typst converter *)

type expr =
  | Text of string
  | Func of string * expr list list
  | Env of string * expr list list * expr list
  | Subscript of expr list
  | Superscript of expr list
  | Quote of expr list
  | DoubleQuote of expr list
