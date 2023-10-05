open Ast
open Grammar
open Grammar_ctx

exception ParseError of string

let parse (src : string) : lc_expr =
  t_ctx := [];
  src |> Lexing.from_string |> Grammar.start Lexer.tok

let string_of_token = function
  | Id s -> s
  | Bool b -> string_of_bool b
  | Integer i -> string_of_int i
  | LParen -> "("
  | RParen -> ")"
  | Lambda -> "λ"
  | Dot -> "."
  | Equal -> "="
  | Semicolon -> ";"
  | Colon -> ":"
  | TUnitToken -> "unit"
  | TIntToken -> "int"
  | TBoolToken -> "bool"
  | Arrow -> "->"
  | EOF -> ""