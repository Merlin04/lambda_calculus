open Ast
open Grammar

exception ParseError of string

let parse (src : string) : lc_expr =
  src |> Lexing.from_string |> Grammar.start Lexer.tok

let string_of_token = function
  | Id s -> s
  | LParen -> "("
  | RParen -> ")"
  | Lambda -> "λ"
  | Dot -> "."
  | Equal -> "="
  | Semicolon -> ";"