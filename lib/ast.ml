type lc_type = TInt | TBool | TUnit | TLambda of lc_type * lc_type | TIllTyped

let rec print_lc_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TLambda (t1, t2) -> (print_lc_type t1) ^ " -> " ^ (print_lc_type t2)
  | TIllTyped -> "any"

type lc_expr =
  | EVar of string
  | ELambda of string * lc_type * lc_expr
  | EApp of lc_expr * lc_expr
  | EBool of bool
  | EInt of int
  | EUnit

let rec print_lc_expr : lc_expr -> string = function
  | EVar id -> id
  | ELambda (id, t, body) -> "&" ^ id ^ " : " ^ print_lc_type t ^ " . " ^ print_child body
  | EApp (e1, e2) -> (print_child e1) ^ " " ^ (print_child e2)
  | EUnit -> "()"
  | EInt i -> string_of_int i
  | EBool b -> string_of_bool b
and print_child = function
  | EVar id -> id
  | c -> "(" ^ print_lc_expr c ^ ")"

let alpha_equiv (expr1 : lc_expr) (expr2 : lc_expr) : bool =
  let lookup id vars =
    let rec help ind n = function
      | [] -> n
      | v :: vs ->
        if v = n then "$" ^ string_of_int ind else help (ind + 1) n vs in
    help 0 id vars in
  let rec de_bruijn vars = function
    | EVar id -> EVar (lookup id vars)
    | ELambda (p, t, b) -> ELambda ("", t, de_bruijn (p :: vars) b)
    | EApp (e1, e2) -> EApp (de_bruijn vars e1, de_bruijn vars e2)
    | e -> e in
  let is_free id = id.[0] <> '$' in
  let rec helper e1 e2 =
    match (e1, e2) with
    | (EVar id1, EVar id2) -> id1 = id2 || (is_free id1 && is_free id2)
    | (ELambda (_, _, b1), ELambda (_, _, b2)) -> helper b1 b2
    | (EApp (el1, er1), EApp (el2, er2)) -> helper el1 el2 && helper er1 er2
    | _ -> false in
  helper (de_bruijn [] expr1) (de_bruijn [] expr2)
