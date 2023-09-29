type lc_expr =
  | EVar of string
  | ELambda of string * lc_expr
  | EApp of lc_expr * lc_expr

let rec print_lc_expr : lc_expr -> string = function
  | EVar id -> id
  | ELambda (id, body) -> "&" ^ id ^ ". " ^ print_child body
  | EApp (e1, e2) -> (print_child e1) ^ " " ^ (print_child e2)
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
    | ELambda (p, b) -> ELambda ("", de_bruijn (p :: vars) b)
    | EApp (e1, e2) -> EApp (de_bruijn vars e1, de_bruijn vars e2) in
  let is_free id = id.[0] <> '$' in
  let rec helper e1 e2 =
    match (e1, e2) with
    | (EVar id1, EVar id2) -> id1 = id2 || (is_free id1 && is_free id2)
    | (ELambda (_, b1), ELambda (_, b2)) -> helper b1 b2
    | (EApp (el1, er1), EApp (el2, er2)) -> helper el1 el2 && helper er1 er2
    | _ -> false in
  helper (de_bruijn [] expr1) (de_bruijn [] expr2)
