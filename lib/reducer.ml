open Ast

let next_var = ref 0
let var_map = ref []

let fresh_var (r : string) : string =
  next_var := !next_var + 1;
  let f = "x$" ^ string_of_int !next_var in
  var_map := (f, r) :: !var_map;
  f

(* this function is really messy, imperative code bleh *)
(* after I finished writing it I realized I could probably just have modified fresh_var to generate new variables *)
(* based on the input (just increasing the amount of ' added, and having counters for separate vars *)
(* but oh well it works so I won't rewrite it, it's 1:08 AM and I am sleepy *)
(* ...why did I stay up to work on this the assignment is due in 5 days I have plenty of time *)
let re_replace_vars (e : lc_expr) : lc_expr =
  let used_maps = ref [] in
  let rec get_replacement (r : string) =
    match List.assoc_opt r !var_map with
    | Some o -> (
        if String.starts_with ~prefix:"x$" o then get_replacement o else
        let rec find_next (v : string) =
          match List.assoc_opt v !used_maps with
            | Some a when a <> r -> find_next (v ^ "'")
            | Some a when a = r -> v
            | None -> used_maps := (v, r) :: !used_maps; v
        in
        find_next o
      )
    | None -> r (* this shouldn't happen but it does sometimes *)
  in
  let rec loop = function
    | EVar a -> EVar (get_replacement a)
    | EApp (t1, t2) -> EApp (loop t1, loop t2)
    | ELambda (a, t, e) -> ELambda (get_replacement a, t, loop e)
    | e -> e
  in
  loop e

let rec subst (e : lc_expr) (t : lc_expr) (x : string) = match e with
  | EVar a when a = x -> t
  | EVar y when y <> x -> EVar y
  | EApp (t1, t2) -> EApp (subst t1 t x, subst t2 t x)
  | ELambda (a, _, _) when a = x -> e
  | ELambda (y, ty, s) -> let z = fresh_var y in
      ELambda (z, ty, subst (subst s (EVar (z)) y) t x)
  | _ -> e

let rec is_irr = function
  | EVar _ -> true (* Irr-Symb *)
  | ELambda (_, _, t) | EApp (EVar _, t) -> is_irr t (* Irr-Lambda, Irr-AppVar *)
  | EApp (EApp (t1, t2), t3) -> (is_irr (EApp (t1, t2))) && (is_irr t3) (* Irr-AppApp *)
  | _ -> false

let rec reduce_inner = function
  | t when is_irr t -> t
  | EApp (ELambda (x, _, t), s) -> subst t s x |> reduce_inner (* Reduce-Beta *)
  | ELambda (x, ty, t) when not (is_irr t) -> ELambda (x, ty, reduce_inner t) |> reduce_inner (* Reduce-Lambda *)
  | EApp (EApp (t1, t2), t3) when not (is_irr (EApp (t1, t2))) -> EApp (reduce_inner (EApp (t1, t2)), t3) |> reduce_inner (* Reduce-Left *)
  | EApp (t1, t2) when not (is_irr t2) -> EApp (t1, reduce_inner t2) |> reduce_inner (* Reduce-AppVar *)
  | EApp (EApp (t1, t2), t3) when is_irr (EApp (t1, t2)) && not (is_irr t3) -> EApp (EApp (t1, t2), reduce_inner t3) |> reduce_inner (* Reduce-AppApp *)

exception TypeError of string

let rec get_type (ctx : (string * lc_type) list) : lc_expr -> lc_type = function
  | EVar v -> (match List.assoc_opt v ctx with
    | Some t -> t
    | None -> TIllTyped)
  | EApp (a, b) -> (match get_type ctx a with
    | TLambda (t1, t2) -> let tb = get_type ctx b in
      if tb = TIllTyped || tb = t1 then t2
        else raise (TypeError ("Expected type " ^ print_lc_type t1 ^ " but got argument of type " ^ print_lc_type tb))
    | TIllTyped -> TIllTyped
    | ta -> raise (TypeError ("Expected lambda type for application but found " ^ print_lc_type ta)))
  | ELambda (a, t1, e) -> let t2 = get_type ((a, t1) :: ctx) e in TLambda (t1, t2)
  | EBool _ -> TBool
  | EInt _ -> TInt
  | EUnit -> TUnit

let reduce t = ignore (get_type [] t); t |> reduce_inner |> re_replace_vars