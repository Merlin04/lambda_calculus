open OUnit2
open Lambda_calculus.Lexer
open Lambda_calculus.Ast
open Lambda_calculus.Reducer
open Lambda_calculus.Parser
open Lambda_calculus.Grammar

let lex_tests = "test suite for lexer" >::: [
    "all tokens" >::
    (fun _ -> assert_equal ~printer:(fun l -> String.concat " " (List.map string_of_token l))
        [Lambda; Dot; Id "x"; LParen; RParen; Equal; Semicolon]
        (tokenize "&.x()=;"));
    "numbers in id" >::
    (fun _ -> assert_equal [Id "_32x"] (tokenize "_32x"));
  ]

let equiv_tests = "test suite for alpha-equivalence" >::: [
    "variable" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "x")
           (parse "y")));
    "lambda" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "&x.x")
           (parse "&y.y")));
    "lambda unequal" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (not (alpha_equiv
           (parse "&x.x")
           (parse "&y.x"))));
    "application" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "(&x.x) a")
           (parse "(&y.y) a")));
    "application right argument" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "(&x.x) (&y.y)")
           (parse "(&y.y) (&z.z)")));
    "application right argument unequal" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (not (alpha_equiv
           (parse "(&x.x) (&y.y)")
           (parse "(&y.y) (&z.a)"))));
    "nested lambda" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "(&f. &x. f x)")
           (parse "(&s. &z. s z)")));
    "nested lambda unequal" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (not (alpha_equiv
           (parse "(&f. &x. f f)")
           (parse "(&s. &z. s z)"))));
  ]

let parse_tests = "test suite for parser" >::: [
    "id" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", TInt, (EVar "x")))
        (parse "&x : int .x"));
    "application" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp (ELambda ("x", TUnit, (EVar "x")), EVar "y"))
        (parse "(&x : unit.x) y"));
    "apply inside lambda" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", TLambda (TBool, TUnit), EApp (EVar "x", EVar "y")))
        (parse "&x : bool -> unit.x y"));
    "naming" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp (ELambda ("fun", TLambda(TInt, TLambda(TBool, TInt)), (EApp (EVar "fun", EVar "v"))),
               ELambda ("x", TInt, ELambda ("y", TBool, EVar "x"))))
        (parse "fun = &x:int. &y:bool. x; fun v"));
    (*"naming church numerals" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp
           (ELambda
              ("zero",
               EApp
                 (ELambda
                   ("succ",
                    EApp (EVar "succ", EApp (EVar "succ", EVar "zero"))),
                 (ELambda
                    ("n",
                     ELambda
                       ("f",
                        ELambda
                          ("x",
                           EApp (EVar "f",
                                 EApp (EApp (EVar "n", EVar "f"),
                                       EVar "x")))))))),
         (ELambda ("f", ELambda ("x", EVar "x")))))
        (parse ("zero = &f. &x. x;" ^
                "succ = &n. &f. &x. f (n f x);" ^
                "succ (succ zero)")));*)
  ]

let reduce_tests = "test suite for the reduction engine" >::: [
    "id" >::
    (fun _ -> assert_equal ~printer:print_lc_expr ~cmp:alpha_equiv
        (EVar "y")
        (reduce (parse "(&x:int.x)y")));
    (*"two" >::
    (fun _ -> assert_equal ~printer:print_lc_expr ~cmp:alpha_equiv
        (parse "&f. &x. f (f x)")
        (reduce (parse ("zero = &f. &x. x;" ^
                        "succ = &n. &f. &x. f (n f x);" ^
                        "succ (succ zero)"))));*)
    "shadowing" >::
    (fun _ -> assert_equal ~printer:print_lc_expr ~cmp:alpha_equiv
        (ELambda ("x", TUnit, EVar "x"))
        (reduce (parse "(&x:int. &x:unit. x) y")));
  ]

let type_tests = "test suite for typechecker" >::: [
  "binding" >::
  (fun _ -> assert_raises (TypeError "Expected type int -> unit but got argument of type int -> int")
    (fun () -> (reduce (parse "a = (&x : int . x); (&x : int -> unit . x) a")))
  );
  "ill-typed variables" >::
  (fun _ -> assert_equal ~printer:print_lc_expr ~cmp:alpha_equiv
    (EVar "x")
    (reduce (parse "(& a : unit . a) x"))
  );
]

let tests = "test_suite for lambda calculus" >::: [
    lex_tests;
(*    equiv_tests; *)
    parse_tests;
    reduce_tests;
    type_tests;
  ]

let _ = run_test_tt_main tests
