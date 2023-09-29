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
        (ELambda ("x", (EVar "x")))
        (parse "&x.x"));
    "application" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp (ELambda ("x", (EVar "x")), EVar "y"))
        (parse "(&x.x) y"));
    "apply inside lambda" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", EApp (EVar "x", EVar "y")))
        (parse "&x.x y"));
    "naming" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp (ELambda ("fun", (EApp (EVar "fun", EVar "v"))),
               ELambda ("x", ELambda ("y", EVar "x"))))
        (parse "fun = &x. &y. x; fun v"));
    "naming church numerals" >::
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
                "succ (succ zero)")));
  ]

let reduce_tests = "test suite for the reduction engine" >::: [
    "id" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EVar "y")
        (reduce (parse "(&x.x)y")));
    "two" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (parse "&f. &x. f (f x)")
        (reduce (parse ("zero = &f. &x. x;" ^
                        "succ = &n. &f. &x. f (n f x);" ^
                        "succ (succ zero)"))));
    "shadowing" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", EVar "x"))
        (reduce (parse "(&x. &x. x) y")));
  ]

let tests = "test_suite for lambda calculus" >::: [
    lex_tests;
    equiv_tests;
    parse_tests;
    reduce_tests;
  ]

let _ = run_test_tt_main tests
