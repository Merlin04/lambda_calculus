%{

open Ast

type t_binding = { id : string; term : lc_expr }

%}

%token <string> Id
%token LParen
%token RParen
%token Lambda
%token Dot
%token Equal
%token Semicolon
%token EOF

%start <lc_expr> start

%type <lc_expr> program term rest
%type <t_binding> binding

%left Equal
%left Dot
%nonassoc Lambda

%%

(*
<program> ::= <term> | <binding> <program>
<binding> ::= $id = <term> ;
<term> ::= & $id . <application> | <application>
<application> ::= <application> <base> | <base>
<base> ::= $id | ( <term> )
*)

start :
  | p = program; EOF { p }

program :
  | t = term { t }
  | b = binding; p = program { EApp (ELambda (b.id, p), b.term) }

binding :
  | i = Id; Equal; t = term; Semicolon { { id = i; term = t } }

term :
  | Lambda; i = Id; Dot; t = term { ELambda (i, t) }
  | t1 = term; t2 = rest { EApp (t1, t2) } (* unclear how to make the parser handle left-recursion by itself when
                                              there's no token between the two expressions to set as left-associative *)
  | r = rest { r }

rest :
  | i = Id { EVar (i) }
  | LParen; t = term; RParen { t }