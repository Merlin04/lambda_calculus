%{

open Ast
open Reducer
open Grammar_ctx

type t_binding = { id : string; term : lc_expr; term_type : lc_type }

%}

%token <string> Id
%token LParen
%token RParen
%token Lambda
%token Dot
%token Equal
%token Semicolon
%token <bool> Bool
%token Colon
%token Arrow
%token <int> Integer
%token TIntToken
%token TBoolToken
%token TUnitToken
%token EOF

%start <lc_expr> start

%type <lc_expr> program term rest
%type <t_binding> binding
%type <lc_type> type_ref

%left Equal
%left Dot
%right Arrow
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
  | b = binding; p = program { EApp (ELambda (b.id, b.term_type, p), b.term) }

binding :
  | i = Id; Equal; t = term; Semicolon { { id = i; term = t; term_type = (let tt = get_type !t_ctx t in t_ctx := (i, tt) :: !t_ctx; tt) } }

term :
  | Lambda; i = Id; Colon; t = type_ref; Dot; e = term { ELambda (i, t, e) }
  | t1 = term; t2 = rest { EApp (t1, t2) } (* unclear how to make the parser handle left-recursion by itself when
                                              there's no token between the two expressions to set as left-associative *)
  | r = rest { r }

type_ref :
  | t1 = type_ref; Arrow; t2 = type_ref { TLambda (t1, t2) }
  | TIntToken { TInt }
  | TBoolToken { TBool }
  | TUnitToken { TUnit }
  | LParen; t = type_ref; RParen { t }

rest :
  | i = Id { EVar (i) }
  | LParen; RParen { EUnit }
  | i = Integer { EInt (i) }
  | b = Bool { EBool (b) }
  | LParen; t = term; RParen { t }