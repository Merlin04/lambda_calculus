# Exercises
## Design

Note: this question is pretty open-ended. My goal is to prompt you to think creatively about programming language design, as well as to get you to use the tools we've developed in class. There is no single correct answer.

For this exercise, we'll start out in our designer role, without worrying about implementation.

We've been using OCaml for a few weeks now, and you probably have started to develop some opinions about it. Describe a language feature you'd like to add to OCaml. (If you've ever been coding for this class and you've though to yourself "I really wish I could ... in OCaml" that's a great starting point.) What syntax would you use to express the feature you're adding? What are the semantics you want the code to have? Use our specification tools (like grammars and inference rules) as appropriate to communicate your ideas. It's okay if you don't exactly know how to describe your feature formally. Imagine that I'm a language implementer and your goal is to describe your idea to me well enough that I can go and implement it without needing to ask clarifying questions.

Now let's take a walk over to the world of implementation. (NOTE: This is a theoretical question. You do not have to write any code.)

How would you go about implementing the feature you described above? It's okay if you don't know exactly, but I want to hear your thought process. Could you write it as a function or set of functions? Would you need to modify the parser? Would you need to change the interpreter? (OCaml is compiled, but since we haven't talked about compilers yet, you can pretend it's interpreted for this question.) Try to estimate how difficult this would be. It's okay to propose features you think would be hard to implement--you don't actually have to do the implementation.


A language feature I'd like to add to OCaml would be first class constructors, where, for some constructor `C of 'a * 'b * ...` of type `t`, when referencing `C` as an expression without calling it, it evaluates to a function `fun a b ... -> C(a, b, ...)`. This feature would make certain kinds of abstraction far more elegant - for example, you could make a function that parses a dyadic operator then use that to create parser functions for multiple operators in your language:

```ocaml
type expr =
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Value of int

let parse = (* ... *)
and dyadic (op : tok) (con : expr -> expr -> expr) (t : tok list) : (expr, tok list) =
  let e1, r1 = parse t in
  match r1 with
    | o :: r2 when o = op -> let e2, r3 = parse r2 in ((con e1 e2), r3)
    | _ -> failwith "unexpected token, expected operator"
and parse_plus (t : tok list) = dyadic plus_tok Plus t
(* ... *)  
```

Without this feature, you would need to write the function that `Plus` evaluates to manually:

```ocaml
and parse_plus (t : tok list) = dyadic plus_tok (fun a b -> Plus (a, b)) t
```

I'm not sure exactly how OCaml's parser works, but syntax errors suggest that identifiers with capital letters are treated as constructors. If this distinction is made in the parser, it would have to be modified to move that check into a context where the type of the constructor is known, so that if the constructor `C` has some associated data, `C` evaluates to the constructor function, but if it doesn't, it just evaluates to the constructor value (since there aren't any parameters you could provide, so no need for a function to construct it).

However, it seems more likely the distinction is made in the type-checker. In this case, we would need to add rules such that a constructor that holds data but is not being invoked should evaluate to the function that creates an instance of it. I'm not sure how to write inference rules to make this sort of check, but it would probably look something like this (where `CCtx` is some context holding key-value pairs of constructors and their associated data type):

```
(C, args) in CCtx    !(args -> null)
---------------------------------------------------------
CCtx |- C -> (fun args -> C args)    C : (args -> C args)
```

It's important to note that this rule would need to take lower precedence than the "traditional" constructor rule, so that expressions like `C (a, b)` don't evaluate to an application of the function `C` with arguments `a, b` (as that would likely be less efficient).

Then, pretending OCaml had an interpreter, you'd have to modify the interpreter to evaluate the expression appropriately when it matches this rule.

Overall, this feature seems moderately difficult to implement, depending on the implementation of the OCaml compiler. It would need significant changes to the type-checker and compiler/interpreter to allow this as a valid usage of a constructor.