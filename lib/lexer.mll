{

open Grammar

let sb = Buffer.create 256

exception SyntaxError of string

}

let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let whitespace = [' ' '\t' '\r' '\n']+


rule tok = parse
| whitespace { tok lexbuf }
| '(' { LParen }
| ')' { RParen }
| '=' { Equal }
| "lambda" { Lambda }
| '&' { Lambda }
| "λ" { Lambda }
| '.' { Dot }
| ';' { Semicolon }
| "/*" { comment lexbuf }
| id as s { Id s }
| eof { EOF }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
| "*/" { tok lexbuf }
| _ { comment lexbuf }

{

let tokenize (s : string) : token list =
  let buf = Lexing.from_string s in
  let rec loop acc = match tok buf with
    | EOF -> List.rev acc
    | t -> loop (t :: acc) in
  loop []

}