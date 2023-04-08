{
    open Parser
    open Lexing

    let keywords = Hashtbl.create 6
    let () = 
    [("if", IF); ("goto", GOTO); ("def", DEF); ("external", EXTERNAL);  ("end", END);
        ("true", TRUE); ("false", FALSE); ("discard", DISCARD); ("lateinit", LATEINIT)
    ] |> List.iter (fun (s, k) -> Hashtbl.replace keywords s k)


    let next_line_and f lexbuf =
        Lexing.new_line lexbuf;
        f lexbuf
}

let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']

let label = (loLetter | upLetter) (loLetter | upLetter | '.' | digit | '_')*

let identifiant = (loLetter | '_') (loLetter | upLetter | digit | "_")*

let decimal_integer = digit (digit | '_')*
let hex_integer = '0' ('x' | 'X') (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'] | '_')*
let octal_intger = '0' ('o' | 'O') (['0'-'7']) (['0'-'7'] | '_')*
let binary_integer = '0' ('b' | 'B') ('0' | '1') ('0' | '1' | '_')*
let number = ('-')? decimal_integer | hex_integer | octal_intger | binary_integer
let tmp_variable = "$tmp" digit


let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']
let whitespace = [' ' '\t' '\r' '\n']+


rule token = parse
| '\n' {
    let _ = Lexing.new_line lexbuf in
    ENDLINE
}
| '\t' {
    TAB
}
| ' ' | '\r' {
    token lexbuf
}
| "&"  { AMPERSAND }
| "^" { XOR }
| "&&" { AND }
| "|" { PIPE }
| "||" { OR }
| "==" { DOUBLEQUAL }
| "!=" { DIF }
| "!" { NOT }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT } 
| "/" { DIV }
| "%" { MOD }
| "<" { INF }
| "<=" { INFEQ }
| ">=" { SUPEQ }
| ">" { SUP }
| "<<" { SHIFTLEFT }
| ">>" { SHIFTRIGHT }
| (number as n) {
    Integer_lit(Nativeint.of_string n)
}
| identifiant as s {
    match Hashtbl.find_opt keywords s with
    | Some keyword -> keyword
    | None -> Identifier s
}
| eof { EOF }