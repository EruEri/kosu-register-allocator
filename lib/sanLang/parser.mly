%{
    
%}

%token <Nativeint.t> Integer_lit
%token <string> Identifier
%token IF GOTO DEF EXTERNAL TRUE FALSE DISCARD LATEINIT END
%token OR TAB ENDLINE
%token PIPE
%token AND
%token XOR 
%token AMPERSAND
%token DOUBLEQUAL DIF
%token SUP SUPEQ INF INFEQ
%token SHIFTLEFT SHIFTRIGHT
%token PLUS MINUS
%token MULT DIV MOD
%token EOF
%token NOT

%start san_module

%type <int list> san_module

%%
san_module:
    | EOF { [] }