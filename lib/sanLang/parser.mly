%{
    open SanAst
%}

%token <Nativeint.t> Integer_lit
%token <string> Identifier
%token <string> String_lit
%token LPARENT RPARENT
%token SSIZE BOOL STRINGL UNIT COMMA
%token IF GOTO DEF EXTERNAL TRUE FALSE DISCARD LATEINIT END
%token OR TAB ENDLINE
%token PIPE
%token AND
%token XOR 
%token AMPERSAND EQUAL
%token DOUBLEQUAL DIF
%token SUP SUPEQ INF INFEQ
%token SHIFTLEFT SHIFTRIGHT
%token PLUS MINUS
%token MULT DIV MOD
%token EOF
%token NOT

%start san_module

%type <SanAst.san_module list> san_module

%%
san_module:
    | list(san_node) EOF { [] }

san_node:
    | EXTERNAL Identifier delimited(LPARENT, separated_list(COMMA, san_type), RPARENT) san_type option(preceded(EQUAL, String_lit)) {
        External { fn_name = $2; signature = ($3, $4); cname = $5 }
    }

parameter_san_type:
    | SSIZE { Ssize }
    | BOOL { Boolean }
    | STRINGL { Stringl }

san_type:
    | parameter_san_type { $1 }
    | UNIT { Unit }

