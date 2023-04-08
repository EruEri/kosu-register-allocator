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
%type <SanAst.atom> atom

%%
san_module:
    | list(san_node) EOF { [] }

san_node:
    | EXTERNAL Identifier delimited(LPARENT, separated_list(COMMA, san_type), RPARENT) san_type option(preceded(EQUAL, String_lit)) {
        External { fn_name = $2; signature = ($3, $4); cname = $5 }
    }

atom:
    | Identifier { Variable $1 }
    | Integer_lit { Int $1 }
    | String_lit { String $1 }
    | TRUE { Boolean true }
    | FALSE { Boolean false }

san_rvalue:
    | atom { RVExpr $1 }
    | MINUS atom { RVUnary { unop = TacUminus; atom = $2 } }
    | NOT atom { RVUnary { unop = TacNot; atom = $2} }
    | DISCARD { RVDiscard }
    | LATEINIT { RVLater }

parameter_san_type:
    | SSIZE { Ssize }
    | BOOL { Boolean }
    | STRINGL { Stringl }

san_type:
    | parameter_san_type { $1 }
    | UNIT { Unit }

