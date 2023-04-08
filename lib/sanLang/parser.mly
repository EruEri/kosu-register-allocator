%{
    open SanAst
%}

%token <Nativeint.t> Integer_lit
%token <string> Identifier
%token <string> String_lit
%token LPARENT RPARENT
%token SSIZE BOOL STRINGL UNIT COMMA COLON
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
    | DISCARD preceded(COLON, san_type) { RVDiscard $2 }
    | LATEINIT preceded(COLON, san_type) { RVLater $2 }
    | atom { RVExpr $1 }
    | MINUS atom { RVUnary { unop = TacUminus; atom = $2 } }
    | NOT atom { RVUnary { unop = TacNot; atom = $2} }
    | atom PLUS atom { 
        binop = TacSelf TacAdd;
        blhs = $1;
        brhs = $3
    }
    | atom MINUS atom { 
        binop = TacSelf TacMinus;
        blhs = $1;
        brhs = $3
    }
    | atom MULT atom { 
        binop = TacSelf TacMult;
        blhs = $1;
        brhs = $3
    }
    | atom DIV atom {
        binop = TacSelf TacDiv;
        blhs = $1;
        brhs = $3
    }
    | atom PIPE atom {
        binop = TacSelf TacBitwiseOr;
        blhs = $1;
        brhs = $3
    }
    | atom AMPERSAND atom {
        binop = TacSelf TacBitwiseAnd;
        blhs = $1;
        brhs = $3
    }
    | atom XOR atom {
        binop = TacSelf TacBitwiseXOr;
        blhs = $1;
        brhs = $3
    }
    | atom SHIFTLEFT atom {
        binop = TacSelf TacShiftLeft;
        blhs = $1;
        brhs = $3
    }
    | atom SHIFTRIGHT atom {
        binop = TacSelf TacShiftRight;
        blhs = $1;
        brhs = $3
    }
    | atom OR atom {
        binop = TacBool TacOr;
        blhs = $1;
        brhs = $3
    }
    | atom AND atom {
        binop = TacBool TacAnd;
        blhs = $1;
        brhs = $3
    }
    | atom SUP atom {
        binop = TacBool TacSup;
        blhs = $1;
        brhs = $3
    }
    | atom SUPEQ atom {
        binop = TacBool TacSupEq;
        blhs = $1;
        brhs = $3
    }
    | atom INF atom {
        binop = TacBool TacInf;
        blhs = $1;
        brhs = $3
    }
    | atom INFEQ atom {
        binop = TacBool TacInfEq;
        blhs = $1;
        brhs = $3
    }
    | atom DOUBLEQUAL atom {
        binop = TacBool TacEqual;
        blhs = $1;
        brhs = $3
    }
    | atom DIF atom {
        binop = TacBool TacDiff;
        blhs = $1;
        brhs = $3
    }
    | Identifier delimited(LPARENT, separated_list(COMMA, atom) ,RPARENT) {
        RVFunctionCall { fn_name = $1; parameters = $2 }
    }
    
san_statement:
    | Identifier EQUAL san_rvalue {
        SSDeclaration ($1, $3)
    }

parameter_san_type:
    | SSIZE { Ssize }
    | BOOL { Boolean }
    | STRINGL { Stringl }

san_type:
    | parameter_san_type { $1 }
    | UNIT { Unit }

