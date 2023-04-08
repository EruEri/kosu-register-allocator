%{
    open SanAst
%}

%token <Nativeint.t> Integer_lit
%token <string> Identifier
%token <string> String_lit
%token <string> Label
%token LPARENT RPARENT
%token SSIZE BOOL STRINGL UNIT COMMA COLON SEMICOLON
%token IF GOTO DEF EXTERNAL TRUE FALSE DISCARD LATEINIT END
%token OR ENDLINE RETURN
%token PIPE
%token AND
%token XOR 
%token AMPERSAND EQUAL
%token DOUBLEQUAL DIF
%token SUP SUPEQ INF INFEQ
%token SHIFTLEFT SHIFTRIGHT
%token PLUS MINUS
%token MULT DIV
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
    | san_function { $1 }

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
        RVBinary { 
            binop = TacSelf TacAdd;
            blhs = $1;
            brhs = $3 
        }
    }
    | atom MINUS atom { 
         RVBinary { 
            binop = TacSelf TacMinus;
        blhs = $1;
        brhs = $3
        }
    }
    | atom MULT atom { 
         RVBinary { 
            binop = TacSelf TacMult;
        blhs = $1;
        brhs = $3
        }
    }
    | atom DIV atom {
         RVBinary { 
            binop = TacSelf TacDiv;
        blhs = $1;
        brhs = $3
        }
    }
    | atom PIPE atom {
         RVBinary { 
            binop = TacSelf TacBitwiseOr;
        blhs = $1;
        brhs = $3
        }
    }
    | atom AMPERSAND atom {
         RVBinary { 
            binop = TacSelf TacBitwiseAnd;
        blhs = $1;
        brhs = $3
        }
    }
    | atom XOR atom {
         RVBinary { 
            binop = TacSelf TacBitwiseXor;
        blhs = $1;
        brhs = $3
        }
    }
    | atom SHIFTLEFT atom {
         RVBinary { 
            binop = TacSelf TacShiftLeft;
        blhs = $1;
        brhs = $3
        }
    }
    | atom SHIFTRIGHT atom {
         RVBinary { 
            binop = TacSelf TacShiftRight;
        blhs = $1;
        brhs = $3
        }
    }
    | atom OR atom {
         RVBinary { 
            binop = TacBool TacOr;
        blhs = $1;
        brhs = $3
        }
    }
    | atom AND atom {
         RVBinary { 
            binop = TacBool TacAnd;
        blhs = $1;
        brhs = $3
        }
    }
    | atom SUP atom {
         RVBinary { 
            binop = TacBool TacSup;
        blhs = $1;
        brhs = $3
        }
    }
    | atom SUPEQ atom {
         RVBinary { 
            binop = TacBool TacSupEq;
        blhs = $1;
        brhs = $3
        }
    }
    | atom INF atom {
         RVBinary { 
            binop = TacBool TacInf;
        blhs = $1;
        brhs = $3
        }
    }
    | atom INFEQ atom {
         RVBinary { 
            binop = TacBool TacInfEq;
        blhs = $1;
        brhs = $3
        }
    }
    | atom DOUBLEQUAL atom {
         RVBinary { 
            binop = TacBool TacEqual;
        blhs = $1;
        brhs = $3
        }
    }
    | atom DIF atom {
         RVBinary { 
            binop = TacBool TacDiff;
        blhs = $1;
        brhs = $3
        }
    }
    | Identifier delimited(LPARENT, separated_list(COMMA, atom) ,RPARENT) {
        RVFunctionCall { fn_name = $1; parameters = $2 }
    }
    
san_statement:
    | Identifier EQUAL san_rvalue SEMICOLON {
        SSDeclaration ($1, $3)
    }

san_ending:
    | RETURN atom { SE_return $2 }
    | IF atom GOTO Label GOTO Label { SE_If {expr = $2; if_label = $4; else_label = $6 } }

san_basic_block:
    | Label COLON ENDLINE nonempty_list(san_statement) option(san_ending) {
        {
            label = $1;
            statements = $4;
            ending = $5
        }
    }
starting_basic_block:
    | nonempty_list(san_statement) option(san_ending) { ($1, $2) }

san_function:
    | DEF label=Label delimited(LPARENT, separated_list(COMMA, param=Identifier COLON san_type=parameter_san_type {param, san_type} ), RPARENT) san_type COLON 
        sbb=starting_basic_block blocks=list(san_basic_block) END {
            let stmts, return = sbb in
            let convert_starting_block = { label; statements = stmts; ending = return } in
            let san_function = {
                fn_name = label;
                parameters = $3;
                return_type = $4;
                san_basic_blocks = convert_starting_block::blocks
            } in
            Declaration san_function
        }

parameter_san_type:
    | SSIZE { Ssize }
    | BOOL { Boolean }
    | STRINGL { Stringl }

san_type:
    | parameter_san_type { $1 }
    | UNIT { Unit }

