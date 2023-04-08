type san_type = 
| Int
| String
| Boolean

type atom = 
| String of string
| Int of Int64.t
| Variable of string
| Boolean of bool

type typed_atom = {
  atom_type: san_type;
  atom: atom
}

type tac_binop_bool =
  | TacOr
  | TacSup
  | TacSupEq
  | TacInf
  | TacInfEq
  | TacEqual
  | TacDiff
  | TacAnd

type tac_binop_self =
  | TacAdd
  | TacMinus
  | TacMult
  | TacDiv
  | TacModulo
  | TacBitwiseOr
  | TacBitwiseAnd
  | TacBitwiseXor
  | TacShiftLeft
  | TacShiftRight

type tac_binop = TacSelf of tac_binop_self | TacBool of tac_binop_bool
type tac_unop = TacNot | TacUminus

type binary = {
  binop : tac_binop;
  blhs : typed_atom;
  brhs : typed_atom;
}

type san_rvalue = 
| RVExpr of atom
| RVDiscard 
| RVLater