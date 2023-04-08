type san_type = 
| Ssize
| Stringl
| Boolean
| Unit

type signature = san_type list * san_type

type atom = 
| String of string
| Int of Nativeint.t
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
  blhs : atom;
  brhs : atom;
}

type unary = {
  unop: tac_unop;
  atom: atom
}

type san_rvalue = 
| RVExpr of atom
| RVUnary of unary
| RVBinary of binary
| RVDiscard 
| RVLater


type san_node =
| External of {
  fn_name: string;
  signature: signature;
  cname: string option
}

type san_module = san_node list