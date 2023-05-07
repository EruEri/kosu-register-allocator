(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open SanAarchCore
open SanTyped.SanTyAst
open SanAarchCore.AsmProgram
open SanAarchCore.Location
open SanAarchCore.Register
open Util



let translate_san_atom ~litterals ~target_reg fd atom = 
  match (atom.atom : SanTyped.SanTyAst.atom) with
  | String s -> 
    let worded_register = Register.worded_register target_reg in
    let (StrLab str_labl) = Hashtbl.find litterals.str_lit_map s in
    worded_register, LineInstruction.load_label ~label:str_labl worded_register
  | Int native -> 
    let worded_register = Register.worded_register target_reg in
    let instructions = LineInstruction.mov_integer worded_register (Int64.of_nativeint native) in
    worded_register, instructions
  | Boolean bool -> 
    let resized = Register.resize32 target_reg in
    let value = if bool then 1L else 0L in
    resized, LineInstruction.mov_integer resized value
  | Variable s -> 
    let variable = s, atom.atom_type in
    let location = FrameManager.location_of variable fd in
    match location with
    | LocReg reg -> 
      if reg.register = target_reg.register then
        target_reg, []
      else
        target_reg, [
          Line.instruction ~comment:(Printf.sprintf "src: , dst") @@
          Instruction.mov ~destination:target_reg ~source: (`Register (Register.resize target_reg.size reg))
        ]
    | LocAddr address ->
      let load_lines = 
        LineInstruction.ldr_instr 
          ~data_size:(Condition_Code.data_size_of_variable variable)
          ~destination:target_reg
          address 
        in
      target_reg, load_lines


  let int64_of_bool = function
  | true -> 1L
  | false -> 0L

  let inline_native_int ~conv ~(where: Location.location) nativeint rvalue = 
    let nativeint = conv nativeint in
    match where with
    | LocAddr address -> 
      let reg = Register.r14_sized rvalue.san_type in
      let instructions = 
        LineInstruction.mov_integer reg (Int64.of_nativeint nativeint) 
      in
    instructions @ LineInstruction.str_instr 
      ~data_size:(Condition_Code.data_size_of_type rvalue.san_type)
      ~source:reg address
    | LocReg reg -> 
      LineInstruction.mov_integer reg (Int64.of_nativeint nativeint) 

let inline_uminus =
  inline_native_int ~conv:(Nativeint.neg) 

let inline_lognot = 
  inline_native_int ~conv:(Nativeint.lognot) 

let inline_unary_int = function
| (TacNot: SanFrontend.SanAst.tac_unop) -> inline_lognot
| TacUminus -> inline_lognot

let inline_boolean ~(where: Location.location) boolvalue rvalue = 
  match where with
  | LocReg reg -> 
    LineInstruction.mov_integer reg (int64_of_bool @@ Bool.not boolvalue) 
  | LocAddr address ->
    let reg = Register.r14_sized rvalue.san_type in
    let instructions = 
      LineInstruction.mov_integer reg (int64_of_bool @@ Bool.not boolvalue) 
    in
  instructions @ LineInstruction.str_instr 
    ~data_size:(Condition_Code.data_size_of_type rvalue.san_type)
    ~source:reg address

let rec translate_unop unop ~litterals ~(where: Location.location) fd ty_atom = 
  let reg_location = where |> reg_opt |> Option.value ~default:(r14_sized ty_atom.atom_type) in
  let last_reg, instructions = translate_san_atom ~litterals ~target_reg:reg_location fd ty_atom in
  let invers_instructions = match unop with
  | (TacNot: SanFrontend.SanAst.tac_unop) when ty_atom.atom_type = Boolean ->
    [ 
      Line.instruction @@ Instruction.eor ~destination:last_reg ~operand1:last_reg ~operand2:(`ILitteral 1L)
    ]
  | TacNot -> [
    Line.instruction @@ Instruction.mvn ~destination:last_reg ~operand:(`Register last_reg)
  ]
  | TacUminus -> [
    Line.instruction @@ Instruction.neg ~destination:last_reg ~operand:last_reg
  ]
  in

  let store_instruction = where |> address_opt |> Option.map (fun address -> 
    LineInstruction.str_instr 
      ~data_size:(Condition_Code.data_size_of_type ty_atom.atom_type)
      ~source:last_reg
      address
  ) |> Option.value ~default:[] 
  in

  instructions @ invers_instructions @ store_instruction


and translate_san_rvalue ~litterals ~(where: Location.location) fd rvalue = 
  match rvalue.san_rvalue with
  | TyRVExpr typed_atom -> begin
    match where with
    | LocAddr address -> 
      let reg_result, instructions = 
      translate_san_atom ~litterals ~target_reg:(Register.r14_sized rvalue.san_type) 
      fd typed_atom 
    in
    instructions @ LineInstruction.str_instr 
      ~data_size:(Condition_Code.data_size_of_type rvalue.san_type)
      ~source:reg_result address
    | LocReg reg ->
      snd @@ translate_san_atom ~litterals ~target_reg:reg fd typed_atom
  end
  | TYRVUnary {unop; ty_atom = {atom = Int n; _}} -> begin 
    inline_unary_int unop ~where n rvalue
  end 
  | TYRVUnary {unop = TacNot; ty_atom = {atom = Boolean b; _}} -> begin
    inline_boolean ~where b rvalue
  end
  | TYRVUnary {unop; ty_atom} -> begin 
    translate_unop unop ~litterals ~where fd ty_atom
  end
  | TYRVBinary ty_binary -> failwith ""
  | TyRVFunctionCall ty_fncall -> 
    let return_type = rvalue.san_type in
    let params_reg_count = List.length Register.arguments_register in
    let register_parameters, stack_parameters = 
      ty_fncall.parameters 
      |> List.mapi Util.couple
      |> List.partition_map (fun (i, value) -> 
        if i < params_reg_count then Either.left value else Either.right value )
    in

    let register_parameters_instructions = 
      Register.arguments_register
      |> Util.combine_safe register_parameters
      |> List.map (fun (atom, raw_register) -> 
        let target_reg = Register.according_register raw_register atom.atom_type in
        snd @@ translate_san_atom ~litterals ~target_reg fd atom  
      )
      |> List.flatten
    in

    let call_instruction = 
      Line.instruction ~comment:(Printf.sprintf "call : %s" ty_fncall.fn_name) 
      @@ Instruction.bl ty_fncall.fn_name
    in

    let store_instructions =  begin match where with
    | LocReg reg -> 
      if Register.cmp_raw_register reg x0 then
        []
      else
        let aligned_reg = Register.align_with ~along:reg x0 in
        [
          Line.instruction @@ Instruction.mov ~destination:reg ~source:(`Register aligned_reg)
        ]
    | LocAddr address -> 
      let resized_x0 = Register.resize_type return_type x0 in
      LineInstruction.str_instr 
        ~data_size:(Condition_Code.data_size_of_type return_type)
        ~source:resized_x0
        address
    end
    in
    
    register_parameters_instructions @ call_instruction::store_instructions
  | TyRVDiscard _ | TYRVLater _ -> []
 
let translate_san_statement ~litterals fd = function
| TySSDeclaration (v, rvalue) ->
  let location = FrameManager.location_of (v, rvalue.san_type) fd in
  translate_san_rvalue ~litterals ~where:location fd rvalue
       
let translate_san_ending ~litterals fd = function
  | TySE_return typed_atom -> 
    snd @@ translate_san_atom ~litterals ~target_reg:x0 fd typed_atom
  | TYSE_If {expr; if_label; else_label} -> 
    let r14 = Register.r14_sized expr.atom_type in
    let last_reg, instructions = translate_san_atom ~litterals ~target_reg:r14 fd expr in
    let cmp_instruction = Line.instruction @@ Instruction.cmp ~operand1:last_reg ~operand2:(`ILitteral 1L) in
    let jmps = Line.instructions [
      Instruction.b ~cc:(Condition_Code.EQ) if_label;
      Instruction.b else_label
    ]
    in
  instructions @ cmp_instruction::jmps


let translate_san_block ~litterals fd block = 
  let label_line = Line.label block.label in
  let statements_instructions = 
    block.statements
    |> List.map (translate_san_statement ~litterals fd)
    |> List.flatten
  in
  let ending_instructions = 
    block.ending
    |> Option.map (translate_san_ending ~litterals fd)
    |> Option.value ~default:[]
  in
  label_line::statements_instructions @ ending_instructions

let translate_san_function litterals fd san_function = 
  san_function.san_basic_blocks
  |> List.map (translate_san_block ~litterals fd)
  |> List.flatten
  |> List.cons (Line.label san_function.fn_name)
