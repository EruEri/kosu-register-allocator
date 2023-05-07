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
       
    