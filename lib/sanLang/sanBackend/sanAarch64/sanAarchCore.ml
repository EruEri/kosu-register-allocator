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


module Immediat = struct
  let mask_6_8bytes = 0xFFFF_0000_0000_0000L
  let mask_4_6bytes = 0x0000_FFFF_0000_0000L
  let mask_2_4bytes = 0x0000_0000_FFFF_0000L
  let mask_0_2bytes = 0x0000_0000_0000_FFFFL
  let max_16bits_immediat = 65535L
  let min_16bits_immediat = -65535L

  let is_direct_immediat int16 =
    int16 >= min_16bits_immediat && int16 <= max_16bits_immediat

  let split n =
    let int16 = Int64.logand mask_0_2bytes n in
    let int32 = Int64.logand mask_2_4bytes n in
    let int48 = Int64.logand mask_4_6bytes n in
    let int64 = Int64.logand mask_6_8bytes n in
    ( Int64.shift_right_logical int64 48,
      Int64.shift_right_logical int48 32,
      Int64.shift_right_logical int32 16,
      int16 )
end


module Condition_Code = struct
  type condition_code =
    | EQ  (** Equal *)
    | NE  (** Not Equal *)
    | CS  (** Carry Set *)
    | CC  (** Carry clear *)
    | MI  (** Minus / Negative *)
    | PL  (** Plus , Posivite ./ Zero *)
    | VS  (** Overflow *)
    | VC  (** No overflow*)
    | HI  (** Unsigned higher*)
    | LS  (** Unsigned lower or same *)
    | GE  (** Signed greater than or equal*)
    | LT  (** Signed less than*)
    | GT  (** Signed greather than *)
    | LE  (** Signed less than or equal *)
    | AL  (** Always*)
end

module Register = struct

  type variable = string * SanTyped.SanTyAst.san_type
  type register_size = SReg32 | SReg64

  type raw_register = 
    | X0
    | X1
    | X2
    | X3
    | X4
    | X5
    | X6
    | X7
    | X8 (* XR *)
    | X9
    | X10
    | X11
    | X12
    | X13
    | X14
    | X15
    | X16
    | X29
    | X30
    | XZR
    | SP

  type register = {
    register: raw_register;
    size: register_size
  }

  type t = raw_register

  let word_regsize = 
    match Nativeint.size with
    | 64 -> SReg64
    | 32 -> SReg32
    | _ -> failwith "Word size unsupported" 

  let w0 = {
    register = X0;
    size = SReg32
  }
  let x0 = {
    register = X0;
    size = SReg64
  }

  let word_x0 = 
    match word_regsize with
    | SReg64 -> x0
    | SReg32 -> w0



  type return_strategy =
  | Indirect_return
  | Simple_return of t
  | Splitted_return of t * t

  let compare = Stdlib.compare

  let caller_saved_register = [
    X0;
    X1;
    X2;
    X3;
    X4;
    X5;
    X6;
    X7;
    X8; (* XR *)
    X9;
    X10;
    X11;
    X12;
    X13;
    X14;
    X15;
  ]

  let callee_saved_register = [
    X16;
    X29;
    X30;
    SP
  ]

  let arguments_register = [
    X0;
    X1;
    X2;
    X3;
    X4;
    X5;
    X6;
    X7;
  ]

  let syscall_register = [
    X0;
    X1;
    X2;
    X3;
    X4;
    X5;
  ]

  let does_return_hold_in_register _ = true

  let indirect_return_register = X8

  let return_strategy _ = Simple_return X0
end


module GreedyColoration = SanCfg.SanRegisterAllocator.GreedyColoring(Register)