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

module SanVariableMap = SanCfg.SanVariableMap
open Common

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
  type shift = SH16 | SH32 | SH48
  type data_size = B | SB

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

  let sp = {
    register = SP;
    size = SReg64
  }

  let according_register raw_register variable = 
    match Sizeof.sizeof @@ snd variable with
    | 8 ->  { register = raw_register; size = SReg64 }
    | _ ->  { register = raw_register; size = SReg32 }

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

  let available_register = [
    X9;
    X10;
    X11;
    X12
  ]

  let does_return_hold_in_register _ = true

  let indirect_return_register = X8

  let return_strategy _ = Simple_return X0
end

module Operande = struct
  type src = [
    `ILitteral of int64
    | `Register of Register.register
  ]

  type dst = Register.register
end

module GreedyColoration = SanCfg.SanRegisterAllocator.GreedyColoring(Register)

module Location = struct
  type adress_offset = [ `ILitteral of int64 | `Register of Register.register ]
  type address = { base : Register.register; offset : adress_offset }
  type adress_mode =
  | Immediat (* out = *intptr; *)
  | Prefix (* out = *(++intptr);*)
  | Postfix (* out = *(intptr++);*)

  type location = 
  | LocReg of Register.register
  | LocAddr of address

  let loc_reg r = LocReg r
  let loc_addr a = LocAddr a

  let create_adress ?(offset = 0L) base = { base; offset = `ILitteral offset }

  let str_ldr_offset_range reg n =
    let open Register in
    if n < 0L then -256L < n
    else
      match reg.size with
      | SReg32 -> n < 255L || (Int64.unsigned_rem n 4L = 0L && n < 16380L)
      | SReg64 -> n < 255L || (Int64.unsigned_rem n 8L = 0L && n < 32760L)
  
  let is_offset_too_far reg address =
    match address with
    | `ILitteral i when not @@ str_ldr_offset_range reg i -> true
    | `ILitteral _ | `Register _ -> false
  
  let increment_adress off adress =
    match adress.offset with
    | `ILitteral offset ->
        { adress with offset = `ILitteral (Int64.add offset off) }
    | `Register _reg -> failwith "Increment register based address"
end


module Instruction = struct
  open Condition_Code
  open Operande
  open Register
  open Location

  type t = 
  | Mov of {destination: register; source: Operande.src}
  | Movk of { destination : Register.register; operand : src; shift : shift option }
  | Mvn of { destination : Register.register; operand : src }
  | Not of { destination : Register.register; source : src }
  | Neg of { destination : Register.register; source : Register.register }
  | Add of { destination : Register.register; operand1 : Register.register;
    (* Int12 litteral oprand*) operand2 : src; offset : bool }
  | Mul of {
      destination : Register.register;
      operand1 : Register.register;
      operand2 : Register.register;
    }
  | SDiv of {
      destination : Register.register;
      operand1 : Register.register;
      operand2 : Register.register;
    }
  | Lsl of {
      destination : Register.register;
      operand1 : Register.register;
      (* LIteral range [0-31] *)
      operand2 : src;
    }
  | Lsr of {
      destination : Register.register;
      operand1 : Register.register;
      (* LIteral range [0-31] *)
      operand2 : src;
    }
  | Asr of {
      destination : Register.register;
      operand1 : Register.register;
      (* LIteral range [0-31] *)
      operand2 : src;
    }
  | Csinc of {
      destination : Register.register;
      operand1 : Register.register;
      operand2 : Register.register;
      condition : condition_code;
    }

  | Cmp of { operand1 : Register.register; operand2 : src }
  | Cset of { register : Register.register; cc : condition_code }
  (* Bitwise And*)
  | And of {
      destination : Register.register;
      operand1 : Register.register;
      operand2 : src;
    }
  (* Bitwise OR*)
  | Orr of {
      destination : Register.register;
      operand1 : Register.register;
      operand2 : src;
    }
  (* Bitwise XOR*)
  | Eor of {
      destination : Register.register;
      operand1 : Register.register;
      operand2 : src;
    }
| Str of { data_size : data_size option; source : Register.register;
    address : address; address_mode : adress_mode;
    }
  | Stp of { x1 : Register.register; x2 : Register.register; address : address;
      adress_mode : adress_mode;
    }
  | Adrp of { dst : Register.register; label : string }
  | B of { cc : condition_code option; label : string }
  | Bl of { cc : condition_code option; label : string }
  | RET
end

module Line = struct
  type line = 
  | Instruction of Instruction.t
  | Comment of string
  | Label of string
  | Directive of string

  type asmline = AsmLine of line * string option

  let instruction ?comment instr = AsmLine (Instruction instr, comment)

  let comment message = AsmLine (Comment message, None)

  let label ?comment l = AsmLine (Instruction l, comment)

  let directive ?comment d  = AsmLine (Instruction d, comment)
end

module AsmProgram = Common.AsmAst.Make(Line)

module FrameManager = struct
  type description = {
    local_space: int;
    variable_map: Location.location SanCfg.SanVariableMap.t
  }

  let frame_descriptor (function_decl: SanTyped.SanTyAst.ty_san_function) = 
    let cfg = SanCfg.SanCfgConv.liveness_of_san_tyfunction function_decl in
    let colored_graph = GreedyColoration.coloration 
      ~parameters:(Util.combine_safe function_decl.parameters Register.arguments_register)
      ~available_color:Register.available_register cfg
  in

  let base_address = Location.create_adress ~offset:0L Register.sp in

  let variable_map, stack_variable = function_decl.locals |> List.sort (fun (_, lhs) (_, rhs) -> 
    let lsize = Sizeof.sizeof lhs in
    let rsize = Sizeof.sizeof rhs in
    compare rsize lsize
    ) |> List.fold_left (fun (acc_map, acc_stack_variable) variable -> 
      let open GreedyColoration.ColoredGraph in
      let node = GreedyColoration.ColoredGraph.find variable colored_graph in
      match node.color with
      | Some color -> 
        let reg = Location.loc_reg @@ Register.according_register color variable in
        SanVariableMap.add variable reg acc_map, acc_stack_variable
      | None -> 
        acc_map, variable::acc_stack_variable
  )  (SanVariableMap.empty, []) in

  let stack_variable_types = List.map snd stack_variable in

  let variable_map = stack_variable |> List.mapi Util.couple |> List.fold_left (fun (acc_address, acc_map) (index, variable) ->
    let offset = Sizeof.offset_of_tuple_index index stack_variable_types in
    let address = Location.increment_adress (Int64.of_int offset) base_address in
    let loc_adrress =  Location.loc_addr address in
    address, SanVariableMap.add variable loc_adrress acc_map
  ) (base_address, variable_map) |> snd in

  let local_space = Sizeof.sizeof_tuple stack_variable_types in
  {
    local_space;
    variable_map
  }

end