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

open SanCfgAst.SanRegisterAllocator
open SanCfgAst.SanRegisterAllocator.Basic
open SanCfgAst.SanRegisterAllocator.Detail
open SanCfgAst.SanRegisterAllocator.Pprint
open SanTyped.SanTyPprint
module StringSet = Set.Make(String)

type dot_digraph_node = {
  name: string;
  elements: string list;
  ending: string option;
  link_to: string list;
  din_vars: TypedIdentifierSet.t;
  dout_vars: TypedIdentifierSet.t;
}

type dot_digrah = {
  entry: string;
  nodes: dot_digraph_node list
}

let diagraph_node_of_basic_block ~(func : 'a -> string) ?(in_vars = TypedIdentifierSet.empty) ?(out_vars = TypedIdentifierSet.empty) (bb: ('a, 'b) Basic.basic_block) = 
  {
    name = bb.label;
    elements = List.map func bb.cfg_statements;
    ending = bb.ending |> Option.map (string_of_basic_block_end);
    link_to = StringSet.elements bb.followed_by;
    din_vars = in_vars;
    dout_vars = out_vars;
  }

let dot_diagrah_of_cfg_liveness (cfg:  Liveness.cfg_liveness_detail) = 
  {
    entry = cfg.entry_block;
    nodes = cfg.blocks_liveness_details |> BasicBlockMap.bindings |> List.map (fun (_, bbl) ->
      diagraph_node_of_basic_block ~in_vars:bbl.in_vars ~out_vars:bbl.out_vars ~func:string_of_cfg_liveness_statement {bbl.basic_block with ending = fst bbl.basic_block.ending}
    )
  }