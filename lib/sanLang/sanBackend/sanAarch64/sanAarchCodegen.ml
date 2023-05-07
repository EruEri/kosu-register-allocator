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


module Make(AsmSpec: SanAarchSpecification.Aarch64AsmSpecification) = struct
  module AsmProgram = Common.AsmAst.Make(SanAarchCore.Line)
  module Pprint = SanAarchPprint.Make(AsmSpec)
  module Conv = SanAarchConv.Make(AsmSpec)

  let compile_s ~outfile santyped = 
    let litterals : AsmProgram.litterals = { 
      str_lit_map = Hashtbl.create 10
    } in
    let AsmModule asm_nodes = Conv.translate_san_module ~litterals santyped in
    asm_nodes |> List.iter (fun node -> 
      let repr =  Pprint.string_of_asm_node node in
      Printf.fprintf outfile "%s\n\n" repr
    )

  let compile_tmp_s ~filename san_typed = 
    let _litterals : AsmProgram.litterals = { 
      str_lit_map = Hashtbl.create 10
    } in
    let filename, outfile = Filename.open_temp_file filename ".s" in
    let () = compile_s ~outfile san_typed in
    let () = close_out outfile in
    filename

  let compile ~outname files santype () =
    let tmp_name = compile_tmp_s ~filename:outname santype in
    let _ = Sys.command @@ Printf.sprintf "cc -o %s %s %s" outname tmp_name (String.concat ", " files) in
    ()
end