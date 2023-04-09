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

open Cmdliner


type cmd = {
  file: string
}
let name = "san"
let version = "0.0.1"


let file_term = 
  let info = 
    Arg.info []
    ~docv:"FILE"
  in
  Arg.required  ( Arg.pos  0 (Arg.some Arg.non_dir_file) None info) 


let cmd_term run = 
  let combine file = 
    run @@ { file }
  in
  Term.(const combine
    $ file_term
  )

let san_doc = "The minimalist 3 address code compiler"

let san_man = [
  `S Manpage.s_description;
  `P 
    "San is the compiler of a minimalist 3 address code language";
  `P
    "San exists to more easily test the register allocator of Kosu";
  `S Manpage.s_see_also;
  `P "Repository: https://github.com/EruEri/kosu-register-allocator";
  `P "Repository: https://github.com/EruEri/kosu-lang";
  `S Manpage.s_authors;
  `P "Yves Ndiaye";
  `S "COPYRIGHT";
  `P "Yves Ndiaye";
  `S "LICENSE";
  `P "San is distributed under the GNU GPL-3.0"; 
]


let san run =
  let info = Cmd.info ~doc:san_doc ~man:san_man ~version name in
  Cmd.v info (cmd_term run)

let run cmd = 
  let { file } = cmd in
  let san_module_res = In_channel.with_open_bin file (fun ic -> 
    let lexbuf = Lexing.from_channel ic in
    SanParser.parse lexbuf (Parser.Incremental.san_module lexbuf.lex_curr_p)
  ) in
  let san_modules = match san_module_res with
  | Ok san_module -> san_module
  | Error error -> raise @@ SanError.Raw_Lexer_Error (error) in
  let () = ignore san_modules in
  ()

let eval () = run |> san |> Cmd.eval ~catch:true