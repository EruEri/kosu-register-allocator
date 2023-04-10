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

module Cfg_Command = struct

  type cfg_type = 
  | Basic
  | Detail
  | Liveness

  type cmd = {
    format: string option;
    colored: bool;
    cfg_type: cfg_type;
    variable_infer: bool;
    fn_name: string option;

    file: string
  }

  let name = "cfg"

  let string_of_enum ?(splitter = "|") ?(quoted = false) enum =
    let f = if quoted then Arg.doc_quote else Fun.id in
    enum |> List.map (fun (elt, _) -> f elt) |> String.concat splitter

  let cfg_type_enum = [
    ("basic", Basic); ("detail", Basic); ("liveness", Liveness )
  ]

  let format_term = 
    Arg.(
      value & opt (some string) None & info ~doc:"invoke the $(b,dot) command and generate graph with $(opt) image format" ["format"] 
    )

  let colored_term = 
    Arg.(
      value & flag & info ~doc:"Color the variable inference graph" ["c"; "colored"]
    )

  let infered_term = 
    Arg.(
      value & flag & info ~doc:"Generate the variable inference graph" ["i"; "infered"] 
    )
    
  let function_name_term = 
    Arg.(
      value & opt (some string) None & info ~doc:"Generate graph for a the $(opt) function" ["f"; "function"]
    )

  let cfg_type_term =
    Arg.(
      value & opt (enum cfg_type_enum) Detail & info ~docv:(string_of_enum cfg_type_enum) ["cfg"]
    )

    let file_term = 
      let info = 
        Arg.info []
        ~docv:"FILE"
      in
      Arg.required (Arg.pos  0 (Arg.some Arg.non_dir_file) None info) 

  let cmd_term run = 
    let combine format colored cfg_type variable_infer fn_name file = 
      run @@ { format; colored; cfg_type; variable_infer; fn_name; file }
    in
    Term.(const combine
      $ format_term
      $ colored_term
      $ cfg_type_term
      $ infered_term
      $ function_name_term
      $ file_term
    )

  let cfg_doc = "Dump san functions representation"

  let cfg_man = []

  let cfg_main cmd = 
    let { format; colored; cfg_type; variable_infer; fn_name; file } = cmd in
    ()

  let cfg = 
    let info = Cmd.info ~doc:cfg_doc ~man:cfg_man name in
    Cmd.v info (cmd_term cfg_main)
end

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


let san_main cmd = 
  
  let { file } = cmd in
  let san_module = SanFrontend.san_module_parse file in
  let ty_san_module = SanTyped.of_san_module san_module in
  let () = ignore ty_san_module in
  ()

let san =
  let info = Cmd.info ~doc:san_doc ~man:san_man ~version name in
  let default = cmd_term san_main in
  Cmd.group ~default info [Cfg_Command.cfg]



let eval () = san |> Cmd.eval ~catch:true