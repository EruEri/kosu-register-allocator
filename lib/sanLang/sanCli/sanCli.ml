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

module CmdError = struct
  type t = 
  | No_function of string

  exception CdmError of t

  let string_of_cmd_erorr = function
  | No_function s -> Printf.sprintf "No function \"%s\" was defined" s
  
end

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
    ("basic", Basic); ("detail", Detail); ("liveness", Liveness )
  ]

  let swap (a,b) = b, a

  let lswap list = list |> List.map swap

  let format_term = 
    Arg.(
      value & opt (some string) None & info ~docv:"format" ~doc:"invoke the $(b,dot) command and generate graph with $(opt) image format" ["format"] 
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
      value & opt (some string) None & info ~doc:"Generate graph for a the $(opt) function" ~docv:"function_name" ["f"; "function"]
    )

  let cfg_type_term =
    Arg.(
      value & opt (enum cfg_type_enum) Detail & info ~docv:(string_of_enum cfg_type_enum) ~doc:"Precise which iteration of the cfg should be printed" ["cfg"]
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

  let cfg_doc = "Visualise control flow graphs and inference graphs"

  let cfg_man = [
    `S Manpage.s_description;
    `P 
      "san-cfg allows you to visual control flow graphs (cfg) and variable inference graph";
    `P "san-cfg relies heavily on the $(b,dot) language and the $(b,graphviz) library";  
    `S Manpage.s_see_also;
    `Noblank;
    `P "$(b,dot)(1)";
    `Noblank;
    `P "Graphviz:  https://graphviz.org";
    `Noblank;
  ]


  let infered_name ~extension ~colored fn_name =
    Printf.sprintf "%s.infered%s.%s" fn_name (if colored then "colored" else "") extension

  let cfg_name ~extension ~ctype fn_name = 
    Printf.sprintf "%s.%s.%s" ( cfg_type_enum |> lswap |> List.assoc ctype ) fn_name extension

  let dot_infered_name = infered_name ~extension:"dot"
  let dot_cfg_name = cfg_name ~extension:"dot"

  let target_file file_type format filename = match format with
  | None -> begin match file_type with
    | `Infered colored -> dot_infered_name ~colored filename
    | `Cfg (cfg_type) -> dot_cfg_name ~ctype:cfg_type filename
  end 
  | Some extension -> begin match file_type with
    | `Infered colored -> infered_name ~colored filename ~extension
    | `Cfg (cfg_type) -> cfg_name ~ctype:cfg_type filename ~extension
  end

  let write_cfg cfg_type ~oc san_function = 
    match cfg_type with
      | Basic -> 
        let converted = SanCfg.SanCfgConv.basic_of_san_tyfunction san_function in
        converted |> SanCfg.SanCfgPprint.dot_diagrah_of_cfg_basic |> SanCfg.SanCfgPprint.string_of_dot_graph ~out:oc 
      | Detail ->
        let converted = SanCfg.SanCfgConv.detail_of_san_tyfunction san_function in
        converted |> SanCfg.SanCfgPprint.dot_diagrah_of_cfg_detail |> SanCfg.SanCfgPprint.string_of_dot_graph ~out:oc 
      | Liveness -> 
        let converted = SanCfg.SanCfgConv.liveness_of_san_tyfunction san_function in
        converted |> SanCfg.SanCfgPprint.dot_diagrah_of_cfg_liveness |> SanCfg.SanCfgPprint.string_of_dot_graph ~out:oc 

  let write_infered ~infered ~colored ~oc (san_function: SanTyped.SanTyAst.ty_san_function) = 
    match infered with
    | false -> ()
    | true -> 
      let livecfg = SanCfg.SanCfgConv.liveness_of_san_tyfunction san_function in
      let transform = if colored then SanCfg.SanCfgPprint.export_colored_graph else SanCfg.SanCfgPprint.export_infer_graph_of_cfg in
      transform ~outchan:oc livecfg ()

  let export_from_san_function cmd (san_function: SanTyped.SanTyAst.ty_san_function) = 
    match cmd.format with
    | None -> (
      let outchan_name = target_file (`Cfg cmd.cfg_type) cmd.format san_function.fn_name in
      let () = Out_channel.with_open_bin outchan_name (fun oc ->
        write_cfg cmd.cfg_type ~oc san_function
      ) in

      match cmd.variable_infer with
      | false -> ()
      | true -> 
        let infered_ouchan = target_file (`Infered cmd.colored) cmd.format san_function.fn_name in
        Out_channel.with_open_bin infered_ouchan (fun oc ->
          write_infered ~infered:cmd.variable_infer ~colored:cmd.colored ~oc san_function
        )
    )
    | Some export_format -> begin
      let cfg_outname = target_file (`Cfg cmd.cfg_type) (cmd.format) san_function.fn_name in
      let filename, tmp_cfg_out = Filename.open_temp_file "dot" "dot" in
      let () = write_cfg cmd.cfg_type ~oc:tmp_cfg_out san_function in
      let () = close_out tmp_cfg_out in
      let _ = Sys.command (Printf.sprintf "dot -T%s -o %s %s" export_format cfg_outname filename) in
      match cmd.variable_infer with
      | false -> ()
      | true ->
        let infered_ouchan = target_file (`Infered cmd.colored) cmd.format san_function.fn_name in 
        let tmp_infered_filename, tmp_infered_out = Filename.open_temp_file "infered" "infered" in
        let () = write_infered ~infered:cmd.variable_infer ~colored:cmd.colored ~oc:tmp_infered_out san_function in
        let () = close_out tmp_infered_out in
        let _ = Sys.command (Printf.sprintf "dot -T%s -o %s %s" export_format infered_ouchan tmp_infered_filename ) in
        ()
    end

  let cfg_main cmd = 
    let { format; colored; cfg_type; variable_infer; fn_name; file } = cmd in
    let typed_san_module = SanTyped.of_file file in
    let typed_san_module = fn_name |> Option.map (fun s -> 
      typed_san_module |> List.filter (fun node -> 
        let open SanTyped.SanTyAst in
        match node with
        | TyDeclaration {fn_name; _} | TyExternal {fn_name; } -> fn_name = s
      )
    ) |> Option.value ~default:typed_san_module in
    let filtered_san_module = typed_san_module |> List.filter_map (
      let open SanTyped.SanTyAst in
      function 
      | TyDeclaration fn -> Some fn
      | TyExternal _ -> None
    ) in

    let () = filtered_san_module |> List.iter (fun (san_function: SanTyped.SanTyAst.ty_san_function) ->
      export_from_san_function cmd san_function
    ) in
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