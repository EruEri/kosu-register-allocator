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


type san_position = {
  start_position : Lexing.position;
  end_position : Lexing.position;
}

type lexer_error = 
| Unexpected_escaped_char of san_position * string
| Unclosed_string of san_position
| Syntax_Error of {
  position: san_position;
  current_lexeme: string;
  message: string;
  state: int option
}

let line_column_of_position p =
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)


let string_of_position_error { start_position; end_position } =
  let start_position_line, start_position_column =
    line_column_of_position start_position
  in
  let end_position_line, end_position_column =
    line_column_of_position end_position
  in
  let column_string =
    Printf.sprintf "%d%s" start_position_column
      (if start_position_column = end_position_column then ""
      else " - " ^ string_of_int end_position_column)
  in
  if start_position_line = end_position_line then
    Printf.sprintf "Line %d, Characters %s" start_position_line column_string
  else
    Printf.sprintf "Lines %d-%d, Characters %d-%d" start_position_line
      end_position_line start_position_column end_position_column

exception Raw_Lexer_Error of lexer_error

let raw_lexer_error e = Raw_Lexer_Error e

let string_of_lexer_error filename = function
| Syntax_Error { position; current_lexeme; message; state } ->
    let s = string_of_position_error position in
    Printf.sprintf
      "\nFile \"%s\", %s : Unexpected \"%s\"\nSyntax Error : %s%s"
      filename s current_lexeme message
      (state
      |> Option.map (Printf.sprintf "Error in state \"%d\"")
      |> Option.value ~default:"")
| Unexpected_escaped_char (position, lexeme) ->
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Unexpected Escaped character: %s" s lexeme
    |> Printf.sprintf "\nFile \"%s\", %s" filename
| Unclosed_string position ->
    position |> string_of_position_error
    |> Printf.sprintf "\nFile \"%s\" %s: String litteral not terminated"
         filename

let register_kosu_error () =
Printexc.register_printer (fun exn ->
    match exn with
    | Raw_Lexer_Error e ->
        Some (string_of_lexer_error "FILE" e)
    | _ -> None)

let () = register_kosu_error ()