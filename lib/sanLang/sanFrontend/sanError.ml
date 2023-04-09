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


open SanAst
open SanPosition

type lexer_error = 
| Unexpected_escaped_char of san_position * string
| Unclosed_string of san_position
| Syntax_Error of {
  position: san_position;
  current_lexeme: string;
  message: string;
  state: int option
}

type san_error = 
| Undefinded_Variable of string loc
| Undefined_Function of string loc
| Already_define_variable of string loc
| If_Not_boolean_type of {
  error_location: unit loc;
  expected: san_type;
  found: san_type;
}
| Wrong_return_type of {
  error_location: unit loc;
  expected: san_type;
  found: san_type;
}
| Incompatible_type of {
  expected: san_type;
  found: san_type loc;
}
| BinOp_diff_type of {
  error_location: unit loc;
  lhs: san_type;
  rhs: san_type
}
| UnaryOperator_not_suppored of {
  error_location: unit loc;
  unop: tac_unop;
  for_type: san_type;
}
| BinaryOperator_not_suppored of {
  error_location: unit loc;
  binop: tac_binop;
  for_type: san_type;
}
| Function_Wrong_args_number of {
  error_location: unit loc;
  fn_name: string;
  expected: int;
  found: int;
}

exception Raw_Lexer_Error of lexer_error
exception San_error of san_error

let raw_lexer_error e = Raw_Lexer_Error e
let san_error e = San_error e


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