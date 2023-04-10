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


type color =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13

type t = color

let compare = compare

let arguments_register = [
    R0;
    R1;
    R2;
    R3;
    R4;
    R5;
    R6;
    R7
  ]

let color_map = [
    (R0, "aqua");
    (R1, "red");
    (R2, "fuchsia");
    (R3, "green");
    (R4, "navyblue");
    (R5, "pink");
    (R6, "orange");
    (R7, "yellow");
    (R8, "hotpink");
    (R9, "indigo");
    (R10, "magenta");
    (R11, "purple")
  ]