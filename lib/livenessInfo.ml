(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of kosu-register-allocator                                               *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
(*                                                                                            *)
(* kosu-register-allocator is free software: you can redistribute it and/or modify            *)
(*  it under the terms either:                                                                *)
(*                                                                                            *)
(*    - the GNU General Public License as published by the Free Software Foundation,          *)
(*        either version 3 of the License, or (at your option) any later version              *)
(*   or                                                                                       *)
(*                                                                                            *)
(*     - the GNU Lesser General Public License as published by the Free Software Foundation,  *)
(*        either version 3 of the License, or (at your option) any later version              *)
(*                                                                                            *)
(* kosu-register-allocator is distributed in the hope that it will be useful,                 *)
(*   but WITHOUT ANY WARRANTY;                                                                *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along                    *)
(* with kosu-register-allocator. If not, see <http://www.gnu.org/licenses/>.                  *)
(*                                                                                            *)
(**********************************************************************************************)

module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module Make(OrderedType: OrderedType) = struct
  
type liveness_info = ( OrderedType.t  * bool ) list

let init map variables :  liveness_info = variables |> List.map (fun v -> v, map v)

let is_alive (elt: OrderedType.t) info: bool = info |> List.assoc elt 

let is_dead elt info = not @@ is_alive elt info

let set_alive (elt: OrderedType.t) info: liveness_info = info |> List.map (fun e -> 
  let in_element, _ = e in
  if 
    OrderedType.compare in_element elt = 0 then
      in_element, true
  else
    e
)

let of_list l: liveness_info = l

let to_list l: ( OrderedType.t * bool ) list = l

let elements : liveness_info -> OrderedType.t list  = List.map fst 

let alive_elements: liveness_info -> OrderedType.t list = List.filter_map (fun (elt, alive) ->
  if alive then Some elt else None  
)

let set_dead (elt: OrderedType.t) info: liveness_info = info |> List.map (fun e -> 
  let in_element, _ = e in
  if 
    OrderedType.compare in_element elt = 0 then
      in_element, false
  else
    e
)

let set_dead_of_list (elts: OrderedType.t list) info: liveness_info = 
  elts |> List.fold_left (fun new_info var -> 
    set_dead var new_info
) info

end