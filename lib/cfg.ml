module StringSet = Set.Make(String)

module type Cfg_Sig = sig
  type tac_typed_rvalue
  type tac_typed_expression
  type rktype

  val compare: (string * rktype) -> (string * rktype) -> int

  val declaration_typed: tac_typed_rvalue -> rktype
  val derefed_typed:  tac_typed_rvalue -> rktype

  val ttrv_identifiers_used: tac_typed_rvalue -> (string * rktype) list
  val tte_idenfier_used: tac_typed_expression -> (string * rktype) list
  val is_affectation: tac_typed_rvalue -> bool
end


module Make(CfgS : Cfg_Sig) = struct

  

  module TypedIdentifierSet = Set.Make(struct
    open CfgS
    type t = (string * rktype)
    let compare = CfgS.compare
  end
  )

  type typed_variable = TypedIdentifierSet.elt



  type cfg_statement =
  | CFG_STacDeclaration of { identifier : string; trvalue : CfgS.tac_typed_rvalue }
  | CFG_STacModification of { identifier : string; trvalue : CfgS.tac_typed_rvalue }
  | CFG_STDerefAffectation of { identifier : string; trvalue : CfgS.tac_typed_rvalue }

  type bbe_if = {
    condition: CfgS.tac_typed_expression;
    if_label: string;
    else_label: string;
  }

  type basic_block_end = 
  | BBe_if of bbe_if
  | Bbe_return of CfgS.tac_typed_expression




  module BasicBlockMap = Map.Make(String)

  module Basic = struct

    type 'a basic_block = {
      label: string;
      cfg_statements: 'a list;
      followed_by: StringSet.t;
      ending: basic_block_end option
    }


    type cfg = {
      entry_block: string;
      blocks: (cfg_statement basic_block) BasicBlockMap.t;
      parameters: TypedIdentifierSet.t;
      locals_vars: TypedIdentifierSet.t;
    }
    let fetch_basic_block_from_label label_name bbset = 
      bbset |> BasicBlockMap.find label_name

    let stmt_identifiers_used = function
    | CFG_STDerefAffectation {trvalue; _}
    | CFG_STacDeclaration {trvalue; _}
    | CFG_STacModification {trvalue; _} -> CfgS.ttrv_identifiers_used trvalue
    
    let basic_block_input_var ~out_vars basic_block = 
      let open CfgS in
      let rec basic_block_cfg_statement_list ~killed ~generated = function
      | [] ->
        basic_block.ending 
        |> Option.map (fun (Bbe_return tte | BBe_if {condition = tte; _}) -> 
          let right_value_variables_used_set = tte |> tte_idenfier_used |> TypedIdentifierSet.of_list in
          let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_variables_used_set killed in
          TypedIdentifierSet.union generated remove_block_create_variable_set
        ) |> Option.value ~default:generated
        |> TypedIdentifierSet.union (TypedIdentifierSet.diff out_vars killed)
      | stmt::q -> 
        begin match stmt with
        | CFG_STacDeclaration {identifier; trvalue} 
          -> 
            let extented_killed_vars, new_geneated =  
              let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
              let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set killed in
              let extented_killed_vars = TypedIdentifierSet.add (identifier, declaration_typed trvalue) killed in
              let new_geneated = TypedIdentifierSet.union remove_block_create_variable_set generated in
              extented_killed_vars, new_geneated
            in
            basic_block_cfg_statement_list ~killed:extented_killed_vars ~generated:new_geneated q

        | CFG_STacModification {identifier = _; trvalue} ->
          let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
          let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set killed in
          let new_geneated = TypedIdentifierSet.union remove_block_create_variable_set generated in
          basic_block_cfg_statement_list ~killed:killed ~generated:new_geneated q
        | CFG_STDerefAffectation {trvalue; identifier}   -> 
          let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
          let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set killed in
          let new_geneated = TypedIdentifierSet.union remove_block_create_variable_set generated in
          let extented_generated = TypedIdentifierSet.add (identifier, derefed_typed trvalue) new_geneated in
          basic_block_cfg_statement_list ~killed:killed ~generated:extented_generated q
        end
      in
      basic_block_cfg_statement_list ~killed:TypedIdentifierSet.empty ~generated:TypedIdentifierSet.empty basic_block.cfg_statements
    
    let rec basic_block_output_var basic_block_set basic_block = 
      match basic_block.ending with
      | Some (Bbe_return _) -> TypedIdentifierSet.empty
      | _ -> 
         StringSet.fold (fun elt acc -> 
          let follow_block = fetch_basic_block_from_label elt basic_block_set in 
          let out_vars = basic_block_output_var basic_block_set follow_block in
          let follow_basic_block_input = basic_block_input_var ~out_vars follow_block in
          TypedIdentifierSet.union acc follow_basic_block_input 
        ) basic_block.followed_by TypedIdentifierSet.empty
  end

   module Detail = struct
    type 'a basic_block_detail = {
      basic_block: 'a Basic.basic_block;
      in_vars: TypedIdentifierSet.t;
      out_vars: TypedIdentifierSet.t;
    }

    type cfg_detail = {
      entry_block: string;
      blocks_details: (cfg_statement basic_block_detail) BasicBlockMap.t;
      parameters: TypedIdentifierSet.t;
      locals_vars: TypedIdentifierSet.t;
    }

    
    let basic_block_detail_of_basic_block set bb = 
      let out_vars = Basic.basic_block_output_var set bb in
      let in_vars = Basic.basic_block_input_var ~out_vars bb in

      {
        basic_block = bb;
        in_vars;
        out_vars;
      }

    let of_cfg (cfg: Basic.cfg) = {
      entry_block = cfg.entry_block;
      blocks_details = cfg.blocks |> BasicBlockMap.bindings |> List.map (fun (label, block) -> label,  basic_block_detail_of_basic_block cfg.blocks block) |> List.to_seq |> BasicBlockMap.of_seq;
      parameters = cfg.parameters;
      locals_vars = cfg.locals_vars;
      }
   end

   module Liveness = struct

    module LivenessInfo = struct
      type liveness_info = ( typed_variable * bool ) list

      let init map variable : liveness_info = variable |> List.map (fun v -> v, map v)

      let is_alive (elt: typed_variable) info: bool = info |> List.assoc elt 

      let is_dead elt info = not @@ is_alive elt info

      let set_alive (elt: typed_variable) info: liveness_info = info |> List.map (fun e -> 
        let in_element, _ = e in
        if 
          CfgS.compare in_element elt = 0 then
            in_element, true
        else
          e
      )

      let of_list l: liveness_info = l

      let to_list l: ( typed_variable * bool ) list = l

      let elements : liveness_info -> typed_variable list  = List.map fst 

      let set_dead (elt: typed_variable) info: liveness_info = info |> List.map (fun e -> 
        let in_element, _ = e in
        if 
          CfgS.compare in_element elt = 0 then
            in_element, false
        else
          e
      )

      let set_dead_of_list (elts: typed_variable list) info: liveness_info = 
        elts |> List.fold_left (fun new_info var -> 
          set_dead var new_info
      ) info
    end


    (* open Util *)
    (* type liveness_info = {
      variable: TypedIdentifierSet.elt;
      is_alive: bool;
    }

    type dated_info = TypedIdentifierSet.elt Date.dated

    type dated_infos = dated_info list

     type 'a cfg_statement_info = {
      stmt: cfg_statement;
      info: 'a list
     }
     type cfg_liveness = liveness_info cfg_statement_info Detail.basic_block_detail
     type cfg_dated = dated_info cfg_statement_info Detail.basic_block_detail *)

     type cfg_liveness_statement = {
      cfg_statement: cfg_statement;
      liveness_info: LivenessInfo.liveness_info
     }

     type cfg_liveness_detail = {
      entry_block: string;
      blocks_liveness_details: (cfg_liveness_statement Detail.basic_block_detail) BasicBlockMap.t;
      parameters: TypedIdentifierSet.t;
      locals_vars: TypedIdentifierSet.t;
     }

     type liveness_var_block =
     | Always_alive
     | Die_at of int
     | Die_in_ending 

     (**
      @returns : whenever the variable leaves the block
     *)
     let does_outlives_block (elt: TypedIdentifierSet.elt) (bbd: cfg_statement Detail.basic_block_detail) = 
      TypedIdentifierSet.mem elt bbd.out_vars


    let dying_in_vars_in_block (bbd: cfg_statement Detail.basic_block_detail) = 
      TypedIdentifierSet.diff bbd.in_vars bbd.out_vars

    let does_in_vars_dies_in_block (elt: typed_variable) (bbd: cfg_statement Detail.basic_block_detail) = 
      bbd
      |> dying_in_vars_in_block
      |> TypedIdentifierSet.mem elt

      let when_variable_dies ~start_from (elt: typed_variable) (bbd: cfg_statement Detail.basic_block_detail) = 
        let open CfgS in
        let (>>=) = Option.bind in
        let liveness =  bbd.basic_block.cfg_statements |> List.fold_left (fun (index, acc) stmt -> 
          let used_vars = Basic.stmt_identifiers_used stmt in
          if 
            used_vars |> List.exists (fun var -> elt |> CfgS.compare var |> ( = ) 0) 
          then
            (index + 1, Die_at (max start_from index))
          else
            (index + 1, acc)
        ) (1, Die_at start_from) |> snd
      in
      bbd.basic_block.ending >>= (fun (Bbe_return tte | BBe_if {condition = tte; _}) -> 
        let used_vars = tte_idenfier_used tte in
        if 
          used_vars |> List.exists (fun var -> elt |> CfgS.compare var |> ( = ) 0) 
        then 
          Some Die_in_ending
        else
          None
        )
        |> Option.value ~default:liveness


      (**
        @returns in how many statements the variable dies 
        @raise Invalid_argument : if the variable outlive the block
      *)
      let when_variable_dies_unsafe ~start_from elt bdd = 
        match when_variable_dies ~start_from elt bdd with
        | Die_at n -> Some n
        | Die_in_ending -> None
        | Always_alive -> elt |> fst |> Printf.sprintf "The variable %s outlives the block" |> invalid_arg


    (**
    Remove the from the hashtable the variable that die at the times of [current]
    @returns : List of dying variable at the times [current]    
    *)
    let fetch_dying_variable ~current map (): typed_variable list =
      let removed =  Hashtbl.find_all map (Some current) in
      let () = map |> Hashtbl.filter_map_inplace (fun key value -> 
        if key = (Some current) then None
        else Some value
      )  in
      removed

    let merge_basic_block_map lmap rmap = 
      BasicBlockMap.union (fun key m1 m2 ->
        let () = Printf.eprintf "Conficiting label = %s\n" key in
        if (m1 <> m2) then
          failwith "Diff for key"
        else
          Some m1
      ) lmap rmap


    let dated_basic_block_of_basic_block_detail ~delete_useless_stmt dated_info_list (bbd: cfg_statement Detail.basic_block_detail) = 
      let open CfgS in
      let when_to_die_hashmap = bbd |> dying_in_vars_in_block |> TypedIdentifierSet.elements |> List.map (fun elt ->
        when_variable_dies_unsafe ~start_from:0 elt bbd, elt
      ) |> List.to_seq |> Hashtbl.of_seq in

      let dated_info_list = LivenessInfo.init (fun typed_variable -> TypedIdentifierSet.mem typed_variable bbd.in_vars && LivenessInfo.is_alive typed_variable dated_info_list) (LivenessInfo.elements dated_info_list) in
      
      (* Be careful new statement are in the reversed order *)
      bbd.basic_block.cfg_statements |> List.fold_left (fun (block_line_index, cfg_liveness_statements, last_dated_info_list) stmt ->
        let dated_info_list = last_dated_info_list in
        let listof_now_dying_var = fetch_dying_variable ~current:block_line_index when_to_die_hashmap () in
        let date_info_updated_dying = LivenessInfo.set_dead_of_list listof_now_dying_var dated_info_list in
        let next_line = block_line_index + 1 in
        match stmt with
        | CFG_STacDeclaration {identifier; trvalue} 
        | CFG_STacModification {identifier; trvalue} -> 
          let typed_variable = (identifier, declaration_typed trvalue) in
          
          begin match does_outlives_block typed_variable bbd with
          | true -> begin 
            let updated_alive_info = if CfgS.is_affectation trvalue then (LivenessInfo.set_alive typed_variable date_info_updated_dying) else date_info_updated_dying in
            next_line,  {cfg_statement = stmt; liveness_info = date_info_updated_dying}::cfg_liveness_statements, updated_alive_info
          end
          | false -> begin match CfgS.is_affectation trvalue with
            | false -> next_line, {cfg_statement = stmt; liveness_info = date_info_updated_dying}::cfg_liveness_statements, date_info_updated_dying
            | true -> 
              let in_how_many_times_it_dies = when_variable_dies_unsafe ~start_from:block_line_index typed_variable bbd in
              let does_it_dies_now = in_how_many_times_it_dies |> Option.map (( = ) block_line_index) |> Option.value ~default:false in
              if 
                does_it_dies_now 
              then
                let updated_dead_liveinfo = (LivenessInfo.set_dead typed_variable date_info_updated_dying) in
                next_line, 
                ( if delete_useless_stmt then cfg_liveness_statements
                else
                  {cfg_statement = stmt; liveness_info = date_info_updated_dying}::cfg_liveness_statements
                )
                ,  updated_dead_liveinfo
              else
                let () = Hashtbl.replace when_to_die_hashmap in_how_many_times_it_dies typed_variable in
                let updated_alive_info = (LivenessInfo.set_alive typed_variable date_info_updated_dying) in
                next_line,  {cfg_statement = stmt; liveness_info = date_info_updated_dying}::cfg_liveness_statements, updated_alive_info
           end 
        end
        | CFG_STDerefAffectation {identifier; trvalue} -> 
          let typed_variable = (identifier, derefed_typed trvalue) in
          let updated_alive_info = (LivenessInfo.set_alive typed_variable date_info_updated_dying) in
          next_line,  {cfg_statement = stmt; liveness_info = date_info_updated_dying}::cfg_liveness_statements, updated_alive_info
      ) (0, ([]) , dated_info_list) |> fun (_, list, latest_liveness_info) -> 
        match list with
        | [] -> [], dated_info_list
        | _::_ as l -> List.rev l, latest_liveness_info

        
    (*
      Relie on the fac that a variable create in a branch block cannot be used in a following block
      It's mostlt true for staticlly typed language but false for dynamic not like Python   
    *)
    let rec basic_block_liveness_of_convert ~delete_useless_stmt ~visited ~dated_info basic_blocks_map (bbd: cfg_statement Detail.basic_block_detail) = 

    let open Detail in
    if not @@ Hashtbl.mem visited bbd.basic_block.label 
      then
        let self_to_cfg_liveness, last_dated_info = dated_basic_block_of_basic_block_detail ~delete_useless_stmt dated_info bbd in
        let basic_block_liveness =  {
            in_vars = bbd.in_vars;
            out_vars = bbd.out_vars;
            basic_block = {
              label = bbd.basic_block.label;
              followed_by = bbd.basic_block.followed_by;
              ending = bbd.basic_block.ending;
              cfg_statements = self_to_cfg_liveness
            }
        }
      in
      let () = Hashtbl.replace visited bbd.basic_block.label () in
      let singleton = BasicBlockMap.singleton (basic_block_liveness.basic_block.label) basic_block_liveness in
      let converted_blocks_map = StringSet.fold (fun follow acc -> 
        let follow_block = Basic.fetch_basic_block_from_label follow basic_blocks_map in
        let follow_transformed_block = basic_block_liveness_of_convert ~delete_useless_stmt ~visited ~dated_info:last_dated_info basic_blocks_map follow_block in
        merge_basic_block_map acc follow_transformed_block
      ) bbd.basic_block.followed_by singleton in 
      converted_blocks_map
    else
      BasicBlockMap.empty

    let of_cfg_details ~delete_useless_stmt (cfg: Detail.cfg_detail) =
      let visited = Hashtbl.create 5 in 
      let entry_block = Basic.fetch_basic_block_from_label cfg.entry_block cfg.blocks_details in
      let liveness_info =  TypedIdentifierSet.fold (fun typed_var acc -> 
        let is_parameters = TypedIdentifierSet.mem typed_var cfg.parameters in
        (typed_var, is_parameters)::acc
      ) cfg.locals_vars [] |> LivenessInfo.of_list in
      let open Detail in
      {
        entry_block = cfg.entry_block;
        parameters = cfg.parameters;
        locals_vars = cfg.locals_vars;
        blocks_liveness_details = basic_block_liveness_of_convert ~delete_useless_stmt ~visited ~dated_info:liveness_info cfg.blocks_details entry_block
      }

   end
end