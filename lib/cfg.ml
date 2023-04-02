module StringSet = Set.Make(String)

module type Cfg_Sig = sig

  type variable
  type tac_typed_rvalue
  type tac_typed_expression

  val repr: variable -> string
  val compare: variable -> variable -> int
  val lvalue_variable: string -> tac_typed_rvalue -> variable
  val lvalue_deref_variable: string -> tac_typed_rvalue -> variable
  val ttrv_identifiers_used: tac_typed_rvalue -> variable list
  val tte_idenfier_used: tac_typed_expression -> variable list
  val is_affectation: tac_typed_rvalue -> bool

  val variables_as_parameter: tac_typed_rvalue -> (string * int) list option
end

module type ABI = sig
  type register
  type any
  type rktype

  type return_strategy = 
  | Indirect_return
  | Simple_return of register
  | Splitted_return of register * register

  val compare: register -> register -> int
  val any: any
  val registers: register list
  val callee_saved_register: register list
  val caller_saved_register: register list
  val syscall_register: register list
  val argument_register: register list
  val syscall_register_code: register

  val does_return_hold_in_register: any -> rktype -> bool
  val indirect_return_register: register
  val return_strategy: any -> rktype -> return_strategy

end


module Make(CfgS : Cfg_Sig) = struct

  module TypedIdentifierSig = struct
    type t = CfgS.variable
    let compare = CfgS.compare
  end

  module TypedIdentifierSet = Set.Make(TypedIdentifierSig)

  type variable = TypedIdentifierSet.elt



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

    type ('a, 'b) basic_block = {
      label: string;
      cfg_statements: 'a list;
      followed_by: StringSet.t;
      ending: 'b
    }


    type cfg = {
      entry_block: string;
      blocks: (cfg_statement, basic_block_end option) basic_block BasicBlockMap.t;
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
              let extented_killed_vars = TypedIdentifierSet.add (lvalue_variable identifier trvalue) killed in
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
          let extented_generated = TypedIdentifierSet.add (lvalue_deref_variable identifier trvalue) new_geneated in
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
    type ('a, 'b) basic_block_detail = {
      basic_block: ('a, 'b) Basic.basic_block;
      in_vars: TypedIdentifierSet.t;
      out_vars: TypedIdentifierSet.t;
    }

    type cfg_detail = {
      entry_block: string;
      blocks_details: (cfg_statement, basic_block_end option) basic_block_detail BasicBlockMap.t;
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
      type liveness_info = ( variable * bool ) list

      let init map variable : liveness_info = variable |> List.map (fun v -> v, map v)

      let is_alive (elt: variable) info: bool = info |> List.assoc elt 

      let is_dead elt info = not @@ is_alive elt info

      let set_alive (elt: variable) info: liveness_info = info |> List.map (fun e -> 
        let in_element, _ = e in
        if 
          CfgS.compare in_element elt = 0 then
            in_element, true
        else
          e
      )

      let of_list l: liveness_info = l

      let to_list l: ( variable * bool ) list = l

      let elements : liveness_info -> variable list  = List.map fst 

      let alive_elements: liveness_info -> variable list = List.filter_map (fun (elt, alive) ->
        if alive then Some elt else None  
      )

      let set_dead (elt: variable) info: liveness_info = info |> List.map (fun e -> 
        let in_element, _ = e in
        if 
          CfgS.compare in_element elt = 0 then
            in_element, false
        else
          e
      )

      let set_dead_of_list (elts: variable list) info: liveness_info = 
        elts |> List.fold_left (fun new_info var -> 
          set_dead var new_info
      ) info
    end

     type cfg_liveness_statement = {
      cfg_statement: cfg_statement;
      liveness_info: LivenessInfo.liveness_info
     }

     type liveness_ending = basic_block_end option * LivenessInfo.liveness_info

     type cfg_liveness_detail = {
      entry_block: string;
      blocks_liveness_details: (cfg_liveness_statement, liveness_ending) Detail.basic_block_detail BasicBlockMap.t;
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
     let does_outlives_block (elt: TypedIdentifierSet.elt) (bbd: (cfg_statement, basic_block_end option) Detail.basic_block_detail) = 
      TypedIdentifierSet.mem elt bbd.out_vars


    let dying_in_vars_in_block (bbd: (cfg_statement, basic_block_end option) Detail.basic_block_detail) = 
      TypedIdentifierSet.diff bbd.in_vars bbd.out_vars

    let does_in_vars_dies_in_block (elt: variable) (bbd: (cfg_statement, basic_block_end option) Detail.basic_block_detail) = 
      bbd
      |> dying_in_vars_in_block
      |> TypedIdentifierSet.mem elt

      let when_variable_dies ~start_from (elt: variable) (bbd: (cfg_statement, basic_block_end option) Detail.basic_block_detail) = 
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
        ) (0, Die_at start_from) |> snd
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
        | Always_alive -> elt |> CfgS.repr |> Printf.sprintf "The variable %s outlives the block" |> invalid_arg


    (**
    Remove the from the hashtable the variable that die at the times of [current]
    @return : List of dying variable at the times [current]    
    *)
    let fetch_dying_variable ~current map (): variable list =
      let removed = map |> Hashtbl.to_seq |> Seq.filter_map (
        fun ((time, elt), _) -> if time = Some current then Some elt else None 
      ) 
      |> List.of_seq in
      (* let removed =  Hashtbl.find_all map (Some current) in *)
      let () = map |> Hashtbl.filter_map_inplace (fun (key, _) value -> 
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


    let dated_basic_block_of_basic_block_detail ~delete_useless_stmt dated_info_list (bbd: (cfg_statement, basic_block_end option) Detail.basic_block_detail) = 
      let open Detail in
      let open Basic in
      let open CfgS in
      let when_to_die_hashmap = bbd |> dying_in_vars_in_block |> TypedIdentifierSet.elements |> List.map (fun elt ->
        let when_it, elt =  when_variable_dies_unsafe ~start_from:0 elt bbd, elt in
        (when_it, elt), elt
      ) |> List.to_seq |> Hashtbl.of_seq in
      
      
      (* let () = when_to_die_hashmap |> Hashtbl.iter (fun dying variable -> 
        let () = Printf.printf "lifetime : %s, v = %s\n" 
          (dying |> Option.map string_of_int |> Option.value ~default:"null") 
          (CfgS.repr variable)
        in 
        ()
      ) in *)
 
      let dated_info_list = LivenessInfo.init (fun variable -> TypedIdentifierSet.mem variable bbd.in_vars && LivenessInfo.is_alive variable dated_info_list) (LivenessInfo.elements dated_info_list) in
      
      (* Be careful new statement are in the reversed order *)
      let stmts, lastest_live_info = bbd.basic_block.cfg_statements |> List.fold_left (fun (block_line_index, cfg_liveness_statements, last_dated_info_list) stmt ->
        let dated_info_list = last_dated_info_list in
        let listof_now_dying_var = fetch_dying_variable ~current:block_line_index when_to_die_hashmap () in
        let date_info_updated_dying = LivenessInfo.set_dead_of_list listof_now_dying_var dated_info_list in
        let next_line = block_line_index + 1 in
        match stmt with
        | CFG_STacDeclaration {identifier; trvalue} 
        | CFG_STacModification {identifier; trvalue} -> 
          let variable = lvalue_variable identifier trvalue in
          
          (* let () = Printf.printf "line : %u, dying = [%s]\n%!" block_line_index (listof_now_dying_var |> List.map CfgS.repr |> String.concat ", ") in *)

          begin match does_outlives_block variable bbd with
          | true -> begin 
            let updated_alive_info = if CfgS.is_affectation trvalue then (LivenessInfo.set_alive variable date_info_updated_dying) else date_info_updated_dying in
            next_line,  {cfg_statement = stmt; liveness_info = dated_info_list}::cfg_liveness_statements, updated_alive_info
          end
          | false -> begin match CfgS.is_affectation trvalue with
            | false -> next_line, {cfg_statement = stmt; liveness_info = date_info_updated_dying}::cfg_liveness_statements, date_info_updated_dying
            | true -> 
              let in_how_many_times_it_dies = when_variable_dies_unsafe ~start_from:block_line_index variable bbd in
              let does_it_dies_now = in_how_many_times_it_dies |> Option.map (( = ) block_line_index) |> Option.value ~default:false in
              if 
                does_it_dies_now 
              then
                let updated_dead_liveinfo = (LivenessInfo.set_dead variable date_info_updated_dying) in
                next_line, 
                ( if delete_useless_stmt then cfg_liveness_statements
                else
                  {cfg_statement = stmt; liveness_info = dated_info_list}::cfg_liveness_statements
                )
                ,  updated_dead_liveinfo
              else
                let () = Hashtbl.replace when_to_die_hashmap (in_how_many_times_it_dies, variable) variable in
                let updated_alive_info = (LivenessInfo.set_alive variable date_info_updated_dying) in
                next_line,  {cfg_statement = stmt; liveness_info = dated_info_list}::cfg_liveness_statements, updated_alive_info
           end 
        end
        | CFG_STDerefAffectation {identifier; trvalue} -> 
          let variable = lvalue_deref_variable identifier trvalue in
          let updated_alive_info = (LivenessInfo.set_alive variable date_info_updated_dying) in
          next_line,  {cfg_statement = stmt; liveness_info = dated_info_list}::cfg_liveness_statements, updated_alive_info
      ) (0, ([]) , dated_info_list) |> fun (_, list, latest_liveness_info) -> 
        match list with
        | [] -> [], dated_info_list
        | _::_ as l -> List.rev l, latest_liveness_info
      in
      stmts, lastest_live_info

        
    (*
      Relie on the fac that a variable create in a branch block cannot be used in a following block
      It's mostlt true for staticlly typed language but false for dynamic not like Python   
    *)
    let rec basic_block_liveness_of_convert ~delete_useless_stmt ~visited ~dated_info basic_blocks_map (bbd: (cfg_statement, basic_block_end option) Detail.basic_block_detail) = 

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
              ending = (bbd.basic_block.ending, last_dated_info) ;
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
      ) (TypedIdentifierSet.union cfg.locals_vars cfg.parameters) [] |> LivenessInfo.of_list in
      let open Detail in
      {
        entry_block = cfg.entry_block;
        parameters = cfg.parameters;
        locals_vars = cfg.locals_vars;
        blocks_liveness_details = basic_block_liveness_of_convert ~delete_useless_stmt ~visited ~dated_info:liveness_info cfg.blocks_details entry_block
      }

    

   end

   module Inference_Graph = struct
     module IG = Graph.Make(TypedIdentifierSig)

     let infer_acc liveness_info graph = 
      let alive_elt = Liveness.LivenessInfo.alive_elements liveness_info in
      let combined_alives = List.flatten @@ Util.combinaison (fun lhs rhs -> if TypedIdentifierSig.compare lhs rhs = 0 then None else Some (lhs, rhs)) alive_elt alive_elt in
      combined_alives |> List.fold_left (fun acc_graph (link, along)  -> 
        (* let lname, aname = fst link, fst along in *)
        (* let () = Printf.printf "lname = %s, rname = %s\n%!" lname aname in *)
        IG.link link ~along acc_graph
        ) graph


     let infer (cfg: Liveness.cfg_liveness_detail) = 
      let open Basic in
      let open Detail in
      let open Liveness in
      let all_variables_seq = TypedIdentifierSet.to_seq @@ TypedIdentifierSet.union cfg.locals_vars cfg.parameters in
      let graph = IG.of_seq all_variables_seq in
      BasicBlockMap.fold (fun _ block graph_acc -> 
        let new_graph = block.basic_block.cfg_statements |> List.fold_left (fun inner_graph_acc stmt ->
            infer_acc stmt.liveness_info inner_graph_acc
          ) graph_acc 
        in
        let ending, liveness = block.basic_block.ending in
        match ending with
        | None -> new_graph
        | Some (BBe_if {condition = _; _} | Bbe_return _) -> 
          infer_acc liveness new_graph
      ) cfg.blocks_liveness_details graph
   end

   module GridyAllocator = struct
     
   end
end