module StringSet = Set.Make(String)

module type Cfg_Sig = sig
  type tac_typed_rvalue
  type tac_typed_expression
  type rktype

  module TypedIdentifierSet : 
    sig include Set.S with type elt = (string * rktype)
  end


  type cfg_statement =
  | CFG_STacDeclaration of { identifier : string; trvalue : tac_typed_rvalue }
  | CFG_STacModification of { identifier : string; trvalue : tac_typed_rvalue }
  | CFG_STDerefAffectation of { identifier : string; trvalue : tac_typed_rvalue }

  type bbe_if = {
    condition: tac_typed_expression;
    if_label: string;
    else_label: string;
  }

  type basic_block_end = 
  | BBe_if of bbe_if
  | Bbe_return of tac_typed_expression

  val compare_type: rktype -> rktype -> int

  val declaration_typed: string -> tac_typed_rvalue -> rktype
  val derefed_typed: string -> tac_typed_rvalue -> rktype

  val ttrv_identifiers_used: tac_typed_rvalue -> (string * rktype) list
  val tte_idenfier_used: tac_typed_expression -> (string * rktype) list
end


module Make(CfgS : Cfg_Sig) = struct

  open CfgS

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
    
    let basic_block_input_var ~out_vars basic_block = 
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
            let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
            let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set killed in
            let extented_killed_vars = TypedIdentifierSet.add (identifier, declaration_typed identifier trvalue) killed in
            let new_geneated = TypedIdentifierSet.union remove_block_create_variable_set generated in
            basic_block_cfg_statement_list ~killed:extented_killed_vars ~generated:new_geneated q
    
        | CFG_STDerefAffectation {trvalue; _} | CFG_STacModification {identifier = _; trvalue}  -> 
          let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
          let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set killed in
          let new_geneated = TypedIdentifierSet.union remove_block_create_variable_set generated in
          basic_block_cfg_statement_list ~killed:killed ~generated:new_geneated q
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

    module BasicBlockDetailSet = Set.Make(struct
      type t = cfg_statement basic_block_detail
      let compare (lhs: t) (rhs: t) = String.compare lhs.basic_block.label rhs.basic_block.label
    end)

    module BasicBlockDetailMap = Map.Make(String)

    type cfg_detail = {
      entry_block: string;
      blocks_details: (cfg_statement basic_block_detail) BasicBlockDetailMap.t;
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
      blocks_details = cfg.blocks |> BasicBlockMap.bindings |> List.map (fun (label, block) -> label,  basic_block_detail_of_basic_block cfg.blocks block) |> List.to_seq |> BasicBlockDetailMap.of_seq;
      parameters = cfg.parameters;
      locals_vars = cfg.locals_vars;
      }
   end

   module Dated = struct
     
   end
end