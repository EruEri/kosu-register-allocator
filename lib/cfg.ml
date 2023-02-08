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

  type basic_block_end = 
  | BBe_if of {
    condition: tac_typed_expression;
    if_label: string;
    else_label: string;
  }
  | Bbe_return of tac_typed_expression

  type 'a basic_block = {
    label: string;
    cfg_statements: 'a list;
    followed_by: StringSet.t;
    ending: basic_block_end option
  }
  module BasicBlockSet :
    sig include Set.S with type elt = (cfg_statement basic_block)
  end  

  type cfg = {
    entry_block: string;
    blocks: BasicBlockSet.t
  }

  val compare_type: rktype -> rktype -> int

  val declaration_typed: string -> tac_typed_rvalue -> rktype
  val derefed_typed: string -> tac_typed_rvalue -> rktype

  val ttrv_identifiers_used: tac_typed_rvalue -> (string * rktype) list
  val tte_idenfier_used: tac_typed_expression -> (string * rktype) list
end


module Make(CfgS : Cfg_Sig) = struct

  open CfgS


let fetch_basic_block_from_label label_name bbset = 
  bbset |> BasicBlockSet.elements |> List.find (fun bb -> bb.label = label_name) 

let basic_block_input_var basic_block = 
  let rec basic_block_cfg_statement_list ~created ~acc = function
  | [] ->
    basic_block.ending 
    |> Option.map (fun (Bbe_return tte | BBe_if {condition = tte; _}) -> 
      let right_value_variables_used_set = tte |> tte_idenfier_used |> TypedIdentifierSet.of_list in
      let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_variables_used_set created in
      TypedIdentifierSet.union acc remove_block_create_variable_set
    ) |> Option.value ~default:acc
  | stmt::q -> 
    begin match stmt with
    | CFG_STacDeclaration {identifier; trvalue}
      -> 
        let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
        let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set created in
        let extented_created_var = TypedIdentifierSet.add (identifier, declaration_typed identifier trvalue) created in
        let new_acc = TypedIdentifierSet.union remove_block_create_variable_set acc in
        basic_block_cfg_statement_list ~created:extented_created_var ~acc:new_acc q

    | CFG_STDerefAffectation {trvalue; _} | CFG_STacModification {identifier = _; trvalue}  -> 
      let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
      let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set created in
      let new_acc = TypedIdentifierSet.union remove_block_create_variable_set acc in
      basic_block_cfg_statement_list ~created:created ~acc:new_acc q
    end
  in
  basic_block_cfg_statement_list ~created:TypedIdentifierSet.empty ~acc:TypedIdentifierSet.empty basic_block.cfg_statements

let basic_block_output_var basic_block_set basic_block = 
  match basic_block.ending with
  | Some (Bbe_return _) -> TypedIdentifierSet.empty
  | _ -> 
     StringSet.fold (fun elt acc -> 
      let follow_block = fetch_basic_block_from_label elt basic_block_set in 
      let follow_basic_block_input = basic_block_input_var follow_block in
      TypedIdentifierSet.union acc follow_basic_block_input
    ) basic_block.followed_by TypedIdentifierSet.empty
    
end

