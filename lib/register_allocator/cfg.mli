module type CfgS = sig
    type variable
    type t = variable
    type tac_typed_rvalue
    type tac_typed_expression
  
    val repr : variable -> string
    val compare : variable -> variable -> int
    val lvalue_variable : string -> tac_typed_rvalue -> variable
    val lvalue_deref_variable : string -> tac_typed_rvalue -> variable
    val ttrv_identifiers_used : tac_typed_rvalue -> variable list
    val tte_idenfier_used : tac_typed_expression -> variable list
    val is_affectation : tac_typed_rvalue -> bool
    val variables_as_parameter : tac_typed_rvalue -> (variable * int) list option
  end
  
  module type CfgPprintSig = sig
    type variable
    type tac_typed_rvalue
    type tac_typed_expression
    
    val string_of_variable: variable -> string
    val string_of_tac_typed_rvalue: tac_typed_rvalue -> string
    val string_of_tac_typed_expression: tac_typed_expression -> string
  end
  
  module type ABI = sig
    type register
    type any
    type rktype
  
    type return_strategy =
      | Indirect_return
      | Simple_return of register
      | Splitted_return of register * register
  
    val compare : register -> register -> int
    val any : any
    val registers : register list
    val callee_saved_register : register list
    val caller_saved_register : register list
    val syscall_register : register list
    val argument_register : register list
    val syscall_register_code : register
    val does_return_hold_in_register : any -> rktype -> bool
    val indirect_return_register : register
    val return_strategy : any -> rktype -> return_strategy
  end

module type ColoredType = Graph.ColoredType

module type VariableSig = sig
    type t
  
    val compare: t -> t -> int
  end

module type S = sig

    type variable
    type tac_typed_rvalue
    type tac_typed_expression

    
    module VariableSig : sig
        include VariableSig with type t = variable 
    end

    module StringSet : sig
        include module type of Set.Make(String)
    end


    module TypedIdentifierSet : sig
        include module type of Set.Make(VariableSig)
    end

    module BasicBlockMap: sig
        include Map.S with type key = String.t 
    end

    type cfg_statement =
    | CFG_STacDeclaration of {
        identifier : string;
        trvalue : tac_typed_rvalue;
      }
    | CFG_STacModification of {
        identifier : string;
        trvalue : tac_typed_rvalue;
      }
    | CFG_STDerefAffectation of {
        identifier : string;
        trvalue : tac_typed_rvalue;
      }

      type bbe_if = {
        condition : tac_typed_expression;
        if_label : string;
        else_label : string;
      }
  
      type basic_block_end =
      | BBe_if of bbe_if
      | Bbe_return of tac_typed_expression

      module Basic : sig
        type ('a, 'b) basic_block = {
            label : string;
            cfg_statements : 'a list;
            followed_by : StringSet.t;
            ending : 'b;
        }

        type cfg = {
            entry_block : string;
            blocks :
              (cfg_statement, basic_block_end option) basic_block BasicBlockMap.t;
            parameters : variable list;
            locals_vars : TypedIdentifierSet.t;
          }

        val create_basic_block: label:string -> cfg_statements:'a list -> followed_by:string list -> ending:'b -> ('a, 'b) basic_block

        val create_cfg: entry_block:string -> parameters:variable list -> locals_vars:variable list -> (cfg_statement, basic_block_end option) basic_block list -> cfg
    end

    module Detail : sig
        type ('a, 'b) basic_block_detail = {
            basic_block : ('a, 'b) Basic.basic_block;
            in_vars : TypedIdentifierSet.t;
            out_vars : TypedIdentifierSet.t;
        }

        type cfg_detail = {
            entry_block : string;
            blocks_details :
              (cfg_statement, basic_block_end option) basic_block_detail
              BasicBlockMap.t;
            parameters : variable list;
            locals_vars : TypedIdentifierSet.t;
        }

        val of_cfg: Basic.cfg -> cfg_detail
    end

    module Liveness : sig
        module LivenessInfo : sig
            include LivenessInfo.LivenessInfoS with type elt = variable
        end

        type cfg_liveness_statement = {
            cfg_statement : cfg_statement;
            liveness_info : LivenessInfo.liveness_info;
        }

        type liveness_ending = basic_block_end option * LivenessInfo.liveness_info


    type cfg_liveness_detail = {
        entry_block : string;
        blocks_liveness_details :
          (cfg_liveness_statement, liveness_ending) Detail.basic_block_detail
          BasicBlockMap.t;
        parameters : variable list;
        locals_vars : TypedIdentifierSet.t;
      }

    val of_cfg_details: delete_useless_stmt:bool -> Detail.cfg_detail -> cfg_liveness_detail
    end

    module Inference_Graph : sig
        module IG : sig
            include module type of Graph.Make(VariableSig)
        end

        val infer: Liveness.cfg_liveness_detail -> IG.graph
    end

    module GreedyColoring (Color : ColoredType) :
        sig
          module ColoredGraph : sig
            include module type of Graph.ColoredMake(VariableSig)(Color)
          end

          val coloration: parameters:(variable * Color.t) list -> available_color:Color.t list -> Liveness.cfg_liveness_detail -> ColoredGraph.colored_graph
        end

end

module type SP = sig
    include S

    module Pprint: sig
      val string_of_typed_indentifier_set: TypedIdentifierSet.t -> string 

      val string_of_cfg_statement: cfg_statement -> string

      val string_of_cfg_liveness_statement: Liveness.cfg_liveness_statement -> string

      val string_of_basic_block_end: basic_block_end -> string
    end
end

module Make (CfgS: CfgS) : S with 
    type variable = CfgS.variable and 
    type tac_typed_expression  = CfgS.tac_typed_expression and
    type tac_typed_rvalue = CfgS.tac_typed_rvalue

module MakePprint(CfgS: CfgS)(Pp: CfgPprintSig with 
    type variable = CfgS.variable and 
    type tac_typed_expression = CfgS.tac_typed_expression and
    type tac_typed_rvalue = CfgS.tac_typed_rvalue
  ) : SP with 
  type variable = CfgS.variable and 
  type tac_typed_expression  = CfgS.tac_typed_expression and
  type tac_typed_rvalue = CfgS.tac_typed_rvalue