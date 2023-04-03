module type OrderedType = sig
  type t

  val compare: t -> t -> int
end

module Make(O: OrderedType) = struct

  module IndexedO = struct
    type t = (O.t * int)

    let compare (lt, li) (rt, ri) = 
      let elt_cmp = O.compare lt rt in
      if elt_cmp <> 0 then elt_cmp
      else Int.compare li ri
  end

  module ReturnSet = Set.Make(O)
  module ParameterSet = Set.Make(IndexedO)
  module ParameterSetSet = Set.Make(ParameterSet)

  type constraints = {
    parameters: ParameterSetSet.t ;
    return: ReturnSet.t
  }

  let empty = { parameters = ParameterSetSet.empty; return = ReturnSet.empty }

  let add_return_constraint return constr = {
      constr with return = ReturnSet.add return constr.return
  }

  let add_returns_constraints returns constr =
    let returns_set = ReturnSet.of_list returns in
    {
      constr with return = ReturnSet.union constr.return returns_set
    }

  let add_function_constraint parameters constr = 
  if parameters = [] then constr
  else
    let parameters_set = parameters |> ParameterSet.of_list in
    {
      constr with parameters = ParameterSetSet.add parameters_set constr.parameters
    }

  let return_constraints constraints = constraints.return |> ReturnSet.elements

  let parameters_constraints constraints = constraints.parameters |> ParameterSetSet.elements |> List.map ParameterSet.elements

  let union lhs rhs = {
    parameters = ParameterSetSet.union lhs.parameters rhs.parameters;
    return = ReturnSet.union lhs.return rhs.return
  }
end