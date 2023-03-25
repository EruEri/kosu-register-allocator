module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module Make(S: OrderedType) = struct
  module NodeSet = Set.Make(S)
  module NodeMap = Map.Make(S)



  type graph = NodeSet.t NodeMap.t

  let add_node (node: S.t) (graph: graph) = 
    let set = match NodeMap.find_opt node graph with
    | Some set -> set
    | None -> NodeSet.empty in
    NodeMap.add node set graph

  let of_seq (nodes: S.t Seq.t): graph = 
    nodes |> Seq.map (fun node -> node, NodeSet.empty) |> NodeMap.of_seq

  let contains node graph = NodeMap.mem node graph

  (**
    @raise Not_found if [node] is not in [graph]    
  *)
  let check_contains node graph = 
    if not @@ contains node graph then raise Not_found

  (**
      @raise Not_found if [node] is not in [graph]    
  *)
  let egde_to : S.t -> graph -> NodeSet.t = 
    NodeMap.find


  (** 
    Add an egde beetween [node] and [along] in [graph]. 
    @raise Not_found if [node] or [along] aren't in the graph
  *)
  let link (node: S.t) ~(along : S.t) (graph: graph): graph = 
    let () = check_contains node graph in
    let () = check_contains along graph in
    let linked_to = egde_to node graph in
    let extended_node = NodeSet.add along linked_to in
    NodeMap.add node extended_node graph

  let mutual_link (node1: S.t) (node2: S.t) (graph: graph): graph =
    let graph = link node1 ~along:node2 graph in
    link node2 ~along:node1 graph

  let merge (lhs: graph) (rhs: graph): graph = 
    NodeMap.union (fun _ lset rset -> 
      Option.some @@ NodeSet.union lset rset
    ) lhs rhs

  let bindings (graph: graph) = 
    graph 
    |> NodeMap.bindings
    |> List.map (fun (node, set) -> node, NodeSet.elements set)
end