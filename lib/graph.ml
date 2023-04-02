module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type ColoredType = sig
  type color

  val compare: color -> color -> int
end

module Make(S: OrderedType) = struct
  module NodeSet = Set.Make(S)
  module NodeMap = Map.Make(S)



  type graph = NodeSet.t NodeMap.t

  let empty: graph = NodeMap.empty

  let fold func init (graph: graph) = NodeMap.fold (fun key set acc -> func acc key set) graph init

  let exist_node func = NodeMap.exists (fun key _ -> func key)

  (**
    add a new node to the graph. the graph is physically equal if the node was already in the graph    
  *)
  let add_node (node: S.t) (graph: graph) = 
     match NodeMap.find_opt node graph with
    | Some _ -> graph
    | None -> 
       NodeMap.add node NodeSet.empty  graph

  let map = 
    NodeMap.map

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

module ColoredMake(S: OrderedType) (Color: ColoredType) = struct

  module SColored = struct
    type colored_t = {
      node: S.t;
      color: Color.color option
    }

    type t = colored_t

    let compare_opt lhs rhs = 
      match lhs, rhs with
      | Some lcolor, Some rcolor -> Color.compare lcolor rcolor
      | _ -> compare lhs rhs

    let compare lhs rhs = 
      let s_compare = S.compare lhs.node rhs.node in
      if s_compare <> 0 then  s_compare
      else compare_opt lhs.color rhs.color
  end

  module ColoredGraph = Make(SColored)
  module NoColoredGraph = Make(S)

  exception Link_same_color

  type graph = NoColoredGraph.graph
  type colored_graph = ColoredGraph.graph

  let make_node color elt = 
    let open SColored in
  {
    color = Some color;
    node = elt
  }

  let make_node_opt color elt = 
    let open SColored in
  {
    color = color;
    node = elt
  }

  let compare_cnode_as_node lhs rhs = 
    let open SColored in
    S.compare lhs.node rhs.node


  let add_colored_node cnode cgraph = 
    let new_graph =  ColoredGraph.add_node cnode cgraph in
    if new_graph = cgraph then new_graph
    else
      cgraph |> ColoredGraph.map (fun set ->
        set |> ColoredGraph.NodeSet.map (fun colored_node ->
          if compare_cnode_as_node colored_node cnode = 0 then cnode else colored_node
      )
    )

  let add_node ?(color = None) elt cgraph =
    let open SColored in 
    let cnode = { node = elt; color } in
    add_colored_node cnode cgraph



  let egde_to = 
    ColoredGraph.egde_to

    

  let contains node = let open SColored in ColoredGraph.exist_node (fun colored_node -> 
    S.compare colored_node.node node.node = 0
  )

  (**
    @raise Not_found if [node] is not in [graph]    
  *)
  let check_contains node graph = 
    if not @@ contains node graph then raise Not_found

  let check_not_same_color lhs rhs = 
    let open SColored in
    let () = if SColored.compare_opt lhs.color rhs.color = 0 && Option.is_some lhs.color then raise Link_same_color
    in
    ()


  (** 
    Add an egde beetween [node] and [along] in [graph]. 
    @raise Not_found if [node] or [along] aren't in the graph
    @raise Link_same_color if [node] has the same color than [along] 
  *)
  let link (node: SColored.t) ~(along : SColored.t) (graph: colored_graph): colored_graph = 
    let () = check_contains node graph in
    let () = check_contains along graph in
    let () = check_not_same_color node along in
    let linked_to = egde_to node graph in
    let extended_node = ColoredGraph.NodeSet.add along linked_to in
    ColoredGraph.NodeMap.add node extended_node graph
    let color_links ~precolored linked = 
      linked |> List.map (fun node ->
        let color = precolored |> List.find_map (fun (elt, color) -> if S.compare node elt = 0 then Some color else None ) in
        make_node_opt color node 
      )
    let of_graph ?(precolored = ( []: (S.t * Color.color) list ) ) graph = 
      let open SColored in
       graph |> NoColoredGraph.bindings |> List.fold_left (fun graph_acc (node, linked) -> 
        let colored_opt = precolored |> List.find_map (fun (elt, color) -> if S.compare node elt = 0 then Some color else None ) in
        let cnode = make_node_opt colored_opt node in
        let clinked = color_links ~precolored linked in
        let graph_acc = add_colored_node cnode graph_acc in
        let graph_acc = clinked |> List.fold_left (fun inner_graph cl -> 
          let new_graph = inner_graph |> add_colored_node cl |> link cnode ~along:cl in
          new_graph
        ) graph_acc in
        graph_acc
      ) ColoredGraph.empty

    let color_node color cnode cgraph = failwith ""
      
      (* graph |>  NoColoredGraph.fold (fun acc node linked ->
        failwith ""
    ) ColoredGraph.of *)
end