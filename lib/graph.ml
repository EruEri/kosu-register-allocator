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

  type colored_node = {
    node: S.t;
    color: Color.color option
  }

  type egde = {
    root: S.t;
    along: S.t
  }

  let compare_opt lhs rhs = 
    match lhs, rhs with
    | Some lcolor, Some rcolor -> Color.compare lcolor rcolor
    | _ -> compare lhs rhs


  exception Unexisting_node of S.t
  exception Link_same_color of colored_node * colored_node

  module Nodes = Set.Make(struct
    type t = colored_node

    let compare lhs rhs = S.compare lhs.node rhs.node
  end)

  module Edges = Set.Make(struct
    type t = egde

    let compare lhs rhs = 
      let f_comp = S.compare lhs.root rhs.root in
      if f_comp <> 0 then f_comp
      else
        S.compare lhs.along rhs.along
  end)

  type colored_graph = {
    nodes: Nodes.t;
    egdes: Edges.t
  }

  let empty = {
    nodes = Nodes.empty;
    egdes = Edges.empty;
  }

  let create_colored color node = 
    {
      node;
      color
    }

  let create_edge root along = 
    {
      root = root.node;
      along = along.node
    }

  let add_node node graph = 
    {
      graph with nodes = Nodes.add node graph.nodes
    }

  let add_edge egde graph = 
    {
      graph with egdes = Edges.add egde graph.egdes
    }

    let add_uncolored_node ?(color) node graph = 
      add_node (create_colored color node) graph

    let edges node cgraph = 
      cgraph.egdes |> Edges.filter (fun edge ->
        S.compare edge.root node = 0  
      ) 

    let check_contains node graph = 
      if not @@ Nodes.mem (create_colored None node) @@ graph.nodes then 
        raise @@ Unexisting_node node

    let check_color root along = 
      if compare_opt root.color along.color = 0 && Option.is_some root.color then 
        raise @@ Link_same_color (root, along)
      
    let link root ~along graph = 
      let () = check_contains root.node graph in
      let () = check_contains along.node graph in
      let () = check_color root along in
      let () = check_color along root in
      let edge = create_edge root along in
      add_edge edge graph
end