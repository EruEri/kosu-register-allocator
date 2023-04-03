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
  module EgdesSig = struct
    type t = {
      root: S.t;
      along: S.t
    }

    let compare lhs rhs = 
      let tmp = S.compare lhs.root rhs.root in
      if tmp <> 0 then tmp
      else S.compare lhs.along rhs.along
  end
  module EdgeSet = Set.Make(EgdesSig)


  type graph = {
    nodes: NodeSet.t;
    edges: EdgeSet.t
  }



  let empty: graph = {
    nodes = NodeSet.empty;
    edges = EdgeSet.empty
  }

  let exist_node func graph = NodeSet.exists (fun key -> func key) graph.nodes

  (**
    add a new node to the graph. the graph is physically equal if the node was already in the graph    
  *)
  let add_node (node: S.t) (graph: graph) = NodeSet.add node graph.nodes

  let of_seq (nodes: S.t Seq.t): graph = 
    let node_set =  nodes |> Seq.map (fun node -> node) |> NodeSet.of_seq in
    {
      nodes = node_set;
      edges = EdgeSet.empty
    }

  let contains node graph = NodeSet.mem node graph.nodes

  (**
    @raise Not_found if [node] is not in [graph]    
  *)
  let check_contains node graph = 
    if not @@ contains node graph then raise Not_found

  (**
      @raise Not_found if [node] is not in [graph]    
  *)
  let egde_of : S.t -> graph -> NodeSet.t = fun node graph ->
    let open EgdesSig in 
    EdgeSet.fold (fun egde acc -> 
      if S.compare egde.root node = 0 then 
        NodeSet.add egde.along acc
      else
        acc
    ) graph.edges NodeSet.empty


  (** 
    Add an egde beetween [node] and [along] in [graph]. 
    @raise Not_found if [node] or [along] aren't in the graph
  *)
  let link (node: S.t) ~(along : S.t) (graph: graph): graph = 
    let open EgdesSig in
    let () = check_contains node graph in
    let () = check_contains along graph in
    let edge = {root = node; along} in
    {
      graph with edges = EdgeSet.add edge graph.edges
    }

  let mutual_link (node1: S.t) (node2: S.t) (graph: graph): graph =
    let graph = link node1 ~along:node2 graph in
    link node2 ~along:node1 graph

  let merge (lhs: graph) (rhs: graph): graph = {
    nodes = NodeSet.union lhs.nodes rhs.nodes;
    edges = EdgeSet.union lhs.edges rhs.edges
  }

  let bindings (graph: graph) = 
    let open EgdesSig in
    NodeSet.fold (fun node acc ->
      let egdes = egde_of node graph in 
      (node, NodeSet.elements egdes)::acc
    ) graph.nodes []
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

  module G = Make(S)

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


  

    let of_graph ?(precolored = ( []: (S.t * Color.color) list ) ) (graph: G.graph) = 
      let color_links ~precolored linked = 
        linked |> List.map (fun node ->
          let color = precolored |> List.find_map (fun (elt, color) -> if S.compare node elt = 0 then Some color else None ) in
          create_colored color node 
        ) 
      in
      graph |> G.bindings |> List.fold_left (fun graph_acc (node, linked) ->
        let colored_opt = precolored |> List.find_map (fun (elt, color) -> if S.compare node elt = 0 then Some color else None ) in
        let cnode = create_colored colored_opt node in
        let clinked = color_links ~precolored linked in
        let graph_acc = add_node cnode graph_acc in
        let graph_acc = clinked  |> List.fold_left (fun inner_graph cl -> 
          let new_graph = inner_graph |> add_node cl |> link cnode ~along:cl in
          new_graph
        ) graph_acc in
        graph_acc
      ) empty
end