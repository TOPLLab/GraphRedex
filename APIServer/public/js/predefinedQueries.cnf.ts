export default [
    {
        name: "All paths to selected",
        query: `LET backReachable = FLATTEN(
		FOR v IN 0..1000 INBOUND @focus GRAPH @graph
			OPTIONS {bfs:true,uniqueVertices: 'global'}
			RETURN DISTINCT v._key)

LET nodes = FLATTEN(
		FOR v IN 0..1000 OUTBOUND @start GRAPH @graph
			OPTIONS {bfs:true,uniqueVertices: 'global'}
			FILTER (v._key IN backReachable)
			RETURN DISTINCT v)

LET nodesKeys = nodes[*]._id

LET edges = (FOR e IN @@edges
						 FILTER
								e._from IN nodesKeys OR e._to IN nodesKeys
						 RETURN DISTINCT e)

RETURN {nodes,edges}`,
    },
    {
        name: "Shortest path to selected",
        query: `
				LET path = (
								FOR v,e IN OUTBOUND SHORTEST_PATH
										@start TO @focus GRAPH @graph
												RETURN {v,e})

				// convert to needed format
				RETURN {
						edges: (FOR d in path FILTER d.e != null RETURN DISTINCT d.e),
						nodes: (FOR d in path FILTER d.v != null RETURN DISTINCT d.v)
				}`,
    },
    {
        name: "Shortest path to all stuck",
        query: `// find all stuck nodes
				LET stuckNodes = (FOR n in @@nodes FILTER n._stuck RETURN n)

				// find path to each of them and join the results
				LET path = FLATTEN(
						FOR target IN stuckNodes
								FOR v,e IN OUTBOUND SHORTEST_PATH
										@start TO target._id GRAPH @graph
												RETURN {v,e})

				// convert to needed format
				RETURN {
						edges: (FOR d in path FILTER d.e != null RETURN DISTINCT d.e),
						nodes: (FOR d in path FILTER d.v != null RETURN DISTINCT d.v)
				}`,
    },
];
