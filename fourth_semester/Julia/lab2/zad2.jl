module Graphs

using StatsBase

export GraphVertex, NodeType, Person, Address,
       generate_random_graph, get_random_person, get_random_address, generate_random_nodes,
       convert_to_graph,
       bfs, check_euler, partition,
       graph_to_str, node_to_str,
       test_graph


 # Types of valid graph node's values.
 abstract NodeType

 type Person <: NodeType
   name :: AbstractString
 end

 type Address <: NodeType
   streetNumber :: Int64
 end

#= Single graph vertex type.
Holds node value and information about adjacent vertices =#

##################################################
############### opt. 1 - typy
##################################################
type GraphVertex{T<:NodeType}
  value :: T
  neighbors ::Vector{GraphVertex}
end




#= Generates random directed graph of size N with K edges
and returns its adjacency matrix.=#
function generate_random_graph(N::Int64, K::Int64)
    A = Array{Int64,2}(N, N)

    for i=1:N, j=1:N
      A[i,j] = 0
    end

    for i in sample(1:N*N, K, replace=false)
      A[i] = 1
    end
    A
end

# Generates random person object (with random name).
function get_random_person()
  Person(randstring())
end

# Generates random person object (with random name).
function get_random_address()
  Address(rand(1:100))
end

# Generates N random nodes (of random NodeType).
function generate_random_nodes(N :: Int64)
  nodes = Array{NodeType,1}()
  for i= 1:N
    push!(nodes, rand() > 0.5 ? get_random_person() : get_random_address())
  end
  nodes
end

#= Converts given adjacency matrix (NxN)
  into list of graph vertices (of type GraphVertex and length N). =#
###############################################
############### opt 1 - typy ##################
###############################################
function convert_to_graph(A::Array{Int64,2}, nodes:: Array{NodeType,1}, graph:: Array{GraphVertex,1})
  N = length(nodes)
  push!(graph, map(n -> GraphVertex(n, GraphVertex[]), nodes)...)

  for i = 1:N, j = 1:N
      if A[i,j] == 1
        push!(graph[i].neighbors, graph[j])
      end
  end
end

#= Groups graph nodes into connected parts. E.g. if entire graph is connected,
  result list will contain only one part with all nodes. =#
function partition(graph:: Array{GraphVertex,1})
  parts = []
  remaining = Set(graph)
  visited = bfs(graph, remaining=remaining)
  push!(parts, Set(visited))

  while !isempty(remaining)
    new_visited = bfs(graph, visited=visited, remaining=remaining)
    push!(parts, new_visited)
  end
  parts
end

#= Performs BFS traversal on the graph and returns list of visited nodes.
  Optionally, BFS can initialized with set of skipped and remaining nodes.
  Start nodes is taken from the set of remaining elements. =#
function bfs(graph:: Array{GraphVertex,1};visited=Set(), remaining=Set())
  if(remaining == Set())
    remaining = Set(graph)
  end
  first = next(remaining, start(remaining))[1]
  q = Array{GraphVertex,1}()
  push!(q, first)
  push!(visited, first)
  delete!(remaining, first)
  local_visited = Set(q)

  while !isempty(q)
    v = pop!(q)
    for n in v.neighbors
      if !(n in visited)
        push!(q, n)
        push!(visited, n)
        push!(local_visited, n)
        delete!(remaining, n)
      end
    end
  end
  local_visited
end

#= Checks if there's Euler cycle in the graph by investigating
   connectivity condition and evaluating if every vertex has even degree =#
function check_euler(graph:: Array{GraphVertex,1})
  if length(partition(graph)) == 1
    return all(map(v -> iseven(length(v.neighbors)), graph))
  end
    "Graph is not connected"
end

##################################################################
################# opt 3. - mniejsze funkcje bez isa ##############
##################################################################
function vertex_to_string(v::GraphVertex{Person})
  return "Person: $(v.value.name)\n"
end

function vertex_to_string(v::GraphVertex{Address})
  return "Street nr: $(v.value.streetNumber)\n"
end

#= Returns text representation of the graph consisiting of each node's value
   text and number of its neighbors. =#
###############################################################
############### opt. 2 - zmienne globalne #####################
###############################################################
function graph_to_str(graph:: Array{GraphVertex,1})
  graph_str = ""
  for v in graph
    graph_str *= "****\n"
    graph_str *= vertex_to_string(v)
    graph_str *= "Neighbors: $(length(v.neighbors))\n"
  end
  graph_str
end

#= Tests graph functions by creating 100 graphs, checking Euler cycle
  and creating text representation. =#
function test_graph()
  for i=1:100

    ###############################################################
    ############### opt. 2 - zmienne globalne #####################
    ###############################################################
    graph = GraphVertex[]
    # Number of graph nodes.
    N = 800

    # Number of graph edges.
    K = 10000

    A = generate_random_graph(N, K)
    nodes = generate_random_nodes(N)
    convert_to_graph(A, nodes, graph)

    str = graph_to_str(graph)
    # println(str)
    println(check_euler(graph))
  end
end

end
