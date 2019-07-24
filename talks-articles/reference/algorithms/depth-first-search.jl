#!/usr/bin/env julia
#=
# Searching all vertices in graph/tree in a depth-first motion,
# marking each vertex as visited/not-visited while avoiding cycles,
# uses queue to remember next vertex to start search, backtracks on dead-end
#
# We've implemented in 1 ways here
# * xdfs() using recursion and 2 global queues
=#

function mark_neighbours(vertices, linkings)
  result = Dict(v => Set{eltype(vertices)}() for v in vertices)
  for (vertex,neighbour) in linkings
    push!(result[vertex], neighbour)
    push!(result[neighbour], vertex)
  end
  result
end

xroute, xmarked = [], []
function xdfs(adjlist, source, dest)
    global xroute
    global xmarked
    append!(xroute, [source])
    if source == dest
      return xroute
    end
    append!(xmarked, [source])

    for vertex in adjlist[source]
      if in(vertex, xmarked)
        continue
      end
      if vertex == dest
        append!(xroute, [vertex])
        return xroute
      end
    end

    for vertex in adjlist[source]
      if in(vertex, xmarked)
        continue
      end
      tmp_xroute = xdfs(adjlist, vertex, dest)
      if tmp_xroute != []
        return tmp_xroute
      end
    end

    []
end


const people = Set([:jean, :javert, :cosette, :gavroche, :éponine, :marius, :alice])
const friendships = [
    (:jean, :cosette),
    (:jean, :marius),
    (:cosette, :éponine),
    (:cosette, :marius),
    (:gavroche, :éponine),
    (:gavroche, :alice),
]

println("-------------------------------------------")

xroute, xmarked = [], []
@time println(xdfs(mark_neighbours(people, friendships), :jean, :jean))
xroute, xmarked = [], []
@time println(xdfs(mark_neighbours(people, friendships), :jean, :cosette))
xroute, xmarked = [], []
@time println(xdfs(mark_neighbours(people, friendships), :jean, :gavroche))
xroute, xmarked = [], []
@time println(xdfs(mark_neighbours(people, friendships), :jean, :javert))
xroute, xmarked = [], []
@time println(xdfs(mark_neighbours(people, friendships), :jean, :alice))

println("-------------------------------------------")
