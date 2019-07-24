#!/usr/bin/env julia
#=
# Searching all vertices in graph/tree in a breadth-first motion,
# marking each vertex as visited/not-visited while avoiding cycles,
# uses queue to remember next vertex to start search, iteration on dead-end
#
# We've implemented in 3 ways here
# * xbfs() uses a clever single queue based approach
# * ybfs() uses recursion with 2 queues
# * zbfs() uses external 2 queues
=#

function mark_neighbours(vertices, linkings)
  result = Dict(v => Set{eltype(vertices)}() for v in vertices)
  for (vertex,neighbour) in linkings
    push!(result[vertex], neighbour)
    push!(result[neighbour], vertex)
  end
  result
end


function xbfs(adjlist, source, dest)
    distances = Dict(source => 0)
    queue = [source]
    route = [source]

    while !isempty(queue)
        current = pop!(queue)

        if current == dest
                push!(route,current)
                println(route)
            return distances[dest]
        end

        for neighbour in adjlist[current]
            if !haskey(distances, neighbour)
                distances[neighbour] = distances[current] + 1
                push!(queue, neighbour)
              else
                if !in(current, route)
                  push!(route,current)
                end
            end
        end
    end

   -1
end
xbfs(people, friendships, source, dest) = xbfs(mark_neighbours(people, friendships), source, dest)


function ybfs(neighbour_map, source, destination, link = [], marked = [])
  if in(source, zmarked)
    return []
  end
  append!(link, [ source ])
  if source == destination
    return link
  end

  append!(marked, [ source ])

  for neighbour in neighbour_map[source]
    if neighbour == destination
      append!(link, [ neighbour ])
      return link
    end
  end

  for neighbour in neighbour_map[source]
    if in(destination, neighbour_map[neighbour])
      append!(link, [ neighbour, destination ])
      return link
    end
  end

  for neighbour in neighbour_map[source]
    if in(neighbour, marked)
      continue
    end
    tmp_link = ybfs(neighbour_map, neighbour, destination, link, marked)
    if length(tmp_link) != 0
      return tmp_link
    end
  end
  []
end


zlink, zmarked, bfs_fin = [], [], false
function zbfs(neighbour_map, source, destination)
  global zlink
  global zmarked
  if in(source, zmarked)
    return []
  end
  append!(zlink, [ source ])
  if source == destination
    return zlink
  end

  append!(zmarked, [ source ])

  for neighbour in neighbour_map[source]
    if neighbour == destination
      append!(zlink, [ neighbour ])
      return zlink
    end
  end

  for neighbour in neighbour_map[source]
    if in(destination, neighbour_map[neighbour])
      append!(zlink, [ neighbour, destination ])
      return zlink
    end
  end

  for neighbour in neighbour_map[source]
    if in(neighbour, zmarked)
      continue
    end
    tmp_link = zbfs(neighbour_map, neighbour, destination)
    if length(tmp_link) != 0
      return tmp_link
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

@time println(xbfs(people, friendships, :jean, :jean))
@time println(xbfs(people, friendships, :jean, :cosette))
@time println(xbfs(people, friendships, :jean, :gavroche))
@time println(xbfs(people, friendships, :jean, :javert))
@time println(xbfs(people, friendships, :jean, :alice))

println("-------------------------------------------")

@time println(ybfs(mark_neighbours(people, friendships), :jean, :jean))
@time println(ybfs(mark_neighbours(people, friendships), :jean, :cosette))
@time println(ybfs(mark_neighbours(people, friendships), :jean, :gavroche))
@time println(ybfs(mark_neighbours(people, friendships), :jean, :javert))
@time println(ybfs(mark_neighbours(people, friendships), :jean, :alice))

println("-------------------------------------------")

zlink, zmarked, bfs_fin = [], [], false
@time println(zbfs(mark_neighbours(people, friendships), :jean, :jean))
zlink, zmarked, bfs_fin = [], [], false
@time println(zbfs(mark_neighbours(people, friendships), :jean, :cosette))
zlink, zmarked, bfs_fin = [], [], false
@time println(zbfs(mark_neighbours(people, friendships), :jean, :gavroche))
zlink, zmarked, bfs_fin = [], [], false
@time println(zbfs(mark_neighbours(people, friendships), :jean, :javert))
zlink, zmarked, bfs_fin = [], [], false
@time println(zbfs(mark_neighbours(people, friendships), :jean, :alice))

println("-------------------------------------------")
