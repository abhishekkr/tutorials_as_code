#!/usr/bin/env julia
#=
#
# at Package Manager
# * add Fire
#
# Help: julia ${path_to_code}/basic-cli.jl --help
=#

using Fire

"""
This will come in help for 'greet'
julia \${path_to_code}/basic-cli.jl greet ABK
"""
@main function greet(name)
  println("Hey, $name")
end

"""add all provided numbers"""
@main function add(num::Integer...)
  println("Adding: $num")
  println(sum(num))
end

@main function sysinfo(entity)
  lowercase_entity = lowercase(entity)
  if lowercase_entity == "cpu"
    Sys.cpu_summary()
  elseif lowercase_entity == "mem"
    println("Total Mem: $(Sys.total_memory() / (2*1024)) | Free Mem: $(Sys.free_memory() / (2*1024))")
  else
    println("[error] available options: cpu | mem")
  end
end
