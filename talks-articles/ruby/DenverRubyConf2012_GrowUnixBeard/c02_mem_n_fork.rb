#!/usr/bin/env ruby

@sample = Array(1..10)

pid = Process.fork do
        @sample.delete_at(-1)
        puts "Child:  #@sample"
      end

Process.wait(pid)
puts "Parent: #@sample"
