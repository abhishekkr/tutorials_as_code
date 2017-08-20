#!ruby
# file, dir, file::stat and more

require 'pathname'

#build paths
dir = Pathname.pwd # or Pathname.new(...)
path = dir + __FILE__ # add paths

#work with paths
puts path.realpath, path.relative_path_from(dir), ''

#use paths to do work
path.open do |io|
  5.times do
    puts io.gets
  end
end
