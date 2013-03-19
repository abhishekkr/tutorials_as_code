desc 'select current line'
task :select => [:goto_src] do
  puts 'select'
end

desc 'cut current line'
task :cut => [:select] do
  puts 'cut'
end

desc 'copy current line'
task :copy => [:select] do
  puts 'copy'
end

desc 'paste after current line'
task :paste => [:copy, :cut, :goto_dest] do
  puts 'paste'
end

desc 'goto source line'
task :goto_src do
  puts 'goto src'
end

desc 'goto destination line'
task :goto_dest do
  puts 'goto dest'
end
