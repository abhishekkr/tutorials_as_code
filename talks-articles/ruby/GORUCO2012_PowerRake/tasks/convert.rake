# running convert makes Rake to create all File tasks for Thumbs
desc 'convert to thumbs'
task :convert => THUMBS

file "final.png" => THUMBS do
  sh "convert #{THUMBS} -append final.png"
end
desc 'merge thumbs to one'
task :merge => "final.png"
