#!rake
# examples

desc 'different usage of FileList'
task :filelists do
  # more magic of FileList
  JPG_FILES = FileList['images/**/*.jpg']
  PNG_FILES = FileList['images/*.png']
  IMG1_FILES = FileList['images/**/*.jpg', 'images/*.png']
  IMG2_FILES = PNG_FILES.include('images/**/*.jpg')
  IMG3_FILES = PNG_FILES.exclude('images/*thumb*')
  IMG4_FILES = FileList['images/**/*.{jpg,png}']
  IMG5_FILES = PNG_FILES.include('images/**/*.jpg').exclude('*thumb*')
  p IMG_FILES
  p IMG1_FILES
  p IMG2_FILES
  p IMG3_FILES
  p IMG4_FILES
  p IMG5_FILES
  IMG4_FILES.each{|img| print img, ' '} ; puts
  p IMG4_FILES.first
  p IMG4_FILES[0]
  p THUMBS
end

desc 'usage of rake pathmap'
task :pathmaps do
  # PathMap
  grandparent_dir = File.expand_path (File.join File.dirname(__FILE__), '..', '..')
  mypath = FileList["#{grandparent_dir}/**/*md"]
  p mypath
  print "%p : ", mypath[0].pathmap("%p"), ' [full path]' ; puts
  print "%d : ", mypath[0].pathmap("%d"), ' [dirname]' ; puts
  print "%f : ", mypath[0].pathmap("%f"), ' [filename with ext]' ; puts
  print "%n : ", mypath[0].pathmap("%n"), ' [filename without ext]' ; puts
  print "%x : ", mypath[0].pathmap("%x"), ' [extension name]' ; puts
  print "%X : ", mypath[0].pathmap("%X"), ' [entire path w/o ext]' ; puts
end
