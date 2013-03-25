#!ruby

require 'fileutils'

FileUtils.mkdir    'tmp' #, options
FileUtils.mkdir_p  'tmp' #, options
FileUtils.ln       __FILE__, 'tmp/a.lnk' #, options
FileUtils.ln_s     __FILE__, 'tmp/b.lnk' #, options
FileUtils.cp       __FILE__, 'tmp/a' #, options
FileUtils.cp_r     __FILE__, 'tmp/b' #, options
FileUtils.mv       'tmp/a', 'tmp/x' #, options
FileUtils.touch    'tmp/abc' #, options
FileUtils.chmod    0777, 'tmp/abc' #, options
FileUtils.chmod_R  0777, 'tmp/abc' #, options
FileUtils.chown    ENV['USER'], 'root', 'tmp/abc' #, options
FileUtils.chown_R  ENV['USER'], 'root', 'tmp/abc' #, options
FileUtils.rm       'tmp/x' #, options
FileUtils.rm_r     'tmp/b' #, options
FileUtils.rm_rf    'tmp' #, options
