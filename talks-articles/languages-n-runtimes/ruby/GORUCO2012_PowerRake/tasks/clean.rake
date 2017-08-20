#clean up library
require 'rake/clean'

# rake clean
CLEAN.include(THUMBS, "thumbs") # clean given filelist

# rake clobber
CLOBBER.include("final.png") # also cleans dependents
