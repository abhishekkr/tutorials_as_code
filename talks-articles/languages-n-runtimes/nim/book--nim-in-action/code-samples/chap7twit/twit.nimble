# Package

version       = "0.1.0"
author        = "AbhishekKr"
description   = "an app to broadcast your voice and listen to others"
license       = "MIT"
srcDir        = "src"
bin           = @["twit"]
skipExt       = @["nim"]

# Dependencies

requires "nim >= 1.6.8",
         "jester >= 0.5.0",
         "uuid4 >= 0.9.3"
