#!ruby
# 1.9
# spawn it

pid = spawn({ "HOME" => "/root" }, #env
            'ls', #cmd
            '-l' ) # options
Process.wait(pid)
