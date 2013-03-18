#!ruby

spawn({'A_VAR' => 'ABC'}, 'echo $A_VAR') # set an Env var
spawn({'A_VAR' => nil}, 'echo $A_VAR') # clear an Env var
#spawn(env, 'echo $HOME', unsetenv_others: true) # clear unset Env var

spawn("echo 00*.rb") # normal shell expansion
spawn("echo", "*.rb") # bypass shell, no expansion
spawn(["echo", "shout"], "*.rb") # rename command in process list

spawn('ls', pgroup: 0) # change the Process group
spawn('ls', rlimit_cpu: Process.getrlimit(:CPU).last) # raise res limit
spawn('ls', chdir: '/tmp') # change working dir
spawn('ls', umask: 0222) # change permission

spawn('ls', in: io) # redirect io
spawn('ls', io => [open, args])
spawn('echo $HOME', io => :close) # close an IO
spawn('w', close_others: true) # close unset IO

pid = spawn('who'); Process.detach(pid) # async
pid = spawn('whoami'); Process.wait(pid) # async
