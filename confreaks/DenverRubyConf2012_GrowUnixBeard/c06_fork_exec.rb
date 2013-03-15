#!/usr/bin/env ruby

pid = fork do
        $stdout.reopen('google.html')

        exec('curl', '-s', 'google.com')
      end
Process.wait(pid)
