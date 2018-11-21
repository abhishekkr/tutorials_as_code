#!/usr/bin/env bash

# `&` and `wait`
### have been used to background process to be disconnect from current flow
### and wait for all of them to finish

inner(){
  local aid="$1"
  { sleep 5 ; echo ${aid}1-5; } &
  { sleep 3 ; echo ${aid}1-3; } &
  { sleep 15 ; echo ${aid}1-51; } &
  wait
  { sleep 5 ; echo ${aid}2-5; } &
  { sleep 3 ; echo ${aid}2-3; } &
  { sleep 15 ; echo ${aid}2-15; } &
  wait
}

outer(){
  for xid in a b c d e; do
    inner $xid &
  done
  wait
}

outer
