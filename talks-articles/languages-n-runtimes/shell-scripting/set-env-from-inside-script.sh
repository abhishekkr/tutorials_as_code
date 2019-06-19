#!/usr/bin/env bash

echo "[+] set environment variable from within a script by running not source-ing"


gdbToSet(){
gdb /proc/${PPID}/exe ${PPID} <<END >/dev/null
call setenv("foo", "bar", 0)
END
}


triggerAChildShellWithEnv(){
  export foo="baaaar"
  exec $SHELL -i
}

barbar(){
  echo "find more"
}


echo "----------------------------------------------------------"
case $1 in
  gdb)
    echo 'try "echo $foo"'
    gdbToSet
    ;;
  child)
    echo 'try "echo $foo"'
    triggerAChildShellWithEnv
    ;;
  b)
    echo 'try "echo $foo"'
    barbar
    ;;
  *)
    echo "if system has gdb, can use it to set... try '$0 gdb', this works with bash not zsh"
    echo "else can spawn a child shell where the new env persists... try '$0 child', this shall work with all"
    ;;
esac
echo "----------------------------------------------------------"
