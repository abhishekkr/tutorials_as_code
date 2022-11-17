#!/usr/bin/env bash

export MYDIR=$(dirname $0)

pushd ${MYDIR}

set -ex
nim c -o:src/protocol src/protocol.nim
nim c -o:bin/chat src/client.nim
# src/client.nims for compiler flags

nim c -o:bin/chat-server src/server.nim
set +ex

popd
