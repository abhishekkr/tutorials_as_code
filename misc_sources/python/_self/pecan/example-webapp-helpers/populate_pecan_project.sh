#!/bin/bash

if [[ -d $1 ]]; then
  echo "Pecan Project passed wasn't found at $1" && exit 1
fi

_WEBAPP_PATH=$1
cd $_WEBAPP_PATH


