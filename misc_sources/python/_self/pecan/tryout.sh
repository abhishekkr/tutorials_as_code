#!/bin/bash

_EX_PWD=$PWD
WEBAPP_NAME="example-webapp"
WEBAPP_PATH="${_EX_PWD}/${WEBAPP_NAME}"

rm -rf $WEBAPP_NAME

pecan create $WEBAPP_NAME
cd $WEBAPP_PATH

python setup.py develop

pecan serve config.py
