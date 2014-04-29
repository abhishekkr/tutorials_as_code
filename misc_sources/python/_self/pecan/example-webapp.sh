#!/bin/bash

_EX_PWD=$( dirname $0 )

WEBAPP_NAME="example-webapp"
WEBAPP_PATH="${_EX_PWD}/${WEBAPP_NAME}"

HELPERS_PATH="${_EX_PWD}/${WEBAPP_NAME}-helpers"

if [[ $1 == "-new" ]]; then

  rm -rf $WEBAPP_NAME

  pecan create $WEBAPP_NAME
  cd $WEBAPP_PATH


  exit
elif [[ $1 == "-devegg" ]]; then

  [ ! -d $WEBAPP_PATH ] && eval "cd $_EX_PWD ; $0 -new"
  cd $WEBAPP_PATH
  python setup.py develop


  exit
elif [[ $1 == "-test" ]]; then

  [ ! -d $WEBAPP_PATH ] && eval "cd $_EX_PWD ; $0 -new"
  cd $WEBAPP_PATH
  python setup.py test -q


  exit
elif [[ $1 == "-populate" ]]; then

  [ ! -d $WEBAPP_PATH ] && eval "cd $_EX_PWD ; $0 -new"
  eval "${HELPERS_PATH}/populate_pecan_project.sh $WEBAPP_PATH"

  exit
elif [[ $1 == "-serve" ]]; then

  [ ! -d $WEBAPP_PATH ] && eval "cd $_EX_PWD ; $0 -new"
  cd $WEBAPP_PATH
  pecan serve config.py


  exit
fi

echo "Pecan Tryout options:"
echo "-new   :: create a fresh pecan project"
echo "-serve :: serve the trial pecan project"
echo "-test  :: run tests in trial pecan project"
