#!/usr/bin/env bash

script_name=$( basename ${0#-} ) #- needed if sourced no path

[[ ! -z "${BASH_SOURCE}" ]] && this_script=$( basename ${BASH_SOURCE} )

if [[ ${script_name} = ${this_script} ]] ; then
    echo "running me directly"
else
    echo "sourced from ${script_name}"
fi
