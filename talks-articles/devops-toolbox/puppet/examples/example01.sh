#!/usr/bin/env bash

MODULE_PATH=$(dirname $0)"/environments/testenv/modules"

echo "Running a simple module: sysutils"

## typical puppet setup's config file will manage environment, modulepath, manifestpath and other stuff
puppet apply --modulepath="$MODULE_PATH" -e "include sysutils"
