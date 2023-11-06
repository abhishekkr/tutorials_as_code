#!/usr/bin/env bash

### To Remove: issue_deprecation_warning due to new convention in 3.7 puppet

SCRIPTS_DIR=$(dirname $0)
MANIFESTS_PATH="${SCRIPTS_DIR}/environments/testenv/manifests"

echo "This example involves running via site.pp; module 'dockerZ' uses extlookup, facters and submodules and defined type"

## typical puppet setup's config file will manage environment, modulepath, manifestpath and other stuff
puppet apply --environment=testenv --confdir="${SCRIPTS_DIR}" "${MANIFESTS_PATH}/site.pp"

