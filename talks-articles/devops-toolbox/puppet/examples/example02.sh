#!/usr/bin/env bash

MANIFESTS_PATH=$(dirname $0)"/environments/testenv/manifests"
MODULES_PATH=$(dirname $0)"/environments/testenv/modules"

echo "This example involves running via site.pp; module 'dockerZ' uses extlookup, facters, submodules and defined type"

## typical puppet setup's config file will manage environment, modulepath, manifestpath and other stuff
## Puppet 3.7 will fail with even manifestdir and modulepath specified due to: http://docs.puppetlabs.com/puppet/latest/reference/environments_classic.html#referencing-the-environment-in-manifests
puppet apply --environment=testenvX --modulepath="${MODULES_PATH}" --manifestdir="${MANIFESTS_PATH}" "${MANIFESTS_PATH}/site.pp"

