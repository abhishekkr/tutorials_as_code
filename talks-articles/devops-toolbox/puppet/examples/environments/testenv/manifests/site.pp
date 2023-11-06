# example

$extlookup_datadir     = "${settings::manifestdir}/extdatadir"
$extlookup_precedence  = ["fqdn_%{fqdn}", "env_%{environment}", "domain_%{domain}", "common"]
 
include sysutils::install
include dockerZ
