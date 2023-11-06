class dockerz::dockerfiles {

  # example pf fetching from local External Data (w/o Hiera)
  $docker_dir   = extlookup('docker_dir')
  $docker_files = split($::docker_files_facter,';')

  file {
    "${docker_dir}":
      ensure => "directory",
  }

  # example of usage of define() : function like re-usable components
  dockerz::dockerfiles::create {
    # example of using facter (here Facter is provided by module itself, can be different)
    $docker_files:
      path => $docker_dir,
  }

  # for more ways of handling data : https://github.com/abhishekkr/eden_guide_to_puppet/blob/master/chapters/Puppet.Part4.md
}
