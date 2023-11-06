# namespace and paths accordingly are very important
define dockerz::dockerfiles::create {

  file {
    "${path}/${name}":
      ensure => "present",
      mode   =>  0664,
      source => "puppet:///modules/dockerz/files/dockerfiles/${name}",
  }
}
