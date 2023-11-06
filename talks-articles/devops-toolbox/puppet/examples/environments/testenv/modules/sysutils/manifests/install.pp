class sysutils::install {
    package { ['htop', 'strace']: ensure => 'installed', }
}
