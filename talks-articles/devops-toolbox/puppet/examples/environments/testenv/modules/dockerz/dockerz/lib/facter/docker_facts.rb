require 'facter'

##
## data from url, system file or any task/computation can be assigned here for facters

facters = {
  :docker_files_facter  => '/etc/httpd/conf.d',
  :random_value         => 'https://dockerfile.github.io/'
}
##
###

facters.each_pair do |key, value|
  Facter.add(key) do
    setcode do
      value
    end
  end
end
