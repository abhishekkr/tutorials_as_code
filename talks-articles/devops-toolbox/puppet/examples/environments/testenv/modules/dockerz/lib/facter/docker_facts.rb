require 'facter'

### can put any Ruby logic here to populate Facter Datasets

##
## data from url, system file or any task/computation can be assigned here for facters

facters = {
  :docker_files_facter  => 'Dockerfile.puppet;Dockerfile.ansible', # as Facter values pass as string so setting array doesn't work
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
