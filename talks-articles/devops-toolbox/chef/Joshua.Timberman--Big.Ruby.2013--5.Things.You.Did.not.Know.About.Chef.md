## 5 Things You Didn't Know About Chef
> at Big Ruby 2013, by Joshua Timberman

#### Changing a single line in a file

* managing whole file resources (template, cookbook\_file, remote\_file)

* to edit single line, 'ruby\_block' resource (not preferred generally if do-able via noun-ish resources)

```
ruby_block "edit /etc/hosts" do
  block do
    rc = Chef::Util::FileEdit.new("/etc/hosts")
    rc.search_file_replace_line(
      /^127\.0\.0\.1 localhost$/,
      "127.0.0.1 #{new_fqdn} #{new_hostname} locahost"
    )
    rc.write_file
  end
end
```

* 'Chef::Util::FileEdit' got multiple more helpful methods, some are
> search\_file\_replace\_line(regex, newline)  : if matched, replace whole line with newline
> search\_file\_replace(regex, replace)       : if matched, replace all occurences
> search\_file\_delete\_line(regex)            : if matched, remove line
> search\_file\_delete(regex)                 : if matched, delete all occurences
> insert\_line\_after\_match(regex, newline)   : if matched, insert newline after matching line
> insert\_line\_if\_no\_match(regex, newline)   : if not matched, insert newline at end of file
> write\_file                                : write the new file content, flush

* cookbook "line", resource provider for managing single lines; 'http://ckbk.it/line'

---

#### Chef's built-in version matching

> Platforms, Cookbooks, Software/packages

Match versions that are dotted numeric notations upto 3 places.
Provides first-class version matching, allows X.Y.Z version string.
* [Documentation on Cookbook Version](http://docs.opscode.com/essentials_cookbook_versions_constraints.html)

* How to use it
> require 'chef/version\_contraint' library
> initialize a Chef::VersionContraint object with the constraint
> send 'include?' method with version to match

* Constraint Operator
```
=   : equal
>   : greater than
<   : less than
>=  : greater than or equal
<=  : less than or equal
~>  : approximately greater than (pessimistic)
```

* Some code

```
require 'chef/version_constraint'

required_version = Chef::VersionContraint.new(">= 10.7.0")
required_version.include?("10.6.0") ## false
required_version.include?("10.7.3") ## true
required_version.include?(node['platform_version'])
```

---

#### REPL for Chef

* 'shef' till Chef.10; 'chef-shell' from Chef.11

* built on IRB; 3 modes/contexts
> main: access node attributes, interact with chef-server, do IRB stuff
> attributes: context of Cookbooks' attributes file, follow same order
> recipe: context of recipe where all resources, DSL methods are available

```
## lot of commands, many available work with chef-server API
$ chef-shell
chef> help
... 

## loading config from a client for a specific chef-server
$ chef-shell -z
chef> Chef::Config[:chef_server_url]
...
chef> node.run_list
...

chef> attributes_mode
chef:attributes> default['shef'] = 'Fun'
chef:attributes> node.default['shef'] = 'Woo'  ## in older version can't just say node['']

chef> recipe_mode
chef> package "vim"
... ## return resource object
chef> package "git"
...
chef> resources
... ## returns list of resources
chef> run_chef
... ## applies resource
chef> search(:node, "role:workstation")
... can search like from server in recipe
chef> data_bag_item(:users, "jtimberman")
... access data-bag-items similarly

$ pry
(main):0 > require 'chef'
(main):0 > require 'chef/shell/ext'
(main):0 > Shell::Extensions.extend_context_object(self)
(main):0 > Chef::Config.from_file("/etc/chef/client.rb")
(main):0 > node.all
... listing all nodes

## can use this similarly from knife-plugins, libraries, applications

```

---

#### The Resource Collection

* Chef loads recipes looking for resources and adds them to ResourceCollection, then walks through it taking actions.

example recipe

```
package 'apache2'

template '/etc/apache2/apache2.conf' do
  mode 0644
  owner 'www-data'
  group 'www-data'
  notifies :restart, 'service[apache2]'
end

service 'apache2' do
  supports :reload => true, :status => true
  action [:enable, :start]
end
```

paste recipe in chef-shell then use the resources()

```
chef:recipe> resources
["package[apache2]", "template[/etc/apache2/apache2.conf]", "service[apache2]"]
```

* run\_context.resource\_collection
> * the run\_context loads and tracks context of Chef run
> * resource collection is part of run context
> * resource collection can be accessed directly with 'run\_context.resource\_collection'

```
chef:recipe> run_context.resource_collection
## here 'resource_by_name' is hash of all resources
## they have numeric index, the order in which action gets applied

chef:recipe> run_context.resource_collection.each do |r|
               puts "#{r} is a #{r.resource_name}"
             end
...

```
---

#### Anatomy of loading and execuing a recipe

* Chef.11 onwards have 'chef-apply', started from [gist](https://gist.github.com/danielsdeleo/2920702).

```
require 'chef/client'
require 'chef/providers'
require 'chef/resources'

Chef::Config[:solo] = true
client = Chef::Client.new

client.node = Chef::Node.build("apply-node")
client.run_ohai
client.build_node

run_context = Chef::RunContext.new(
      client.node, {}, client.events
    )

recipe = Chef::Recipe.new(
      "(chef-apply cookbook)",
      "(chef-apply recipe)",
      run_context
    )
recipe.from_file(recipe_path)

runner = Chef::Runner.new(run_context)
runner.converge
```

* **NEVER DO THIS**, but 'chef-apply' takes STDIN

start chef-apply over a listener

```
while true
do
nc -l 8787 | sudo chef-apply -s
done
```

then from anywhere via telnet, run insecure $#!t

```
telnet $IP_OF_NC8787 8787
package "git"
^]
^D
```

---

#### FAQ:Testing

* Chef-10.14 onwards 'why-run' mode is dry-run
* Test-Kitchen
* ChefSpec

---
---
