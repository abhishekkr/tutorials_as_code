
> Puppet beginners concept guide (Part 2)

## Chapter.2 The Road To Modules

### Puppet Modules?
####  (no, no..... nothing different conceptually)

Puppet Modules (like in most other technological references, and the previous part of this tutorial)  are libraries to be loaded and shared as per the required set of configuration.

Think if you have a war application to be deployed over tomcat. For the stated requirement you require tomcat to be present on the machine with correct required configurations and war file to be correctly downloaded and placed on the machine with correct permissions.
In a general scenario requirement like this, two modules come up. One to install, configure and service-start tomcat service. Another to download/locate war file, use tomcat's configure and service sub-module.

### Logic of Structure Logic
#### (just how is your module structured and )

The different components of structural design followed by each puppet module:

>
> * manifests
>
> All your '&lt;module/submodule&gt;.pp' manifest files go into '&lt;module_dir&gt;/manifests'.
> Puppet has an auto-load service for modules/sub-modules, so the naming of these *.pp files should be suiting the class names.
> As discussed above for a 'tomcat' module, you are also gonna create sub-modules like 'tomcat::install', 'tomcat::configure', and 'tomcat::service'.
> So the files that will get create be '&lt;tomcat-module&gt;/manifests/install.pp', '&lt;tomcat-module&gt;/manifests/configure.pp',  '&lt;tomcat-module&gt;/manifests/service.pp'.
> Now if there would have been a sub-module like 'tomcat::configure::war',  then the file-path would go like '&lt;tomcat-module&gt;/manifests/configure/war.pp'.
>

>
> * templates
>
> As any other language, where you want some static data merged with varying passed-on or environment  variables and pushed in somewhere as content. Say, for 'tomcat::config' sub-module as you wanna parameter-ize somethings like 'war' file name. Then this war file-name is being passed-on by 'deploy_war' module.
> This ruby template goes in '&lt;tomcat-module&gt;/files/war_app.conf.erb' and whenever required it's content received as "template('&lt;tomcat-module&gt;/war_app.conf.erb')" files.
> Any kin'of static file can be served from a module using puppet's fileserver mount points. Every puppet module has a default file-server mount location at '&lt;tomcat-module&gt;/files'.
> So a file like '&lt;tomcat-module&gt;/files/web.war' get to be served at Puppet Agents pointing to source of 'puppet:///modules/&lt;tomcat-module&gt;/web.war'.
>

>
> * lib
>
> This is the place where you can plug-in your custom mods to puppet and use your newly powered up puppet features.
> This is the one feature that lets you actually utilize your ruby-power and add-on custom facts, providers & types (with default location at '&lt;tomcat-module&gt; /lib/ &lt;facter|puppet&gt;', '&lt;tomcat-module&gt; /lib/puppet/ &lt;parser|provider|type&gt;') to be used via puppet in your modules. To be used it requires 'pluginsync = true' configuration to be present at 'puppet.conf' level.
> We'll discuss this in more detail with all sorts of examples in following up blogs and add the links here. Until then it can be referred at [docs.puppetlabs.com](http://docs.puppetlabs.com/guides/plugins_in_modules.html).
>

>
> * spec/tests
>
> As Love needs Money to avoid worldly issues affect its charm. Similarly, Code need Tests.  In location '&lt;tomcat-module&gt;/spec/' you can have your [puppet-rspec](http://rspec-puppet.com/) [tests](http://bombasticmonkey.com/2012/03/02/automatically-test-your-puppet-modules-with-travis-ci/) for puppet module.
> The path '&lt;tomcat-module&gt;/tests/' would have common examples on how the module classes would be defined.
>


### Modules Fundamental Live
#### (mean the actual code sample.....)

```Ruby
# <tomcat-module>/manifests/install.pp

class <tomcat-module>::install {
  package { 'tomcat6': ensure => 'installed', }
}
```

```Ruby
# <tomcat-module>/manifests/configure.pp

class <tomcat-module>::configure {
  <tomcat-module>::configure::war {
    ['web']:
      require  => Package['tomcat6'],
  }
}
```

```Ruby
# <tomcat-module>/manifests/configure/war.pp

class <tomcat-module>::configure::war {
  file {
    "/var/lib/tomcat6/webapps/${name}.war":
      content => template('<tomcat-module>/war_app.conf.erb'),
      owner    => tomcat,
      group     => tomcat,
      mode     => '0644',
  }
  file {
    "/etc/${name}.conf":
      content => template('<tomcat-module>/war_app.conf.erb'),
      owner    => tomcat,
      group     => tomcat,
      mode     => '0644',
      require  => File["/var/lib/tomcat6/webapps/${name}.war"],
      notify     => Service['tomcat6'],
  }
}
```

```Ruby
# <tomcat-module>/templates/war_app.conf.erb

com.<%= name %>.adept.serviceURL=http://<%= fqdn %>:8080/<%= name %>
com.<%= name %>.adept.log.file=/var/log/tomcat6/<%= name %>.log
com.<%= name %>.adept.persist.sql.driverClass=com.mysql.jdbc.Driver
com.<%= name %>.adept.persist.sql.connection=jdbc:mysql://127.0.0.1:3306/adept
com.<%= name %>.adept.persist.sql.dialect=mysql
com.<%= name %>.adept.persist.sql.user=<%= mysql_user%>
com.<%= name %>.adept.persist.sql.password=<%= mysql_password%>
```

```Ruby
# '<tomcat-module>/files/web.war'

~ the war-packaged file for web-application
```

```Ruby
# <tomcat-module>/lib/facter/database.rb

require 'facter'
require 'yaml'
facts_file = '/etc/puppet/myfacts.yaml'
facts = {}
facts.merge YAML.load_file(facts_file) if File.exist? facts_file
Facter.add("mysql_user") do
  setcode do
    return 'mysql-password' if facts['mysql'].nil? || facts['mysql']['user'].nil?
    facts['mysql']['user']
  end
end
Facter.add("mysql_password") do
  setcode do
    return 'mysql-password' if facts['mysql'].nil? || facts['mysql']['password'].nil?
    facts['mysql']['password']
  end
end
```

```Ruby
# <tomcat-module>/spec/classes/<tomcat-module>/install_spec.pp

require 'spec_helper'
describe 'tomcat::install' do
  it do
    should contain_package('tomcat6').with_ensure('latest')
  end
end
```

```Ruby
#<tomcat-module>/tests/

node just-tomcat6 {
  include <tomcat-module>::install
}
```

[Puppet Module Code Sample](https://gist.github.com/abhishekkr/3070758)

---
