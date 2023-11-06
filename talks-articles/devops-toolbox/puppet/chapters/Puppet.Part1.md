
> Puppet beginners concept guide (Part 1)

## Chapter.1 Kickstart

### What it is? When is it required?
#### (for all new guys, who came here while just browsing internet)

>
> Puppet is an OpenSource automated configuration management framework (which means a tool that knows how to configure all machines to a deterministic state once you provide it the required set of manifests pulling the correct strings).
> It's managed at enterprise level by an organization called [PuppetLabs](http://puppetlabs.com/) (http://puppetlabs.com/).
>

It is required:

* when you have a hell-lot of machines required to be configured in a similar form.

* when you have a infrastructure requirement of dynamic scale-up and scale-down of machines with a pre-determined (or at least metadata-calculated) configuration.

* to have a control over all set of configured machines, so a centralized (master-server or repo-based) change gets propagated to all automatically.


And more scenarios come up as you require it.

---


### Quickie.

>
> Install [Ruby](http://www.ruby-lang.org/en/downloads/), [Rubygems](http://rubygems.org/) on your machine where you aim to test it.
>

```Shell
gem install puppet --no-ri --no-rdoc
```

Download installers @[Windows](http://docs.puppetlabs.com/windows/installing.html)  @[MacOSX](http://downloads.puppetlabs.com/mac/) ::&:: [Docs to installing](http://docs.puppetlabs.com/guides/installation.html).

Checking, if it's installed properly and acting good
Now, 'puppet --version' shall give you the version of installed puppet once succeed.
Executing 'facter' and you shall get a list of System Environment related major information.

Have a quick puppet run, this shall create a directory '/tmp/pup' if absent. Creates a file '/tmp/pup/et' with 'look at me' as its content.
{In case of trying out on platforms without '/tmp' location. Like for Windows, change '/tmp' with 'C:/' or so}


```Shell
puppet apply -e "file{'/tmp/pup':
                               ensure => 'directory'}
                             file{ '/tmp/pup/et':
                               ensure => 'present',
                               content => 'look at me',
                               require => File['/tmp/pup']}
                           "
```

---


### Dumb usage structure.

Create huge manifest for your node with all resources & data mentioned in it. Then directly apply that manifest file instead of '-e "abc{.....xyz}"'.

Say if the example above is your entire huge configuration commandment for the node. Copy all that to a file say 'mynode.pp'.
Then apply it similarly like


```Shell
puppet apply mynode.pp
```

---


### How it evolves?

Now, as any application had pluggable library components to be loaded and shared as and when required. Puppet too have a concept of modules. These modules can have manifests, files-serving and more.

Modules can be created in any design preference. Normally it works by having different modules per system component. To entertain different logical configuration states for any given system component (and also keeping it clean) further re-factoring can be done in the modules' manifest dividing it into different scopes.

Taking example of a module for 'apache httpd'.
For a very basic library, you might wanna structure your module like:

* a directory base for your module:  &lt;MODULE_PATH&gt;/httpd/
* a directory in module to serve static files:   &lt;MODULE_PATH&gt;/httpd/files
* static configuration file for httpd:   &lt;MODULE_PATH&gt;/httpd/files/myhttpd.conf
* AccessFileName .acl
* directory to hold your manifests in module:   &lt;MODULE_PATH&gt;/httpd/manifests/
* a complete solution manifest:   &lt;MODULE_PATH&gt;/httpd/manifests/init.pp

```Ruby
class httpd{
  include httpd::install
  include httpd::config
  include httpd::service
}
```


a manifest just installing httpd:    &lt;MODULE_PATH&gt;/httpd/manifests/install.pp
```Ruby
class httpd::install {
  package {'httpd': ensure => 'installed'}
}
```

a manifest just configuring httpd:    &lt;MODULE_PATH&gt;/httpd/manifests/config.pp
```Ruby
class httpd::config{
  file {'/etc/httpd/conf.d/httpd.conf':
    ensure => 'present',
    source => 'puppet:///modules/httpd/myhttpd.conf'
  }
}
```


a manifest just handling httpd service:  &lt;MODULE_PATH&gt;/httpd/manifests/service.pp
```Ruby
class httpd::service{
  service{'httpd': ensure => 'running' }
}
```


Now, using it

```Shell
puppet apply --modulepath=<MODULE_PATH>  -e "include httpd"
```

would install, custom-configure and start the httpd service.


```Shell
puppet apply --modulepath=<MODULE_PATH>  -e "include httpd::install"
```

would just install the httpd service.

---
