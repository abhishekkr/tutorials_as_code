
> Puppet beginners concept guide (Part 3)

## Chapter.3 Modules with More

Here some time on the practices to prefer while writing most of your modules

### HowTo Write Good Puppet Modules
#### (so everyone could use it and you could use it everywhere)

* *platform-agnostic*

With change in Operating System distro; module also might require difference in package names, configuration file locations, device port names, system commands and more.
Obviously, it's not expected to test each and every module against each and every distro and get it full-proof for community usage. But what's expected is to use case $operatingsystem{...} statements for whatever distros you can and let the users get notified in case they gotta add something for their distro by fail(""), and might also contribute back..... like following

```JSON
case $operatingsystem {
  centos, redhat: {
    $libxml2_development = 'libxml2-devel'
  }
  ubntu, debian: {
    $libxml2_development = 'libxml2-dev'
  }
  default: {
    fail("Unrecognized libxml2 development header package name for your O.S. $operatingsystem")
  }
}
```

* *untangled puppet strings*

You are writing puppet modules. Good chance is you have a client or personal environment to manage for which you had a go at it.
That means there gonna be your environment specific some client or personal code &/or configuration that is 'for your eyes only'. This will prohibit you from placing any of your module in Community.
It's wrong on two main fronts. First, you'll end up using so much from OpenSource and give back nothing. Second, your modules will miss on the community update/comment action.
So, untangle all your modules into atomic service level modules. Further modularize those modules into service puppet-ization requirement. That will be like sub-modules for install, configure, service and whatever more you can extract out. Now these sub-modules can be clubbed together to and we can move bottom-up gradually.
Now you can just keep your private service modules to yourself, go ahead and use the community trusted and available modules for whatever you can..... try  making minor updates to those and also contribute these updates. Write the others that you don't find out in the wild and contribute those too for community to use, update and improve.


* *no data in c~o~d~e*

Now when you are delivering 'configuration as a code', adapt the good coding practices applicable in this domain. One of those is keeping data separate than the code, as in no db-name, db-user-name, db-password, etc. details stored directly in the module's manifest intending to create the db-config file.
There will be a detailed section later over different external data usage involving separate [parameter manifest](http://docs.puppetlabs.com/guides/parameterized_classes.html#appendix-smart-parameter-defaults) setting up values when included, [extlookup](http://docs.puppetlabs.com/references/2.6.8/function.html#extlookup) loading values from CSVs, [puppetDB](http://puppetlabs.com/blog/introducing-puppetdb-put-your-data-to-work/), [hiera data-store](http://projects.puppetlabs.com/projects/hiera/) and [custom facts](http://docs.puppetlabs.com/guides/custom_facts.html) file to load up key-values.


* *puppet-lint*

To keep the modules adhere to dsl-syntactic correct and beautiful code writing practice. So the DSL and the community contributors, both find it easy to understand your manifests. It's suggested to have it added to rake default of your project to check all the manifests, ran before every repo-check-in.


* *do-undo-redo*

It's suggested to have a undo-manifest ready for all the changes made by a module. It mainly comes in handy for infrastructures/situations where creating and destroying a node is not under your administrative tasks or consumes hell lot of time.
Obviously, in case getting new node is easier..... that's the way to go instead of wasting time in undo-ing all the changes (and also relying on that).


_Those are just there for the dry-days when there is no 'cloud'._

### More about Modules
#### (moreover..... how to get more)

Where to get new: [http://forge.puppetlabs.com/](http://forge.puppetlabs.com/) is the community-popular home for most of the Puppet Modules.

Where to contribute:

* Can manage your public module at [GitHub](https://github.com/) or similar online free repository like [puppetlabs-kvm](https://github.com/puppetlabs/puppetlabs-kvm).
* Then you can push your module to [forge.puppetlabs.com](http://docs.puppetlabs.com/puppet/2.7/reference/modules_publishing.html).

---
