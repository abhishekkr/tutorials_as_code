## Chef Cookbook Design Patterns
> at MWRC 2011, by Joshua Timberman

* no data in recipes just freaking references to attributes

* using 'Chef::Config[:file\_cache\_path]' for anything requiring a temporary store

* [Bash Script] Paths, Versions, Config-Options (anything qualifies as data) needs to be an attribute

* Attributes conditional on Platform

* recipes separated by functionalities

* search attribute usage

```
search(:node, "#{search_string}") do |n|
  required_ipadress << n['ipadress']
end

## now loop over required_ipadress for task, in templates
```

* for masterless mode

```
if Chef::Config[:solo]
  node_list = nodes['some_criteria']['node_list']
else
  node_list = search(:node, "#{search_string}")
end
```

* avoid hard-coding data instead use node-attributes, data-bags, chef-search

* avoid code redundancy

* document required attributes, how managed

* file specificity, organized by platform or other differentiating factor

* Extend via Library, Resources, Proiders, Definitions

* Libraries give Recipe Helpers, LWRP Helpers and HWRP.

> * Recipe Helpers via Library

```Recipe-Helper

## in library
class Chef
  class Recipe
    def broken?
      if ['ubuntu'].incude?(node['platform'])
        true
      end
      false
    end
  end
end

## in recipe
if not broken?
  package 'apache'
end

```

> * Libraries extending LWRP to DRY API calls

```LWRP-Helper

## in library
module Opscode
  module Aws
    module Ec2
      def ec2
        @@ec2 ||= RightAws::EC2.new(
          new_resource.aws_access_key,
          new_resource.aws_secret_access_key,
          { :logger => Chef::Log }
        )
      end
    end
  end
end


## in provider
include Opscode::Aws::Ec2
...
  ec2.describe_addresses.find{|a| a[:public_ip] == ip}
...
```

#### HWRP.

> Full classes in Chef itself, allows behaviors not available in LWRPs.
> Inherit/Extend existing Resource/Provider

* Resource contain action and attributes
> attibutes' validation parameters are
> :default - default value
> :kind\_of - value must be kind\_of?(Klass)
> :required - exception raised if not provided
> :regex - value shall match
> :equal\_to - value must equate
> :name\_attribute - set name of resource to infer
> :callbacks - hash of procs, should return true
> :respond\_to - ensure value has given method

example resource

```
actions :create

attrbite :mypath, :kind_of => String
```

* Resouces need Providers
> DSL defined action methods
> Chef Recipe DSL is extended

example provider

```
### use more ruby here, try make idempotent
action :create do
  unless ::File.exists(mypath)
    Chef::Log.info "Creating mypath at #{new_resource.mypath}"
    file "#{new_resource.mypath}" do
      content "Yes This Is MyPath"
    end
    new_resource.updated_by_last_action(true)
  end
end
```

#### Cookbook Metadata

```
maintainer ''
maintainer_email ''
license ''
description ''
long_description "all libraries, resources, recipes, external attributes"
version ''

depends ''
```

---
---
