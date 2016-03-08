> earlier version affinity

# Basics

### Managed Chef by Chef.io

Can use it by an account at [manage.chef.io](https://manage.chef.io/login). To get an account can [sign-up](https://manage.chef.io/signup) here.

Create an Organization, create and download "$ORG-validator.pem" key.
Add/invite User, create and download "$USER.pem" key.

---

### Knife

The orchestration utility provided.

> By default tried to pick configuration from '/etc/chef'

> to check if 'knife' is available, it should show Chef version
```
knife -v
```

* validator.pem validates you as member of group
* <User>.pem authenticates to Chef-Server for tasks

> First time Chef runs on a node it looks for a 'client.pem', when note found it retrieves one using 'validator.pem'. Henceforth doesn't use 'validator.pem'.

* Once knife configured, node created.

```
knife client list
knife node list
knife node create $SAMPLE_NODE_NAME
knife node edit $SAMPLE_NODE_NAME
knife node show $SAMPLE_NODE_NAME

## to manage node definitions under SCM
knife-abk node show "$SAMPLE_NODE_NAME" -Fj | tee "nodes/${SAMPLE_NODE_NAME}.json"

## creating/updating nodes with their definition
knife-abk node from file "nodes/${SAMPLE_NODE_NAME}.json"

## if auth issues happen and need to reregister, this will get you new validator/client.pem
knife client reregister

## to remove
knife client delete $SAMPLE_NODE_NAME
```

---

### Vagrant to manage local test 'nodes'

> Chef created Boxes can be found at [Atlas/Bento](https://atlas.hashicorp.com/bento/).
> More info at [Bento](https://github.com/chef/bento/blob/master/README.md).

```
vagrant init
## update initialized Vagrantfile
vagrant up
## vagrant provision
## vagrant reload
```

Following config lines updated to Vagrantfile will make sure you use a chef ready box and provision it with right 
chef configuration.

```
Vagrant::Config.run do |config|
  config.vm.box = "#{VAGRANT_BENTO_BOX_NAME}"
  config.vm.box_url = "#{URI_TO_VAGRANT_BENTO_BOX}"
  config.vm.forward_port 80, 8088

  config.vm.provision :chef_client do |chef|
    chef.chef_server_url = "#{CHEF_SERVER_URL}"
    chef.validation_key_path = "#{VALIDATOR_PEM}"
    chef.validation_client_name = "#{ORGNAME}-validator" ## self-hosted-default: chef-validator
    chef.node_name = "#{SAMPLE_NODE_NAME}"
  end
end
```
---

### Working with cookbooks

> for managing dependents of cookbook, use [Berkshelf](https://berkshelf.com)

#### update chef on node with chef

* need to get required 'cookbook'

```
## download the cookbook from supermarket
knife cookbook site download omnibus_updater
## 'knife cookbook site [..]' takes it to supermarket

## extract it into COOKBOOKS_DIR and remove waste
tar zxf omnibus_updater-*.tar.gz -C $COOKBOOKS_DIR
rm omnibus_updater-*.tar.gz

## upload cookbook to your Organization on hosted Chef
knife cookbook upload omnibus_updater
## to upload all cookbooks under 'cookbook' dir
## knife cookbook upload -a
```

* now need to add it to 'run\_list', can do it via Server's web-ui, but better

```
knife node edit $SAMPLE_NODE_NAME

## this opens up JSON config file for node's state
## this need to have a "run_list" attribute as level-1-branch as a list
## add element of recipe to be applie here as 'recipe[omnibus_updater]'
## above will refer to 'default' recipe
```

* run chef-client on node again

```
## with vagrant example
## vagrant provision
```

#### custom cookbook

* let's try create a custom cookbook

```
knife cookbook create $SAMPLE_COOKBOOK
## to create cookbook at current dir not $COOKBOOKS_DIR, following
## knife cookbook create camlistore -o .
mkdir $COOKBOOKS_DIR/$SAMPLE_COOKBOOK/spec  ## for chef-spec tests
```

this will generate a structure at "$COOKBOOKS\_DIR/camlistore" something like
```
$ tree .
.
├── attributes
├── CHANGELOG.md
├── definitions
├── files
│   └── default
├── libraries
├── metadata.rb
├── providers
├── README.md
├── recipes
│   └── default.rb
├── resources
└── templates
    └── default

10 directories, 4 files
```

* now with better workflow available and best practices

```
## now can use ```chef cookbook generate $SAMPLE_COOKBOOK``` to create at current dir
## this creates minimal best practice set
```

this has following structure
```
$ tree .
.
├── Berksfile
├── Berksfile.lock
├── chefignore
├── metadata.rb
├── README.md
├── recipes
│   └── default.rb
├── spec
│   ├── spec_helper.rb
│   └── unit
│       └── recipes
│           └── default_spec.rb
└── test
    └── integration
        ├── default
        │   └── serverspec
        │       └── default_spec.rb
        └── helpers
            └── serverspec
                └── spec_helper.rb
```

---

### Roles (and Cookbook as Role)

> Can contain a set of 'attributes' and 'run\_list' to be applied and maintained as abstracted entity for any 'node' that wanna have that 'role'.

```
## to create a role at chef-server
knife role create $SAMPLE_ROLE_NAME

knife role show $SAMPLE_ROLE_NAME
## to get role into local for versioning
knife role show $SAMPLE_ROLE_NAME -Fj | tee "roles/${SAMPLE_ROLE_NAME}.json"

## updating a role at chef-server from local
knife role from file "roles/${ROLE_CREATED_LOCALLY}.json"

knife role list
```

> Using 'Cookbook' as a single collection of all properties otherwise maintained in 'Role', allows you to use entire dev-workflow around cookbook for this abstraction.

---
---
