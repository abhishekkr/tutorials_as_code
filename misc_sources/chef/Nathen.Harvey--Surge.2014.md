# Surge-2014 Introduction to Chef

## Chef-DK

#### chef-apply

* providing details directly at command line

```
sudo chef-apply -e "package 'vim'"
```

* providing details via a recipe file

```
cat > emacs.rb <<EOF
editors=["vim", "emacs"]

package editors
EOF

sudo chef-apply emacs.rb
```

* recipe file with specific action

```
cat > emacs.rb <<EOF
editors=["vim", "emacs"]

package editors do
  action :upgrade
end
EOF

sudo chef-apply emacs.rb
```

---

## Constructs

#### Recipes
> collection of resources

* example for setting up maah webserver

```
cat > apache.rb <<EOF
package "httpd"

service "httpd" do
  action :start
end

httpd_index = "/var/www/html/index.html"

file httpd_index do
  content "<h1>cfgmgmt: chef</h1>/n"
end
EOF

sudo chef-apply apache.rb
```

* Separaing data from policy
> storing the home-page can be separated using cookbook\_file, remote\_file, template

**chef-apply is not capable for loading templates, need to graduate to chef-client and cookbook**

#### Cookbook

contain
* name, version
* recipes, files, templates, data-attributes, libraries, etc.

chef-repo as independent software projects, dependencies managed as separate artefacts.

* chef generator allows creating lots of boilerplate code to start quickly

```
chef generate --help
chef generate repo --help
chef generate cookbook --help
```

* with cookbook

```
## -p means policy only, independent repo per cookbook
## adds './cookbooks' to be git-ignored
chef generate repo some-repo -p

cd some-repo
git status -b

cd cookbooks
chef generate cookbook apache

cd apache && git init && git add . && git commit -m "started apache cookbook"

##
# can generate template using command
# $ chef generate template $APACHE_COOKBOOK_PATH index.html -s /var/www/html/index.html
##
mkdir -p templates/default
cat > templates/default/index.html.erb <<ERBEOF
<h1>cfgmgmt: chef</h1>/n
<p>
  This is a <%= node["platform"] %> <%= node["platform_version"] %> server
  with <%= node["memory"]["total"] %> RAM.
</p>
<p>
  <% if node["platform"] == "centos" %>
  Oh, hey CentOS!
  <% else %>
  Ah, it's not CentOS!
  <% end %>
</p>
ERBEOF

cat > recipes/default.rb <<EOF
package "httpd"

service "httpd" do
  action :start
end

httpd_index = "/var/www/html/index.html"

template httpd_index do
  source "index.html.erb"
end
EOF

git add . && git commit -m "template added for apache and default recipe updated"
```

#### chef-zero (local node)

* applying the apache cookbok from before

```
chef-client -z -r "recipe[apache]"
```

#### ohai

> provides attributes for node-objects, may include custom attributes

* generate attribute

```
chef generate attribute $APACHE_COOKBOOK_PATH default
## creates $APACHE_COOKBOOK_PATH/attributes/default.rb
cat > $APACHE_COOKBOOK_PATH/attributes/default.rb <<ATTREOF
default['apache']['greeting'] = 'wassup'
ATTREOF

cat > $APACHE_COOKBOOK_PATH/templates/default/index.html.erb <<ERBEOF
<p>>%= node['apache']['greeting'] %></p>
<h1>cfgmgmt: chef</h1>/n
<p>
  This is a <%= node["platform"] %> <%= node["platform_version"] %> server
  with <%= node["memory"]["total"] %> RAM.
</p>
<p>
  <% if node["platform"] == "centos" %>
  Oh, hey CentOS!
  <% else %>
  Ah, it's not CentOS!
  <% end %>
</p>
ERBEOF
```

> attributes loaded in sequence of roles, then lexical order

---

## Faster Feedback

#### Chef Testing

> * Did chef-client run complete successfully (tool: test-kitchen)
> * Did the recipe put node in desired state  (tool: Serverspec)
> * Are resources properly defined            (tool: ChefSpec)
> * Does code follow style guide              (tool: foodcritic)

* **Test Kitchen**
> to test chef-client successful run on desired distros
> * '.kitchen.yml' the configuration file
> * driver : virtualization support
> * provisioner : application to configure the node
> * platforms : target OS
> * suites : target configurations

```
cat $APACHE_COOKBOOK_PATH/.kitchen.yml <<KITCHENEOF
---
driver:
  name: docker

provisioner:
  name: chef_zero

platforms:
  - name: centos-6.4

suites:
  - name: default
    run_list:
      - recipe[apache::default]
    attributes:
      - apache
        - greeting: 'howdy'
KITCHENEOF

## kitchen-docker -- gem enabling test-kitchen to work with docker

## to see list of test-kitchens available
kitchen list

## to create kitchens over provided virt
kitchen create

## executing chef on kitchen
kitchen converge

## to log-in and check to target kitchen {user:kitchen; password:kitchen}
kitchen login

## to clean-up the kitchen, ready to create again
kitchen destroy

## to run 'serverspec' tests (it will make sure 'create' and 'converge' is done)
kitchen verify

## create -to-> converge -to-> verify -to-> destroy
kitchen test
```

* **Serverspec**
> RSpec tests to verify servers are in desired state; independent of chef
> Defines many resource types, works well with TestKitchen; [serverspec.org](http://serverspec.org)

```
APACHE_DEFAULT_SERVERSPEC_PATH="$APACHE_COOKBOOK_PATH/test/integration/default/serverspec"
mkdir -p $APACHE_DEFAULT_SERVERSPEC_PATH

cat > $APACHE_DEFAULT_SERVERSPEC_PATH/default_spec.rb <<SERVERSPECEOF
require 'serverspec'

include Serverspec::Helper::Exec
include Serverspec::Helper::DetectOS

describe 'apache' do
  it "is installed" do
    expect (package 'httpd').to be_installed
  end

  it "is running" do
    expect (service 'httpd').to be_running
  end

  it "is listening on port #0" do
    expect (port  $0).to be_listening
  end

  it "displays a custom homepage" do
    expect (command 'curl localhost').to return_stdout(/howdy/)
  end
end
SERVERSPECEOF
```

* **ChefSpec**
> to check if resources are properly defined; test before you converge
> it just depends on chef, doesn't need a node to converge

```
APACHE_CHEFSPEC_PATH="$APACHE_COOKBOOK_PATH/spec/unit"
mkdir -p $APACHE_CHEFSPEC_PATH

cat > "$APACHE_CHEFSPEC_PATH/default.rb" <<CHEFSPECEOF
require 'chefspec'

describe 'apache::default' do
  let (:chef_run) do
    ChefSpec::Runner.new.converge(described_recipe)
  end

  it 'install apache' do
    expect(chef_run).to install_package('httpd')
  end
end
CHEFSPECEOF

## to run chefspec
rspec $APACHE_CHEFSPEC_PATH/*.rb
```

* **FoodCritic**
> Check cookbook for common problems for style, correctness, deprecations, etc.

```
foodcritic $APACHE_COOKBOOK_PATH
```

> can use 'guard' gem to run specific tests when code changes

---
---
