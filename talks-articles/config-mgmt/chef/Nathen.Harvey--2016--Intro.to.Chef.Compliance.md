# Introduction to Chef Compliance
by Nathen Harvey

> Installation, Configuration and Operation

#### Chef compliance

* Allows running complaince againts infrastructure, need to be accessible, not necessarily have chef on them.

* Can produce reports indicating risks and issues by severity and impact levels; has built-in distro specific profiles.

* It leverage 'InSpec', an OpenSource run-time framework and rule-language to specify compliance, security and policy requirements for testing any node in your infrastructure.

* InSpec includes resources to help write auditing rules (typically concept of Servespec plus GUI)

```
SSH_COOKBOOK_PATH="$PWD"
SSH_INSPEC_PATH="$SSH_COOKBOOK_PATH/test/integration/client/inspec"

cat > $SSH_INSPEC_PATH/client_spec.rb <<SSHINSPECEOF
control 'ssh-4' do
  impact 1.0
  title 'Client: Set SSH protocol version to 2'
  desc " Set SSH protocol version to 2. "

  describe ssh_config do
    its('Protocol') { should eq('2') }
  end
end
SSHINSPECEOF

## can be run over docker kitchen instance
inspec exec $SSH_INSPEC_PATH/client_spec.rb -t docker://CONTAINER_ID
```


#### Chef Compliance Server

* It just analyzes the target nodes, not change.

* It has suite of such tests ready to be used.


---
---
