
## Setup Kubernetes using KubeSpray on Local VM

> * Or Localhost, any Cloud Instance;
> * actually any ssh-able machine.

* Fetch Kubespray and install dependencies

```
virtualenv3 .venv && source .venv/bin/activate
git clone https://github.com/kubernetes-incubator/kubespray.git
cd kubespray
git checkout v2.23.1
pip install --upgrade pip && pip install -r requirements.txt
```

* Generate required ansible YAML for VM to be configured

```
cp -rfp inventory/sample inventory/test-k8s
declare -a IPS=(192.168.124.66)
CONFIG_FILE=inventory/test-k8s/hosts.yaml python3 contrib/inventory_builder/inventory.py ${IPS[@]}

# all > hosts > node..:
#       ansible_connection: ssh
#       ansible_user: ...
#       ansible_ssh_pass: ...
#       ansible_sudo_pass: ...
vim inventory/test-k8s/hosts.yaml

# disable_host_nameservers=...
# upstream_dns_servers=...
vim inventory/test-k8s/group_vars/all/all.yml

# cluster_name=...
# event_ttl_duration=...
vim inventory/test-k8s/group_vars/k8s_cluster/k8s-cluster.yml

# helm_enabled=..
vim inventory/test-k8s/group_vars/k8s_cluster/addons.yml
```

> For this tag version we need to update `roles/kubernetes/node/tasks/main.yml` for Modprobe of `nf_conntrack`
>
> can `git apply ./kubespray-v2.23.1.patch`; the [patch file is available here](./kubespray-v2.23.1.patch).


* cleanup

```
ansible-playbook -i inventory/test-k8s/hosts.yaml  --become --become-user=root reset.yml
```

* setup

```
ansible-playbook -i inventory/test-k8s/hosts.yaml  --become --become-user=root cluster.yml
```

* access from local

```
scp @node:/etc/kubernetes/admin.conf @local:~/.kube/config

kubectl get nodes
```

---
