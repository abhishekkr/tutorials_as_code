
## Pritunl

#### setup script

```
#!/bin/bash

set -ex

echo "deb http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.2 multiverse" > /etc/apt/sources.list.d/mongodb-org-3.2.list
echo "deb http://repo.pritunl.com/stable/apt xenial main" > /etc/apt/sources.list.d/pritunl.list
apt-key adv --keyserver hkp://keyserver.ubuntu.com --recv 42F3E95A2C4F08279C4960ADD68FA50FEA312927
apt-key adv --keyserver hkp://keyserver.ubuntu.com --recv 7568D9BB55FF9E5287D586017AE645C0CF8E292A
apt-get --assume-yes update
apt-get --assume-yes upgrade

apt-get --assume-yes install pritunl mongodb-org

systemctl start pritunl mongod
systemctl enable pritunl mongod
```

#### configure

* to create a setup key for web-app to get into config section on init

```
sudo pritunl setup-key
```

* default pritunl web admin creds: `user: pritunl` `password: pritunl`, could be changed in settings


* to reset web admin password, in case forgotten later

```
sudo pritunl reset-password
```

#### create a usable openvpn

* create an organization, add users to it

* create a server, attach organization to it, attach route to it, start server

* download profiles for users in organization

---
