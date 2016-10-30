
## MariaDB - Lock it down like a chastity belt

> by, Ben Stillman
> at, DerbyCon 6.0 Louisville 2016


### `mysql_secure_installation`

* set a root password
* drop anonymous users
* disable remote root login
* drop test db, remove privileges on test db
* reload privileges table

---

### `mysql_secure_installation` doesn't

* rename root user, cuz default usernames are bad

```
RENAME USER 'root'@'localhost' TO 'some_new_root_user'@'localhost';
```

* disable symlinks
> example someone gets access, creates symlink to mysql user table and load their own privileges

```
;; differs with version
have_symlink = 0
```

* disable `local_infile`
> trying import enormous record CSV from laptop, bad for prod

```
local_infile = 0
```

* skip dns lookup (can faked/posion/break)

```
skip_name_resolve = 1
```

* disable auto user create
> if Grant without password, and user doesn't exist; user gets created without password

```
sql_mode = no_auto_create_user
```

---

### Users and Roles

* create user with `CREATE USER`

```
CREATE USER 'alice'@'10.1.1.10' IDENTIFIED BY 'rock-solid-password';

;; allow for subnet
CREATE USER 'alice'@'10.1.%' IDENTIFIED BY 'rock-solid-password';
```


* create user with `GRANT` need to have `IDENTIFIED`

```
GRANT SELECT ON db.* TO 'newuser'@'10.1.1.10'
  IDENTIFIED BY 'rock-solid-password';
```


* Roles

```
CREATE ROLE read_only;

GRANT SELECT ON db.* TO read_only;

GRANT read_only TO 'olduser'@'10.1.1.10'
```


* Find users with no passwords

```
SELECT user,host FROM mysql.user WHERE password='';

SET PASSWORD FOR 'olduser'@'10.1.1.10' = PASSWORD('new-password-to-be-changed');
```


* Find users at any host

```
SELECT user,host FROM mysql.user WHERE host='%';

RENAME USER 'olduser'@'%' TO 'olduser'@'10.1.%';
```


#### Password Hashing

```
SET PASSWORD FOR 'olduser'@'10.1.1.10' = PASSWORD('all-new-password');

SELECT password FROM mysql.user WHERE user = 'olduser' and host = '10.1.1.10';
;; if value is a hash
```


* Find users with old password hash

```
SELECT user,host FROM mysql.user WHERE LENGTH(password) < 41;

SHOW GLOBAL VARIABLES LIKE 'old_passwords';

SET PASSWORD FOR 'olduser'@'10.1.10' = PASSWORD('new-secure-password');
```


#### Privileges

```
;; this should never be the case, at the most provide SELECT,INSERT,UPDATE,DELETE on specific table
GRANT ALL PRIVILEGES ON *.* TO 'anyuser'@'%';

GRANT SELECT ON db1.* TO 'olduser'@'10.1.1.10';
GRANT INSERT,UPDATE ON db2.tableOrders TO 'newuser'@'10.1.1.10';
```

* Find users with Global Privileges

```
SELECT user,host,select_priv,insert_priv,update_priv,delete_priv,
    create_priv,drop_priv,grant_priv,index_priv,alter_priv,create_tmp_table_priv,
    lock_tables_priv,create_view_priv,show_view_priv,create_routine_priv,
    alter_routine_priv,execute_priv,event_priv,trigger_priv
  FROM mysql.user
  ORDER BY user, host \G
```

* Show a user's grants

```
SHOW GRANTS FOR 'olduser'@'10.1.1.10';
```

#### simple password check plugin

```
MariaDB> INSTALL SONAME 'simple_password_check';
```

* config

```
simple_password_check_minimal_length = 8
simple_password_check_digits = 1
simple_password_check_letters_same_case = 1
simple_password_check_other_characters = 1
```

#### cracklib password check plugin

> can help eliminate dictionary passwords

```
MariaDB> INSTALL SONAME 'cracklib_password_check';


$ semanage fcontext -a -t mysqld_etc_t "/usr/share/cracklib(/.*)?"

$ restorecon -Rv /usr/share/cracklib
```

#### `Unix_socket` auth plugin

```
MariaDB> INSTALL PLUGIN unix_socket SONAME 'auth_socket';
```

```
$ whoami
ben

MariaDB> CREATE USER 'wontwork'@'localhost' IDENTIFIED VIA unix_socket;

$ mysql --user=wontwork
ERROR 1045 (28000): Access denied for user 'wontwork'@'localhost' (using password: NO)

MariaDB> CREATE USER 'ben'@'localhost' IDENTIFIED VIA unix_socket;

$ mysql --user=ben
Welcome.
```

#### PAM plugin install

```
MariaDB> INSTALL SONAME 'auth_pam';
```

##### LDAP Auth

```/etc/pam.d/mariadb

auth        required    pam_ldap.so
account     required    pam_ldap.so
```

```
MariaDB> CREATE USER 'aduser'@'10.1.1.10' IDENTIFIED VIA pam USING 'mariadb';
```

* LDAP group mapping install

```
wget https://raw.githubusercontent.com/MariaDB/server/10.1/plugin/auth_pam/mapper/pam_user_map.c

gcc pam_user_map.c -shared -lpam -fPIC -o pam_user_map.so

sudo install --mode=0755 pam_user_map.so lib64/security/
```

* group mapping config

```/etc/security/user_map.conf

## @group: proxy_user ##
@dba: dba
@accounting: accounting
```

```/etc/pam.d/mariadb

auth        required    pam_ldap.so         audit
account     required    pam_ldap.so         audit
auth        required    pam_user_map.so
```

* group mapping in use

```
MariaDB> GRANT ALL PRIVILEGES ON *.* TO 'dba'@'10.1.1.%' IDENTIFIED BY '1a9876987@$FL_9&';

MariaDB> GRANT ALL PRIVILEGES ON *.* TO 'dba'@'10.1.1.%' IDENTIFIED BY '1a9876987@$FL_9&';
```

```
;; create anon user and grant it proxy
MariaDB> CREATE USER ''@'%' IDENTIFIED VIA pam USING 'mariadb';

MariaDB> GRANT PROXY ON 'dba'@'%' TO ''@'%';
MariaDB> GRANT PROXY ON 'accounting'@'%' TO ''@'%';
```

```
$ whoami
ben

$ groups
dba

$ mysql --user=ben
Welcome.
```


##### RSA SecurID 2-factor auth

```/etc/pam.d/mariadb

auth        required    pam_securid.so
account     include     pam_securid.so
auth        include     pam_securid.so
```


##### Google Auth 2-factor auth

```/etc/pam.d/mariadb

auth        required    pam_unix.so
auth        include     pam_google_authenticator.so
account     include     pam_unix.so
```


##### Kerberos/GSSAPI plugin

> currently in beta

* server-side prep

```
$ kadmin -q "addprinc -randkey mariadb/host.domain.com"

$ kadmin -q "ktadd -k /path/to/mariadb.keytab mariadb/host.domain.com"
```

* install plugin

```
MariaDB> INSTALL SONAME 'auth_gssapi';
```

* config file

```
gssapi_keytab_path = /path/to/mariadb.keytab

gssapi_principal_name = alternative/principalname@REALM
```

* now just create user

```
MariaDB> CREATE USER 'newuser'@'10.1.1.10' IDENTIFIED VIA gssapi AS 'bin/mariadb';
```

---

### Connection Encryption

* server config

```
[server]
ssl_ca = /path/to/ca-cert.pem
ssl_cert = /path/to/server-cert.pem
ssl_key = /path/to/server-key.pem
```

* client config, need to be careful for app library connectors

```
[client]
ssl_cert = /path/to/client-cert.pem
ssl_key = /path/to/client-key.pem
```

* confirm configs

```
MariaDB> SHOW GLOBAL VARIABLES LIKE 'have_%ssl';
```

* create ssl users

```
MariaDB> CREATE USER 'newuser'@'10.1.1.10' IDENTIFIED BY 'rock-solid-password' REQUIRE SSL;

MariaDB> GRANT USAGE ON *.* TO 'newuser'@'10.1.1.10' REQUIRE SSL;
```

#### SSL for Replication

```
MariaDB> GRANT USAGE ON *.* TO 'repluser'@'host' REQUIRE SSL;

MariaDB> STOP SLAVE;

MariaDB> CHANGE MASTER TO MASTER_SSL=1;

MariaDB> START SLAVE;
```

#### SSL for Galera Replication

```
wsrep_provider_options = "socket.ssl_key=/path/to/server-key.pem;socket.ssl_cert=/path/to/server-cert.pem;socket.ssl_ca=/path/to/cacert.pem"
```

* SSL for Galera xtrabackup-v2 SST

```
[sst]
encrypt = 3
tca = /path/to/ca.pem
tkey = /path/to/key.pem
tcert = /path/to/cert.pem
```

---

### Data Encryption

#### Data at rest

* config

```
$ openssl enc -aes-256-ctr -k your_password -P -md sha1

file_key_management_filename = /path/to/keys.txt

keys.txt
1;3ffffffffffffff-some-long-alphanumeric-text
```


* Data at rest - config options

```
plugin_load_add = file_key_management.so

file_key_management
file_key_management_filename = /path/to/keys.txt
file_key_management_encryption_algorithm
file_key_management_filekey
encrypt_tmp_disk_tables
encrypt_tmp_files
encrypt_binlog
aria_encrypt_tables

innodb_default_encryption_key
innodb_encrypt_tables
innodb_encrypt_log
innodb_encryption_rotate_key_age
innodb_encryption_rotation_iops
innodb_encryption_threads
innodb_tablespace_encryption
```

* Data at rest - encrypt table

```
MariaDB> CREATE TABLE tbl (id int, value varchar(25)) ENCRYPTED=YES ENCRYPTION_KEY_ID=1;

MariaDB> ALTER TABLE tbl ENCRYPTED=YES ENCRYPTION_KEY_ID=1;
```


##### Word of Caution

* Slow Query Log `ain't encrypted`
* General query log `ain't encrypted`
* Error log `ain't encrypted`
* Galera's cache file `ain't encrypted`
* Audit plugin log `ain't encrypted`

* Xtrabackup doesn't work

* mysqlbinlog can't read encrypted binary logs

* only data at rest is encrypted


#### Eperi Gateway

* Gateway/proxy for encrypting data

* Can use triggers/views to encrypt data

* Data is stored encrypted in the database

* Encryption rules

* Key managed by gateway

* Commercial Product


#### DES ENCRYPT and DECRYPT

> encrypt data while insert

```
MariaDB> CREATE TABLE t1 (id int, text BLOB);

MariaDB> INSERT INTO test.t1 VALUES (1, DES_ENCRYPT('rock maria', 'secret-passphrase'));

MariaDB> SELECT * FROM t1;
;; encrypted value

MariaDB> SELECT id, DES_DECRYPT(text, 'secret-passphrase') FROM t1;

```

* can use a DES Key file (no in-built key rotation)

```---des-key-file=/etc/my.cnf.d/des_key_file
0 aKey00000000000000000
1 otherKey0000000000000
2 anotherKey00000000000
3 4thKey000000000000000
4 fifthKey0000000000000
```

```
MariaDB> INSERT INTO test.t1 VALUES (1, DES_ENCRYPT('rock maria', 3));

MariaDB> SELECT * FROM t1;
;; encrypted value

MariaDB> SELECT id, DES_DECRYPT(text, 3) FROM t1;
```


### In Application

* safe bet for DB compromise
* handle encryption/decryption in application, over the wire
* no keys with DB

* but no search query or such at DB layer, cuz DB got no clue

---

### AUDIT Plugin

installing

```
MariaDB> INSTALL PLUGIN server_audit SONAME 'server_audit';
```

configuring

```
server_audit_logging = ON
server_audit = FORCE_PLUS_PERMANENT
server_audit_output_type = SYSLOG
server_audit_events = CONNECT,QUERY,TABLE,QUERY,DDL,QUERY,DML
server_audit_incl_users = alice,bob,eve
## server_audit_excl_users =
## server_audit_file_path = /var/lib/mariadb_audit.log
## server_audit_file_rotate_now = OFF
## server_audit_file_rotate_size = 100000
## server_audit_file_rotations = 9
## server_audit_query_log_limit = 1024
## server_audit_syslog_facility = LOG_USER
## server_audit_syslog_ident = mysql-server_auditing
## server_audit_syslog_info =
## server_audit_syslog_priority = LOG_INFO
```

---
---
