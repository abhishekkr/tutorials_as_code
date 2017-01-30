
## OpenWhisk - A quick tech preview

[source](https://developer.ibm.com/openwhisk/)

#### High-Level Architecture

Provides distributed compute service to execute application logic in response to events.

Several key architectural concepts

* Trigger
> Class of actions emitted by event sources.

* Actions
> Encapsulate actual code to be executed which support multiple language bindings including NodeJS, Swift and arbitrary binary programs encapsulated in containers.
> Actions invoke any part of an open ecosystem including existing Bluemix services for analytics, data, cognitive or any other 3rd party service.

* Rules
> An association between trigger and action.

* Packages
> Describe external services in a uniform manner.

---

[source](https://developer.ibm.com/open/2016/02/22/openwhisk-a-quick-tech-preview/)


* OpenWhisk, cloud-first distributed event-based programming service.

* It provides a programming model to upload event handlers to a cloud service and register handlers to respond to various events.

* Triggers come from cloud sources to the Whisk server. Events processed based on rules enforced by Whisk runtime.

```
                  ,-----,                ,-----,                ,---------,
                  [ CLI ]                [ UI  ]                [ IOS SDK ]
                  '-----'                '-----'                '---------'
                        \\   CRUD triggers, || actions & rules //
                        .\\, invoke actions.||,    ,//=========
                    ,-------------------------------------,
                   /   \_____REST_____________________/    \
 ,---------, ,    [      ________                           ]
 [ Package ]o))   [Rule [ Action ]                          ]
 [ ,-----, ] '    [====>[--------]                          ]
 [ [ Feed] ]      [     [ NodeJS ]                          ]
 [ '-----' ]      [     '--------'                          ]         ,-------------------------,
 '---------'      [                  chain        chain     ]         [ Service Ecosystem       ]
 ,---------, ,    [      ________   / ________   / ________ ]         [                         ]
 [ Package ]o))   [Rule [ Action ] / [ Action ] / [ Action ]] invoke  [ * Bluemix Services      ]
 [ ,-----, ]=====>[====>[--------]==>[--------]==>[--------]]========>[ * 3rd Party Services    ]
 [ [ Feed] ]trig- [     [ Swift  ]   [ NodeJS ]   [ Docker ]]         [ * Self-enabled Services ]
 [ '-----' ]-ger  [     '--------'   '--------'   '--------']         '-------------------------'
 '---------'      [                                         ]
 ,---------, ,    [      ________                           ]
 [ Package ]o))   [Rule [ Action ]                          ]
 [ ,-----, ] '    [====>[--------]                          ]
 [ [ Feed] ]      [     [ Docker ]                          ]
 [ '-----' ]      [     '--------'                          ]
 '---------'      [                                         ]
 ,---------, ,    '-----------------------------------------'
 [ Package ]o))  
 [ ,-----, ] '   
 [ [ Feed] ]     
 [ '-----' ]     
 '---------'     

```

*  OpenWhisk must be build and then deployed on target environment. Tested on Ubuntu and Mac OS/X.


#### Installing OpenWhisk

Dependencies
> * Virtualbox
> * Vagrant
> * Git

```
wget -c https://raw.githubusercontent.com/openwhisk/openwhisk/master/tools/vagrant/Vagrantfile

vagrant up
```

Use a `Cloudant` datastore on Bluemix, will need a Bluemix ID.
Cloud Foundary cli tool `cf` to send appropriate commands to Bluemix to create Cloudant backing store.

to install `cf` tool

```
cd /usr/bin

sudo wget -O cf.tgz  https://cli.run.pivotal.io/stable?release=linux64-binary&source=github 

sudo tar xvzf cf.tgz
```






---
---
