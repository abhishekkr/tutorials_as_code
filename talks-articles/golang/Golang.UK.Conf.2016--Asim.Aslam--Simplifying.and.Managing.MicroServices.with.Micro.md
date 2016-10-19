
## Simplifying and Managing MicroServices with Micro

> by, Asim Aslam
> at Golang UK Conf 2016


[micro](https://github.com/micro)
[go-micro](https://github.com/micro/go-micro)
[go-plugins](https://github.com/micro/go-plugins)
[go-platform](https://github.com/micro/go-platform)
[slack channel](slack.micro.mu)


Humble Beginnings
> started with `go-micro`
> pluggable RPC framework
> service discovery, encoding, sync/async comm
> leveraged youtube/vitess/rpcplus
> OpenSource Jan'15


* Golang Landscape
> net/http              -to- gizmo
> net/rpc               -to- go-kit
> gorilla/rpc           -to- gRPC
> in-house frameworks   -to- micro
> and many more


* Beyond Go
> Netflix OSS in Java
> Seneca in Node.JS
> Nameko in Python
> Finagle in JVM
> Linkerd ~ Agnostic ; idea should be foundation of write in any language and use RPC proxy to distribute system aspect


* Toolkit
> Gi-micro (RPC framework)
> Sidecar (HTTP Proxy)
> API (HTTP => RPC)
> Web (UI+Proxy)
> CLI (Terminal)
> Bot (Hubot)

---

### Go-micro

> Pluggable RPC Framework
> Strongly defined interfaces
> Sane defaults, consul, http, {json,proto}-rpc
> extensible via wrappers/middleware
> 

```
 addresses core requirements   ,----------------------------------------------,
 * service discovery           |[                 Service                    ]|
 * message encoding            ||--------------------------------------------||
 * request-response            |[        Client         ][       Server      ]|
 * pub-sub                     ||--------------------------------------------|
 * client-side load balancing  |[Broker][Codec][Registry][Selector][Transport]|
                               '----------------------------------------------'
```

#### Interfaces

Service Discovery example

* Registry

```
type Registry interface {
  Register(*Service, ...RegisterOption) error
  Deregister(*Service) error
  GetService(string) ([]*Service, error)
  ListServices() ([]*Service, error)
  Watch() (Watcher, error)
  String() string
}
```

* Consul

```
type consulRegistry struct {}

func (c *consulRegistry)  Register(*Service, ...RegisterOption) error {...}
func (c *consulRegistry)  Deregister(*Service) error {...}
func (c *consulRegistry)  GetService(string) ([]*Service, error) {...}
func (c *consulRegistry)  ListServices() ([]*Service, error) {...}
func (c *consulRegistry)  Watch() (Watcher, error) {...}
func (c *consulRegistry)  String() string {...}

func NewRegistry(...Option) Registry {
  return &consulRegistry
}

```

---

### Writing a Service

* protobuf definition of a service, define api

```
syntax = "proto3";

service Greeter {
  rpc Hello(Request) returns (Response) {}
}

message Request {
  string name = 1;
}

message Response {
  string greeting = 1;
}
```


* define handler

```
import (
  "github.com/path/to/proto"
  "golang.org/x/net/context"
    )

type Greeter struct{}

func (g *Greeter) Hello(ctx context.Context, req *proto.Request, rsp *proto.Response) error {
  rsp.Greeting = "Hello " + req.Name
  return nil
}
```


* create service

```
service := micro.NewService(
  micro.Name("com.example.srv.greeter")
  micro.Version("1.0.0")
    )

service.Init()

proto.RegisterGreeterHandler(service.Server(), new(Greeter))

if err := service.Run(); err != nil {
  log.Fatal(err)
}
```


* call service at client

```
greeter := proto.NewGreeterClient("com.example.srv.greeter", service.Client())

rsp, err := greeter.Hello(context.TODO(), &proto.Request{
  Name: "John"
    })

if err != nil {
  //handle error
}

fmt.Println(rsp.Greeting) // Hello John
```

---

### Beneath the Covers

##### Server Side

* generates service definition 

* register with service discovery

* listen and accept requests

* decode and handle requests

* optionally heartbeat with discovery


##### Client Side

* request encoding

* service lookup

* node selection

* connection pooling

* retries, timeouts, backoff

* making the request

---

> Rate-limiting, Circuit-breaking, ...?
> using middleware...

* Authentication
* Circuit Breaking
* Rate Limiting
* Logging
* Event Notifications
* Instrumentation
* Context Injection

```
/*
client      type Wrapper func(Client) Client
server      type HandlerWrapper func(HandlerFunc) HandlerFunc
*/

service := micro.NewService( {
  micro.Name("com.example.srv.foobar")
  micro.WrapClient(circuitBreaker(3)),
  micor.WrapHandler(rateLimiter(10)),
    }
```

---

### Entrypoint

> micro API

* External gateway aka API Gateway
* API Service Layer
* HTTP => RPC, or reverse proxy
* Path based resolution '/[service]/[method]'
* zero-config


> micro web

* web dashboard
* reverse proxy


> Sidecar

* replicates features of go-micro as a HTTP interface


> CLI

```
micro get service $SERVICE_NAME
```


> Bot

* support hipchat and slac

---

Swap out backends

* broker/kafka
* registry/etcd
* transport/nats

---
---

