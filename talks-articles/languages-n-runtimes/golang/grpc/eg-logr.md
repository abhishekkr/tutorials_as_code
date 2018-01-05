
### Logr

#####  Sample GRPC Client Server

> new `logr` grpc client-server created following steps from [source](http://www.agiratech.com/building-high-performance-apis-go-grpc/ )


* fetch grpc go package

```
go get google.golang.org/grpc
```


* install `protoc` Protocol Buffers v3


* fetch protobuf go package

```
github.com/golang/protobuf/{proto,protoc-gen-go}
```


* creating base skeleton [project space](./eg-logr), adding [proto def](./eg-logr/logr/logr.proto) and generating [code](./eg-logr/logr/logr.pb.go)

```
mkdir eg-logr/{client,server,logr}

cd eg-logr
touch logr/logr.proto  ## push in required proto def

protoc -I logr/ logr/logr.proto --go_out=plugins=grpc:logr
```


* create a [server](./eg-logr/server/logr.go) to utilize the generated protobuf package


* create a [client](./eg-logr/client/logr.go) to talk to server producing and consuming log lines, using pre-generated protobuf package

---

