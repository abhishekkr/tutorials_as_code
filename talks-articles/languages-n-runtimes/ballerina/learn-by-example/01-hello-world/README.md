
## Hello World

### Hello World Service

* `service` represents collections of network accessible entry points.
> By default HTTP service exposes HTTP/1.1 as in [hello-world-svc.bal](./hello-world-svc.bal).

* `resource` represents entry point, exposed over a network protocol of listener service is attached to
> `http:caller, http:request` arguments passed to invoked resource functions
>
> `http:caller->respond(...)` to send back response to caller

* running the server

```
% ballerina run ./hello-world-svc.bal
Initiating service(s) in 'hello-world-svc.bal'
[ballerina/http] started HTTP/WS endpoint 0.0.0.0:9090
```

* usage

```
## unavailable resource
% curl http://localhost:9090/hello/
no matching resource found for path : /hello , method : GET

## available resource
% curl http://localhost:9090/hello/sayHello
Hello, World!
```

---

### Hello World Main

* public function named `main` is considered default entry point

```hello-world-main.bal
import ballerina/io;

public function main() {
    io:println("Hello, World!");
}
```

---

### Hello World Parallel

* sequence of `worker` for a set of tasks that can run concurrently, invoking function starts all workers

```
function wxyz() {
  worker wx {...}
  worker wy {...}
  worker wz {...}
}
```

* example [hello-world-parallel.bal](./hello-world-parallel.bal) says 3 hello-worlds concurrently

---

### Hello World Client

* clients can be used to connect and interact with HTTP endpoints, creates HTTP/1.1 client by default

---
