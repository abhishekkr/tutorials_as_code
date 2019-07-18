
## Gen Quick Start

> [source](https://github.com/probcomp/gen-quickstart)

* a Dockerfile is provided at source which can be used to build an isolated trial container, one can be pulled from [abhishekkr/gen:v0](https://hub.docker.com/r/abhishekkr/gen)

> the Dockerfile setup flow can be used for manual local setup; or just use `docker pull abhishekkr/gen:v0`

* can start container as `docker run -it --name gen -p 8080:8080 -p 8090:8090 -p 8091:8091 -p 8092:8092 gen:v0`, jupyter notebooks will be available at [localhost:8080](http://127.0.0.1:8080/)

* updated notebooks will get saved in named notebooks, can be started again as `docker start -ia gen`

* open `Gen Quick Start.ipynb` notebook for trial

---
