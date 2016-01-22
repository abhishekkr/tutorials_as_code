FROM alpine:latest
MAINTAINER abhishekkr <abhikumar163@gmail.com> (@abionic)

LABEL alpine.group="development"
LABEL version="0.1"

ENV GO_HOME /.go
ENV GOROOT /usr/lib/go
ENV GOPATH $GO_HOME
ENV GOBIN $GO_HOME/bin
ENV PATH $PATH:$GOROOT/bin:$GOPATH/bin

RUN mkdir -p $GOPATH/src $GOPATH/bin && chmod -R 777 $GO_HOME

RUN apk update && apk add bash curl git mercurial bzr subversion openssh-client ca-certificates go && rm -rf /var/cache/apk/*
