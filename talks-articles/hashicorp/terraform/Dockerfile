FROM alpine:latest
MAINTAINER AbhishekKr <abhikumar163@gmail.com>

ARG GLIBC=2.25-r0
ARG TERRAFORM_VERSION=0.8.7

ARG LANG=en_US.UTF-8
ENV TMPDIR /tmp
ENV TEMP /tmp

RUN apk --no-cache --update add ca-certificates wget unzip openssh-client bash curl jq

RUN wget -q -c -O /etc/apk/keys/sgerrand.rsa.pub https://raw.githubusercontent.com/sgerrand/alpine-pkg-glibc/master/sgerrand.rsa.pub && \
    wget -q -c -O glibc.apk https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.25-r0/glibc-2.25-r0.apk && \
    apk add glibc.apk && \
    rm glibc.apk


RUN cd /usr/local/bin && \
    wget -c -O terraform_${TERRAFORM_VERSION}_linux_amd64.zip https://releases.hashicorp.com/terraform/${TERRAFORM_VERSION}/terraform_${TERRAFORM_VERSION}_linux_amd64.zip && \
    unzip terraform_${TERRAFORM_VERSION}_linux_amd64.zip && \
    rm terraform_${TERRAFORM_VERSION}_linux_amd64.zip


ARG BUILD_DEPS="gettext"
ARG RUNTIME_DEPS="libintl"

RUN apk add --update $RUNTIME_DEPS && \
    apk add --virtual build_deps $BUILD_DEPS &&  \
    cp /usr/bin/envsubst /usr/local/bin/envsubst && \
    apk upgrade && \
    apk del build_deps


RUN rm -rf /var/cache/apk/* && \
    rm -rf /tmp/*


