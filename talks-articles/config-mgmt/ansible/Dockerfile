FROM abhishekkr/abk-alpine-ansible:latest
MAINTAINER AbhishekKr <abhikumar163@gmail.com>

ENV ANSIBLE_VERSION 2.4.0.0

ENV LANG en_US.UTF-8
ENV TMPDIR /tmp
ENV TEMP /tmp

RUN rmdir /opt/vesemir

RUN apk --no-cache --update add ca-certificates wget openssh-client bash curl


ARG BUILD_DEPS="gettext"
ARG RUNTIME_DEPS="libintl"

RUN apk add --update $RUNTIME_DEPS && \
    apk add --virtual build_deps $BUILD_DEPS &&  \
    cp /usr/bin/envsubst /usr/local/bin/envsubst && \
    apk upgrade && \
    apk del build_deps


RUN rm -rf /var/cache/apk/* && \
    rm -rf /tmp/*


RUN pip install pip --upgrade
RUN pip install ansible==$ANSIBLE_VERSION
