FROM centos:7
MAINTAINER abhishekkr <abhikumar163@gmail.com>

# Install InfluxDB
ENV TEMPDIR			/tmp/influx
ENV INFLUXDB_VERSION 		0.9.4_4609-1
ENV INFLUXDB_RPM 		influxdb-${INFLUXDB_VERSION}.x86_64.rpm
ENV INFLUXDB_REMOTE_PATH 	https://s3.amazonaws.com/influxdb/${INFLUXDB_RPM}
ENV INFLUXDB_LOCAL_PATH 	$TEMPDIR/influxdb-${INFLUXDB_VERSION}.rpm

RUN mkdir -p $TEMPDIR
RUN curl -s -o $INFLUXDB_LOCAL_PATH $INFLUXDB_REMOTE_PATH   && \
  yum install -y curl && \
  yum localinstall -y $INFLUXDB_LOCAL_PATH && \
  rm -rf $TEMPDIR && \
  yum clean all

ADD config.toml /config/config.toml
ADD run.sh /run.sh
RUN chmod +x /*.sh

ENV PRE_CREATE_DB **None**
ENV SSL_SUPPORT **False**
ENV SSL_CERT **None**

# Admin server
EXPOSE 8083

# HTTP API
EXPOSE 8086

# HTTPS API
EXPOSE 8084

# Raft port (for clustering, don't expose publicly!)
#EXPOSE 8090

# Protobuf port (for clustering, don't expose publicly!)
#EXPOSE 8099

VOLUME ["/data"]

CMD ["/run.sh"]
## CMD ["/etc/init.d/influxdb start"]
