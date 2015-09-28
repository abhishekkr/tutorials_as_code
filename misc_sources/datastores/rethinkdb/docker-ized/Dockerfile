FROM centos:7
MAINTAINER abhishekkr <abhikumar163@gmail.com>

ENV RETHINKDB_DIR /rethinkdb
ENV RETHINKDB_LOGDIR $RETHINKDB_DIR/log
ENV RETHINKDB_DATADIR $RETHINKDB_DIR/data

ADD rethinkdb.conf /rethinkdb.conf
ADD rethinkdb.run  /rethinkdb.run

# Install RethinkDB
RUN yum install -y wget

RUN wget http://download.rethinkdb.com/centos/6/`uname -m`/rethinkdb.repo \
          -O /etc/yum.repos.d/rethinkdb.repo

RUN  yum install -y rethinkdb && \
     yum clean -y all

RUN  rethinkdb create --directory $RETHINKDB_DIR
RUN  rethinkdb create --directory $RETHINKDB_LOGDIR
RUN  rethinkdb create --directory $RETHINKDB_DATADIR

RUN  chmod +x /rethinkdb.run

# WebUI Port
EXPOSE 8080

# Client Driver Port
EXPOSE 28015

# Intracluster Port
EXPOSE 29015

VOLUME ["$RETHINKDB_DIR"]

CMD ["/rethinkdb.run"]
