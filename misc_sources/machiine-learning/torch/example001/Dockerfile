# VERSION               0.0.1

FROM      abhishekkr/ml-torch
MAINTAINER AbhishekKr <abhikumar163@gmail.com>

ENV PATH              /opt/torch/install/bin:$PATH
ENV LD_LIBRARY_PATH   /opt/torch/install/lib:$LD_LIBRARY_PATH 

ADD https://raw.githubusercontent.com/andresy/torch-demos/master/linear-regression/example-linear-regression.lua /tmp/example-linear-regression.lua

RUN th /tmp/example-linear-regression.lua
