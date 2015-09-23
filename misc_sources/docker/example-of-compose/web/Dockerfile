FROM python:2.7
MAINTAINER abhishekkr

LABEL infra.group="web"
LABEL version="0.1"


ADD ./app-code /code
WORKDIR /code
RUN pip install -r requirements.txt
CMD python app.py
