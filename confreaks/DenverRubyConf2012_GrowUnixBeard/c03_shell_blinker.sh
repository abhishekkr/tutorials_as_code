#!/bin/bash

yes | ruby -ne '$><<"\r\x1b[#{31+($.+=1)%7}mZOMG PREFORKING"; sleep 0.1'
