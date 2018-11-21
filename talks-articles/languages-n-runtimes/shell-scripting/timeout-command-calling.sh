#!/usr/bin/env bash

timeout 1 read ; echo "hello"

timeout 1 sleep 60 ; echo "did it took 1s or 60s"
