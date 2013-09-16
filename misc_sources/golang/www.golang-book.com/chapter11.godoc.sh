#!/bin/bash

godoc ./chapter11 Shout

echo "Visit the Go Documentation at localhost:8888"
godoc -http=":8888"
