#!/usr/bin/env bash

# Grab a thread as HTML
# usage: ./html-get.sh <thread_id>

curl -XGET localhost:8001/thread/$1 \
  -H 'Accept: text/html'
