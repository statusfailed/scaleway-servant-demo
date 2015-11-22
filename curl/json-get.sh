#!/usr/bin/env bash

# Grab the index page as HTML
# usage: ./json-get.sh <thread_id>

curl -XGET localhost:8001/thread/$1 \
  -H 'Accept: application/json'
