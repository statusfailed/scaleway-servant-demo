#!/usr/bin/env bash
set -e

# populate API with some fake data.
# usage: ./populate.sh

create () {
  curl \
    localhost:8001/thread \
    -XPOST \
    -vvv \
    -H 'Content-Type: application/json' \
    -d"{\"link\": \"$1\", \"title\": \"$2\"}"
}

comment () {
  curl \
    localhost:8001/thread/$1 \
    -XPOST \
    -vvv \
    -H 'Content-Type: application/json' \
    -d"{\"user\": \"$2\", \"text\": \"$3\"}"
}

create "http://www.reddit.com" "Reddit: a site with compact design"
comment 0 "anonymous" "hello world"

create "http://www.weibo.com" "Sina weibo: a chinese microblogging site"
comment 1 "微博" "It's big in china"

create \
  "https://haskell-servant.github.io" \
  "Servant: A haskell library for building webservices"
comment 2 "foobar" "All of these comments are fake"
comment 2 "scaleway" "You can run servant apps on scaleway"
comment 2 "statusfailed" "Servant is a great project"
