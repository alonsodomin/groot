#!/bin/bash

set -e

stack --no-terminal test --haddock --no-haddock-deps
stack --local-bin-path . install groot

mkdir -p dist
mv groot "dist/groot-$TRAVIS_OS_NAME"