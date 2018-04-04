#!/bin/bash

sudo yum install -y rpm-build zlib-devel

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

curl -sSL https://get.haskellstack.org/ | sh

cd /vagrant
stack setup
stack install cabal-install cabal-rpm