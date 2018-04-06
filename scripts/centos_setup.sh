#!/bin/bash

sudo yum install -y rpm-build zlib-devel epel-release
sudo yum update -y
sudo yum install -y cabal-rpm

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

curl -sSL https://get.haskellstack.org/ | sh

cd /vagrant
stack setup
