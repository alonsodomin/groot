#!/bin/bash

set -e

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
  brew update
elif [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  apt-get install -y libpthread-stubs0-dev
fi

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

# Download and unpack the stack executable
if [[ ! -x ~/.local/bin/stack ]]; then
  curl -sSL https://get.haskellstack.org/ | sh
fi
stack --version

# Install GHC
stack setup
stack exec -- ghc --version

# Install additional build tools
stack install hakyll # cabal-rpm cabal-debian