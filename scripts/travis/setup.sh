#!/bin/bash

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
  brew update
  brew install llvm
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
#stack install cabal-rpm cabal-debian