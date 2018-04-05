#!/bin/bash

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

# Download and unpack the stack executable
if [[ ! -x ~/.local/bin/stack ]]; then
  #travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
  #chmod a+x ~/.local/bin/stack
  curl -sSL https://get.haskellstack.org/ | sh
fi
stack --version

# Install GHC
stack setup
stack exec -- ghc --version

# Install additional build tools
#stack install cabal-rpm cabal-debian