#!/bin/sh

set -e

if [ ! -f build ]; then
  cabal sandbox init --sandbox=build
  cabal install --only-dep --enable-tests
fi

cabal install -j1
