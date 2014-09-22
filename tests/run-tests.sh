#!/bin/sh

set -e

if [ ! -f ./mistral.cabal ]; then
  echo "tests must be run from the top of the mistral repository"
  exit 1
fi

if [ ! -d build ]; then
  ./build.sh
fi

cabal test --show-details=always \
           --test-option=--report-xml=report.xml \
           --test-option=tests/syntax \
