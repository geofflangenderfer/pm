#!/usr/bin/env bash

if ! test -d dist-newstyle
then
    cabal build --enable-executable-dynamic
fi

$(find dist-newstyle -type f -iregex ".*pm")
