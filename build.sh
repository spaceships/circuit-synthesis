#!/bin/bash

cabal new-build
exe=$(find dist-newstyle -name circuit-synthesis -type f)
if [ ! "$exe" ]; then
    echo "error: no binary"
    exit 1
fi
echo "created executable cxs"
cp $exe cxs
