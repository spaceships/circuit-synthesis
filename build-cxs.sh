#!/bin/bash

set -e

usage () {
    echo "build-cxs.sh"
    echo "Usage: $0 [-p]"
    echo "Options:"
    echo "  -p      build with profiling turned on"
    exit $1
}

profiling=""

while getopts "hp" opt; do
    case $opt in
        h) usage && exit 0;;
        p) profiling=1;;
        *) usage && exit 1;;
    esac
done

shift $((OPTIND-1))

if [ $profiling ]; then
    cabal new-build --enable-profiling --profiling-detail=all-functions
else
    cabal new-build
fi

exe=$(find dist-newstyle -name cxs -type f | head -n1)
if [ ! "$exe" ]; then
    echo "error: no binary"
    exit 1
fi
echo "created executable cxs"
cp $exe cxs
