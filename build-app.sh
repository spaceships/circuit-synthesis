#!/bin/bash

set -e

apps="cxs boots"

usage () {
    echo "Usage: $0 [-p] [-o APP]"
    echo "Options:"
    echo "  -p      build with profiling turned on"
    echo "  -o APP  only build app APP"
    exit $1
}

profiling=""
only_app=""

while getopts "hpo:" opt; do
    case $opt in
        h) usage && exit 0;;
        p) profiling=1;;
        o) only_app=$OPTARG;;
        *) usage && exit 1;;
    esac
done

shift $((OPTIND-1))

if [ $only_app ]; then
    if ! perl -E "exit 1 unless \"$apps\" =~ \"$only_app\""; then
        echo "error: unknown program \"$only_app\""
        echo "supported programs: $apps"
        exit 1
    fi
    apps=$only_app
fi

for app in $apps; do
    echo -en '\e[0;32m'
    echo "building $app"
    echo -en '\e[0m'
    if [ $profiling ]; then
        cabal new-build $app --enable-profiling --profiling-detail=all-functions
    else
        cabal new-build $app
    fi
done

for app in $apps; do
    exe=$(find dist-newstyle -name $app -type f | head -n1)
    if [ ! "$exe" ]; then
        echo "error: no binary in build directory"
        exit 1
    fi
    echo -en '\e[0;32m'
    echo "created executable $app"
    echo -en '\e[0m'
    cp $exe $app
done
