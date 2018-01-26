#!/bin/bash

set -e

mio=../circ-obfuscation/mio
skip_mife=""
naive=""

usage () {
    echo "gc-bench.sh: benchmark our scheme"
    echo "Usage: $0 [options] CIRCUIT"
    echo "  -t      skip mife"
    echo "  -n      naive"
    exit $1
}

POSITIONAL=()
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -t)
            skip_mife=1
            shift
            ;;
        -n)
            naive="-n"
            shift
            ;;
        -h) 
            usage 0
            shift
            ;;
        *)
            POSITIONAL+=("$1")
            shift
    esac
done
set -- "${POSITIONAL[@]}"

if [[ -z "$1" ]]; then
    usage 1
fi

circuit=$1

ninputs=0
test_inp=()
test_out=()

while read -r line && [[ "$line" != ":start" ]]; do
    res=$(echo $line | perl -nE 'print $1 if /:ninputs (\d+)/')
    [[ $res ]] && ninputs=$res

    inp=$(echo $line | perl -nE 'print $1 if /:test (\d+) (\d+)/')
    out=$(echo $line | perl -nE 'print $2 if /:test (\d+) (\d+)/')
    if [[ $inp ]] && [[ $out ]]; then
        test_inp+=($inp)
        test_out+=($out)
    fi
done < $circuit

ntests=${#test_inp[@]}

function eval_test() {
    inp=$1
    ./boots wires $inp
    ./boots test
    ./boots eval
}

function eval_mife() {
    inp=$1
    ./boots wires $inp

    echo "MIFE UNIMPLEMENTED" > /dev/stderr
    exit 1

    ./boots eval
}

for (( i=0; i<${ntests}; i++)); do
    echo -n "test $i/$ntests: ${test_inp[$i]} -> ${test_out[$i]} ... "
    ./boots garble $circuit $naive
    if [[ "$skip_mife" ]]; then 
        res=$(eval_test ${test_inp[$i]})
    else
        res=$(eval_mife ${test_inp[$i]})
    fi
    if [[ "$res" = "${test_out[$i]}" ]]; then
        echo "ok"
    else
        echo "FAILED"
    fi
done
