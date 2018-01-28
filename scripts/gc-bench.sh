#!/bin/bash

set -e

function mio() {
    (cd ../circ-obfuscation; ./mio.sh $@)
}

use_mife=1
indexed=1
secparam=""

usage () {
    echo "gc-bench.sh: benchmark our scheme"
    echo "Usage: $0 [options] CIRCUIT"
    echo "  -t          testing mode: no MIFE"
    echo "  -n          naive"
    echo "  -l NUM      security parameter for CLT (none implies dummy mmap)"
    exit $1
}

while getopts "tnl:h" opt; do
    case $opt in
        t) use_mife="";;
        n) indexed="";;
        l) secparam=$OPTARG;;
        h) usage 0;;
        *) usage 1;;
    esac
done
shift $((OPTIND-1))

if [[ $# -ne 1 ]]; then
    usage 1
fi

if [[ ! -f $1 ]]; then
    echo "error: \"$1\" does not exist!"
    exit 1
fi

circuit=$(readlink -f $1)

test_inp=()
test_out=()
while read -r line && [[ "$line" != ":start" ]]; do
    inp=$(perl -nE 'print $1 if /:test (\d+) (\d+)/' <<< $line)
    out=$(perl -nE 'print $2 if /:test (\d+) (\d+)/' <<< $line)
    if [[ $inp ]] && [[ $out ]]; then
        test_inp+=($inp)
        test_out+=($out)
    fi
done < $circuit
ntests=${#test_inp[@]}

################################################################################
## protocol

function unary() {
    perl -E "say '0'x$1, '1', '0'x$(( $2 - $1 - 1))"
}

if [[ $secparam ]]; then
    mmap="--mmap CLT"
    secparam_arg="--secparam $secparam"
else
    mmap="--mmap DUMMY"
    secparam_arg=""
fi

# setup
SECONDS=0
if [[ $indexed ]]; then 
    ./boots garble $circuit
else
    ./boots garble $circuit -n
fi

dir=$(readlink -f obf)
gb=$(readlink -f obf/gb.acirc2)

# set up mife and generate indices
if [[ $use_mife ]]; then
    echo "setting up MIFE"
    mio mife setup $mmap $secparam_arg $gb

    if [[ $indexed ]]; then
        index_len=$(grep -m1 ":symlen" $gb | perl -nE 'print $1 if /:symlens \d+ (\d+)/')
        echo -n "encrypting indices ($index_len):"
        for (( i=0; i < $index_len; i++ )); do
            echo -n " $i"
            ix=$(unary $i $index_len)
            mio mife encrypt $mmap $gb $ix 1 >/dev/stderr
            mv $gb.1.ct $gb.1.ct.ix$i
        done
        echo
    fi
fi
setup_time=$SECONDS

function encrypt() {
    inp=$1
    ./boots wires $inp
    if [[ $use_mife ]]; then
        mio mife encrypt $mmap $gb $(< $dir/seed) 0 >/dev/stderr
    fi
}

function decrypt() {
    if [[ $use_mife ]]; then
        if [[ $indexed ]]; then
            rm $dir/gates
            for (( i=0; i < $index_len; i++ )); do
                cp $gb.1.ct.ix$i $gb.1.ct
                mio mife decrypt $mmap $gb | perl -nE 'say ((split)[1])' >> $dir/gates
            done
        else
            # the perl command splits the output into 360 character lines 
            mio mife decrypt $mmap $gb | 
                perl -nE 'map {say} unpack "(A360)*", (split)[1]' > $dir/gates
        fi
    else
        ./boots test
    fi
    ./boots eval
}

enc_times=()
dec_times=()

for (( i=0; i<${ntests}; i++)); do
    echo -n "test $i/$ntests: ${test_inp[$i]} -> ${test_out[$i]} ... "

    SECONDS=0
    encrypt ${test_inp[$i]}
    enc_times+=($SECONDS)

    SECONDS=0
    res=$(decrypt)
    dec_times+=($SECONDS)

    if [[ $res = ${test_out[$i]} ]]; then
        echo ok
    else
        echo FAILED
    fi
done

avg_enc_time=$(IFS="+"; bc<<<"(${enc_times[*]}) / ${#enc_times[@]}")
avg_dec_time=$(IFS="+"; bc<<<"(${dec_times[*]}) / ${#dec_times[@]}")

################################################################################
## benchmark info

function filesize() {
    if [[ -f $1 ]]; then
        stat $1 --printf="%s"
    else
        echo 0
    fi
}

function sizes() {
    total=0
    for f in $@; do
        total=$(( total + $(filesize $f) ))
    done
    echo $total
}

keysize=$(sizes obf/*.circ obf/*.acirc2 obf/*.1.ct.ix* obf/*.ek)
ctsize=$(sizes obf/wires obf/*.0.ct)

echo
echo "setup time:      $setup_time s"
echo "enc time (avg):  $avg_enc_time s"
echo "dec time (avg):  $avg_dec_time s"
echo "key size:        $((keysize/1024/1024)) mb"
echo "ciphertext size: $((ctsize/1024/1024)) mb"
