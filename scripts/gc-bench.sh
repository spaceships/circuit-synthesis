#!/bin/bash

set -e

function mio() {
    (cd ../circ-obfuscation; ./mio.sh "$@")
}

function progress() {
    x=$(( $1+1 ))
    perl -E "\$r=$x/$2; \$n=int(\$r*60); printf(\"%s%s (%d/%d)\\r\", '#'x\$n, ' 'x(60-\$n), $x, $2);"
}

use_mife=1
use_existing=""
verbose=""
mmap_secparam=""
fail=""
gc_secparam=""
gc_padding=""

usage () {
    echo "gc-bench.sh: benchmark our scheme"
    echo "Usage: $0 [options] CIRCUIT"
    echo "  -t          testing mode: no MIFE"
    echo "  -l NUM      mmap security parameter for CLT (none implies dummy mmap)"
    echo "  -v          verbose mode"
    echo "  -f          exit if a test fails"
    echo "  -e          use exising garbler circuit"
    echo "  -s NUM      security param for garbler (wire size)"
    echo "  -p NUM      padding size for garbler"
    exit $1
}

while getopts "tl:vfhes:p:" opt; do
    case $opt in
        t) use_mife="";;
        l) mmap_secparam=$OPTARG;;
        v) verbose="-v";;
        f) fail=1;;
        e) use_existing=1;;
        s) gc_secparam="-s $OPTARG";;
        s) gc_padding="-p $OPTARG";;
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
        test_inp+=("$inp")
        test_out+=("$out")
    fi
done < $circuit
ntests=${#test_inp[@]}

################################################################################
## protocol

function unary() {
    perl -E "say '0'x$1, '1', '0'x$(( $2 - $1 - 1))"
}

if [[ $mmap_secparam ]]; then
    mmap="--mmap CLT"
    secparam_arg="--secparam $mmap_secparam"
else
    mmap="--mmap DUMMY"
    secparam_arg=""
fi

SECONDS=0
if [[ ! $use_existing ]]; then 
    rm -rf obf
    eval "./boots garble $circuit $gc_secparam $gc_padding"
fi

dir=$(readlink -f obf)
gb=$(readlink -f obf/gb.acirc2)

# set up mife and generate indices
if [[ $use_mife ]]; then
    echo "setting up MIFE"
    mio mife setup $mmap $secparam_arg $gb

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
        rm -f $dir/gates
        progress 0 $index_len >/dev/stderr
        cp $gb.1.ct.ix0 $gb.1.ct
        mio mife decrypt $mmap $gb | perl -nE 'say ((split)[1])' >> $dir/gates
        for (( i=1; i < $index_len; i++ )); do
            progress $i $index_len >/dev/stderr
            cp $gb.1.ct.ix$i $gb.1.ct
            mio mife decrypt --saved $mmap $gb | perl -nE 'say ((split)[1])' >> $dir/gates
        done
        echo >/dev/stderr
    else
        ./boots test
    fi
    ./boots eval 
}

enc_times=()
dec_times=()

for (( i=0; i<${ntests}; i++)); do
    echo "test $((i+1))/$ntests: ${test_inp[$i]} -> ${test_out[$i]}"

    SECONDS=0
    encrypt ${test_inp[$i]}
    enc_times+=($SECONDS)

    SECONDS=0
    if ! res=$(decrypt); then
        echo $res
        echo "decryption failed, skipping"
        [[ $fail ]] && exit 1
        continue
    fi
    dec_times+=($SECONDS)

    if [[ $res = "${test_out[$i]}" ]]; then
        echo ok
    else
        echo "FAILED (got $res)"
        [[ $fail ]] && exit 1
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
    for f in "$@"; do
        total=$(( total + $(filesize $f) ))
    done
    echo $total
}

keysize=$(sizes $dir/*.circ $dir/*.acirc2 $dir/*.1.ct.ix* $dir/*.ek)
ctsize=$(sizes $dir/wires $dir/*.0.ct)

echo
echo "setup time:      $setup_time s"
echo "enc time (avg):  $avg_enc_time s"
echo "dec time (avg):  $avg_dec_time s"
echo "key size:        $((keysize/1024)) kb"
echo "ciphertext size: $((ctsize/1024)) kb"
