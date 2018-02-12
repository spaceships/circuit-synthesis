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
verbose=1
mmap_secparam=""
fail=""
gc_secparam=""
padding=""
gates_per_index=1
info_only=""

usage () {
    echo "gc-bench.sh: benchmark our scheme"
    echo "Usage: $0 [options] CIRCUIT"
    echo "  -i          print info and quit"
    echo "  -t          testing mode: no MIFE"
    echo "  -l NUM      mmap security parameter for CLT (none implies dummy mmap)"
    echo "  -q          quiet mode"
    echo "  -f          exit if a test fails"
    echo "  -e          use exising garbler circuit"
    echo "  -s NUM      security param for garbler (wire size)"
    echo "  -p NUM      padding size for garbler"
    echo "  -g NUM      gates per index in garbler"
    exit $1
}

while getopts "itl:qfhes:p:g:" opt; do
    case $opt in
        i) info_only=1;;
        t) use_mife="";;
        l) mmap_secparam=$OPTARG;;
        q) verbose="";;
        f) fail=1;;
        e) use_existing=1;;
        s) gc_secparam="-s $OPTARG";;
        p) padding="-p $OPTARG";;
        g) gates_per_index=$OPTARG;;
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
## checks

if [[ $use_mife ]]; then
    echo "MIFE currently unsupported!!!!"
fi

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
    echo "creating garbler circuit"
    rm -rf obf
    eval "./boots garble -g $gates_per_index $circuit $gc_secparam $padding"
fi

dir=$(readlink -f obf)
gb=$(readlink -f obf/gb.acirc2)

total_inputs=$(grep :ninputs $dir/c.circ | perl -nE '/(\d+)/; say $1')
symlens=($(grep :symlens $dir/c.circ | perl -nE '/((:?\s+\d+)+)/; say $1'))

if [[ $verbose ]]; then
    echo "ninputs:" $total_inputs
    echo "symlens: ${symlens[@]}"
    echo "number of ands:" $(grep -ce "and" $dir/c.circ)
    echo "number of xors:" $(grep -ce "xor" $dir/c.circ)
    [[ $info_only ]] && exit 0
fi

# set up mife and generate indices
if [[ $use_mife ]]; then
    echo "setting up MIFE"
    mio mife setup $mmap $secparam_arg $gb

    index_len=$(grep -m1 ":symlens" $gb | perl -nE 'print $1 if /:symlens \d+ (\d+)/')
    if [[ $index_len ]]; then 
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
    [[ $verbose ]] && echo "encrypting $2 into slot $1"
    slot=$1
    inp=$2
    ./boots seed $1
    if [[ $use_mife ]]; then
        mio mife encrypt $mmap $gb $(< $dir/seed) 0 >/dev/stderr
    else
        echo $inp > "$dir/input$slot"
    fi
}

function decrypt() {
    [[ $verbose ]] && echo "decrypting" >/dev/stderr
    if [[ $use_mife ]]; then
        if [[ $index_len ]]; then 
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
            mio mife decrypt $mmap $gb | perl -nE 'say ((split)[1])' > $dir/gates
        fi
    else
        [[ $verbose ]] && echo "running boots test" >/dev/stderr
        cat $dir/input* | xargs ./boots test >/dev/stderr
    fi
    [[ $verbose ]] && echo "running boots eval" >/dev/stderr
    ./boots eval 
}

enc_times=()
dec_times=()

for (( i=0; i<${ntests}; i++)); do
    echo "test $((i+1))/$ntests: ${test_inp[$i]} -> ${test_out[$i]}"

    # determine what symbols the test inputs go to
    unpack_str=""
    for len in "${symlens[@]}"; do 
        unpack_str+="A$len"
    done
    input_syms=($(perl -nE "say for unpack '$unpack_str'" <<< ${test_inp[$i]}))

    SECONDS=0
    for (( sym=0; sym<${#symlens[@]}; sym++ )); do
        encrypt $sym ${input_syms[$sym]}
    done
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
