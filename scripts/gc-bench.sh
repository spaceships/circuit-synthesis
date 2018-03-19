#!/bin/bash

set -e

function progress() {
    x=$(( $1+1 ))
    perl -E "\$r=$x/$2; \$n=int(\$r*40); printf(\"        %s%s (%d/%d)\\r\", '#'x\$n, ' 'x(40-\$n), $x, $2);"
}

mio=../circ-obfuscation/mio.sh
use_mife=1
use_existing=""
verbose=1
mmap_secparam=""
fail=""
gc_secparam=40
padding=8
gates_per_index=1
info_only=""
max_tests=""
dir="obf"
progress=1
no_index=""
garbler=""

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
    echo "  -N          do not use indexing"
    echo "  -n NUM      only evaluate NUM tests"
    echo "  -d DIR      use directory DIR for saving files"
    echo "  -P          no progress bars"
    echo "  -m FILE     location of mio.sh"
    echo "  -a APP      use APP as alternate boots garbler"
    exit $1
}

while getopts "itl:qfhes:p:g:n:d:Pm:Na:" opt; do
    case $opt in
        i) info_only=1;;
        t) use_mife="";;
        l) mmap_secparam=$OPTARG;;
        q) verbose="";;
        f) fail=1;;
        e) use_existing=1;;
        s) gc_secparam=$OPTARG;;
        p) padding=$OPTARG;;
        g) gates_per_index=$OPTARG;;
        n) max_tests=$OPTARG;;
        d) dir=$OPTARG;;
        P) progress="";;
        m) mio=$OPTARG;;
        N) no_index=1;;
        a) garbler=$OPTARG;;
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
if [[ $no_index ]]; then
    gates_per_index=$(wc -l $circuit | cut -d' ' -f1)
fi

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
    echo "creating garbler circuit..."
    if [[ $garbler ]]; then
        $garbler $circuit $gc_secparam $padding $dir
        touch $dir/naive
        echo "GarblerParams {securityParam = $gc_secparam, paddingSize = $padding, numIndices = 1, gatesPerIndex = $gates_per_index}" > $dir/params
        cp $circuit $dir/c.acirc2
    else
        ./boots garble -i -d $dir -g $gates_per_index -s $gc_secparam -p $padding $circuit
    fi
    echo "${SECONDS}s"
fi

gb=$(readlink -f $dir/gb.acirc2)
wires_gen=$(readlink -f $dir/wires-gen.acirc2)

total_inputs=$(grep :ninputs $circuit | perl -nE '/(\d+)/; say $1')
symlens=($(grep :symlens $circuit | perl -nE '/((:?\s+\d+)+)/; say $1'))
nsyms=${#symlens[@]}

if [[ $verbose ]]; then
    echo
    echo "c.circ stats:"
    echo -e "\tninputs:" $total_inputs
    echo -e "\tsymlens: ${symlens[@]}"
    echo -e "\tnsyms: $nsyms"
    echo -e "\tnumber of ands:" $(grep -ce "and\|mul" $circuit)
    echo -e "\tnumber of xors:" $(grep -ce "xor\|add" $circuit)
    echo
    [[ $info_only ]] && exit 0
fi

# set up mife and generate indices
if [[ $use_mife ]]; then
    echo "setting up MIFE"
    setup_start=$SECONDS
    $mio mife setup --npowers 5 $mmap $secparam_arg $gb
    $mio mife setup --npowers 5 $mmap $secparam_arg $wires_gen

    # possibly encrypt indices
    if [[ ! -f "$dir/naive" ]]; then 
        index_len=$(grep -m1 ":symlens" $gb | perl -nE 'print $1 if /.* (\d+)$/')
        echo -n "encrypting indices ($index_len):"
        for (( i=0; i < $index_len; i++ )); do
            echo -n " $i"
            ix=$(unary $i $index_len)
            $mio mife encrypt $mmap $gb $ix $nsyms >/dev/stderr
            mv $gb.$nsyms.ct $gb.$nsyms.ct.ix$i
        done
    fi
    echo " ($((SECONDS-setup_start))s)"
fi
setup_time=$SECONDS

function encrypt() {
    [[ $verbose ]] && echo -n "encrypting $2 into slot $1..."
    encrypt_start=$SECONDS
    slot=$1
    inp=$2
    ./boots seed $slot -d $dir
    if [[ $use_mife ]]; then
        $mio mife encrypt $mmap $gb $(< $dir/seed$slot) $slot
        $mio mife encrypt $mmap $wires_gen $(< $dir/seed$slot) $((2*slot))
        $mio mife encrypt $mmap $wires_gen $inp $((2*slot+1))
    else
        echo $inp > "$dir/input$slot"
    fi
    echo "$((SECONDS-encrypt_start))s"
}

function decrypt() {
    [[ $verbose ]] && echo "decrypting" >/dev/stderr
    if [[ $use_mife ]]; then
        # gen gates
        [[ $verbose ]] && echo -e "\trunning gen gates..."
        gates_start=$SECONDS
        if [[ -f $dir/naive ]]; then 
            $mio mife decrypt $mmap $gb | perl -nE 'say ((split)[1])' > $dir/gates
        else
            rm -f $dir/gates
            [[ $progress ]] && progress 0 $index_len
            cp $gb.$nsyms.ct.ix0 $gb.$nsyms.ct
            $mio mife decrypt $mmap $gb | perl -nE 'say ((split)[1])' >> $dir/gates
            for (( ix=1; ix < $index_len; ix++ )); do
                [[ $progress ]] && progress $ix $index_len
                cp $gb.$nsyms.ct.ix$ix $gb.$nsyms.ct
                $mio mife decrypt --saved $mmap $gb | perl -nE 'say ((split)[1])' >> $dir/gates
            done
            echo
        fi
        [[ $verbose ]] && echo -e "\t($(( SECONDS - gates_start ))s)"

        [[ $verbose ]] && echo -ne "\trunning gen wires..."
        wires_start=$SECONDS
        $mio mife decrypt $mmap $wires_gen | 
            perl -nE "say for unpack '(A$gc_secparam)*', ((split)[1])" > $dir/wires
        [[ $verbose ]] && echo "$(( SECONDS - wires_start ))s"
    else
        [[ $verbose ]] && echo -e "\trunning boots test"
        cat $dir/input* | xargs ./boots test -d $dir
    fi
    
    eval_start=$SECONDS
    [[ $verbose ]] && echo -ne "\trunning boots eval..."
    ./boots eval -d $dir > $dir/result || cat $dir/result 
    [[ $verbose ]] && echo "$(( SECONDS - eval_start ))s"
}

enc_times=()
dec_times=()

if [[ $max_tests ]]; then
    test_cases=$max_tests
else
    test_cases=$ntests
fi
for (( i = 0; i < $test_cases; i++)); do
    echo
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
    decrypt
    dec_times+=($SECONDS)
    res=$(cat $dir/result)
    if [[ $res = "${test_out[$i]}" ]]; then
        echo "ok (${SECONDS}s)"
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

eksize=$(sizes $dir/*.circ $dir/*.acirc2 $dir/*.ek)
if [[ ! -f $dir/naive ]]; then 
    eksize=$(( eksize + $(sizes $dir/*.$nsyms.ct.ix*) ))
fi
sksize=$(sizes $dir/*.sk)
ctsize=$(sizes $dir/gb.acirc2.0.ct $dir/wires-gen.acirc2.0.ct $dir/wires-gen.acirc2.1.ct)

echo
echo "setup time:      $setup_time s"
echo "enc time (avg):  $avg_enc_time s"
echo "dec time (avg):  $avg_dec_time s"
echo "eval key size:   $((eksize/1024)) kb"
echo "ciphertext size: $((ctsize/1024)) kb"
echo "gb kappa:        $($mio mife get-kappa $gb)"
echo "wires-gen kappa: $($mio mife get-kappa $wires_gen)"
