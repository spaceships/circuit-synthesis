#!/bin/bash
#
# Cryptol to acirc
#

set -e

usage () {
    echo "C2A: Cryptol to acirc through AND-inverter graphs"
    echo "Usage: $0 <cryptol-file> <function>"
    exit $1
}

if [ x$1 = x ] || [ x$2 = x ]; then
    usage 1
fi

num_test_cases=10

scriptdir=$(dirname $(readlink -f "${BASH_SOURCE[0]}"))

cryptolfile=$(realpath $1)
func=$2
testfile=${2}_tests.cry

tmp=$(mktemp -d)
cd $tmp

cat > sawcmds.saw <<- EOM
m <- cryptol_load "$cryptolfile";
f <- cryptol_extract m "$func";
write_aig "${func}.aig" f;
EOM

cat > abccmds.abc <<- EOM
read "${func}.aig"
strash
multi
fraig
rewrite -l
balance -s -d -x
write "${func}.bench"
EOM

sizes=($(egrep "^$func\\s*:\\s*" $cryptolfile | perl -ne \
    'if (/: \(\[(\d+)],\s*\[(\d+)\]\) ->/) { print "$1 $2"; } elsif (/: \[(\d+)\] ->/) { print "$1 0"; }' \
))
xsize=${sizes[0]}
ysize=${sizes[1]}

saw sawcmds.saw > /dev/null
abc -f abccmds.abc > /dev/null
perl $scriptdir/bench2circ.pl $func.bench ${3:-arithmetic-circuit} $xsize $ysize
