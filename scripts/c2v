#!/bin/bash
#
# Cryptol to acirc optimized by ABC
#

set -e

usage () {
    echo "C2V: Cryptol to acirc through Verilog"
    echo "Usage: $0 <cryptol-file> <function>"
    exit $1
}

if [ x$1 = x ] || [ x$2 = x ]; then
    usage 1
fi

cryptolfile=$(realpath $1)
func=$2

scriptdir=$(dirname $(readlink -f "${BASH_SOURCE[0]}"))
libertyfile=$scriptdir/../liberties/aig.lib

tmp=$(mktemp -d)
cd $tmp

cat > sawcmds.saw <<- EOM
m <- cryptol_load "$cryptolfile";
f <- cryptol_extract m "$func";
write_aig "${func}.aig" f;
EOM

cat > abccmds.abc <<- EOM
read "${func}.aig"
write_verilog "${func}.in.v"
EOM

cat > yosys.ys <<- EOM
read_verilog "${func}.in.v"
synth -run fine
techmap; opt -fast
dfflibmap -liberty ${libertyfile}
abc -liberty ${libertyfile}
write_verilog ${func}.out.v
EOM

sizes=($(egrep "^$func\\s*:\\s*" $cryptolfile | perl -ne \
    'if (/: \(\[(\d+)],\s*\[(\d+)\]\) ->/) { print "$1 $2"; } elsif (/: \[(\d+)\] ->/) { print "$1 0"; }' \
))
inputsize=${sizes[0]}
keysize=${sizes[1]}

saw sawcmds.saw > /dev/null
abc -f abccmds.abc > /dev/null
yosys yosys.ys > /dev/null

python $scriptdir/verilog2acirc.py $func.out.v $inputsize $keysize
