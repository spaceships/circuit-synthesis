#!/bin/bash

set -ex

scriptdir=$(dirname $(readlink -f "${BASH_SOURCE[0]}"))
rootdir=$(readlink -f "$scriptdir/..")
cryptoldir=$(readlink -f "$rootdir/cryptol")

pushd $rootdir

function add_tests() {
    cabal run --verbose=0 -- -A $1
}

function generate_circuit() {
    type_of_compilation=$1
    source_file=$2
    func_name=$3

    case $type_of_compilation in
        C2A)
            result_file=$func_name.c2a.acirc
            $scriptdir/c2a $source_file $func_name > $result_file
            ;;

        C2V)
            result_file=$3.c2v.acirc
            $scriptdir/c2v $source_file $func_name > $result_file
            ;;

        *)
            echo "[error] unrecognized compilation mode:" $type_of_compilation
            exit 1
            ;;
    esac
    add_tests $result_file
}

# generate AES circuits
for ty in C2A C2V; do
    for f in aes1r b0 b0_64 b0_32 b0_16 b0_8 b0_7 b0_6 b0_5 b0_4 b0_3 b0_2 sbox_ linearParts; do
        generate_circuit $ty $cryptoldir/AES.cry $f
    done
done
cabal run --verbose=0 -- -C aes #requires linearParts

# generate goldreich prg circuits
for ty in C2A C2V; do
    for f in prg_16_16 prg_16_32 prg_16_48 prg_16_64 prg_32_32 prg_32_64 prg_32_96; do
        generate_circuit $ty $cryptoldir/goldreich.cry $f
    done
done
cabal run --verbose=0 -- -C goldreich

# generate GGM circuits
cabal run --verbose=0 -- -C ggm

# generate Applebaum-Raykov circuits
for ty in C2A C2V; do
    for f in f1_16 f1_32 f1_64 f1_128 f3_4; do
        generate_circuit $ty $cryptoldir/applebaum.cry $f
    done
done

for ty in C2A C2V; do
    for f in mapper_2 mapper_4 mapper_8; do
        generate_circuit $ty $cryptoldir/mapper.cry $f
    done
done

# ensure the mapper exists for f3_4
if [ ! -e mappers/mapper_8 ]; then
    mkdir -p mappers
    generate_circuit C2V $cryptoldir/mapper.cry mapper_8
    mv mapper_8.c2v.acirc mappers
fi

cabal run --verbose=0 -- -C applebaum

# generate tribes circuits
cabal run --verbose=0 -- -C tribes

# generate mimc circuits
for ty in C2A C2V; do
    for f in mimc_16_10r mimc_16_5r mimc_16_1r mimc_8_5r mimc_8_1r; do
        generate_circuit $ty $cryptoldir/mimc.cry $f
    done
done

# package everything
tmpdir=$(mktemp -d)
mkdir $tmpdir/other $tmpdir/sigma
mv *.acirc $tmpdir
mv $tmpdir/*sigma*acirc $tmpdir/sigma
mv $tmpdir/*mimc*acirc $tmpdir/other

popd

tar --transform="s|$tmpdir|circuits|" -Pczf $PWD/circuits.tgz $tmpdir
