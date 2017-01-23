#!/bin/bash

set -e

function add_tests() {
    echo cabal run --verbose=0 -- -A $1
    cabal run --verbose=0 -- -A $1
}

function generate_circuit() {
    type_of_compilation=$1
    source_file=$2
    func_name=$3

    case $type_of_compilation in
        C2A)
            result_file=$func_name.c2a.acirc
            echo "./scripts/cryptol2acirc $source_file $func_name > $result_file"
            ./scripts/cryptol2acirc $source_file $func_name > $result_file
            ;;

        C2C)
            result_file=$3.c2c.acirc
            echo "./scripts/cryptol2circ $source_file $func_name > $result_file"
            ./scripts/cryptol2circ $source_file $func_name > $result_file
            ;;

        *)
            echo "[error] unrecognized compilation mode:" $type_of_compilation
            exit
            ;;
    esac
    add_tests $result_file
}

# generate AES circuits
for ty in C2A C2C; do
    for f in aes1r b0 b0_64 b0_32 b0_16 b0_8 b0_7 b0_6 b0_5 b0_4 b0_3 b0_2 sbox_ linearParts; do
        generate_circuit $ty cryptol/AES.cry $f
    done
done
echo cabal run --verbose=0 -- -C aes #requires linearParts
cabal run --verbose=0 -- -C aes #requires linearParts

# generate goldreich prg circuits
for ty in C2A C2C; do
    # for f in prg_16_16 prg_16_32 prg_16_48 prg_16_64 prg_32_32 prg_32_64 prg_32_96 prg_32_128; do
    for f in prg_16_16 prg_16_32 prg_16_48 prg_16_64 prg_32_32 prg_32_64 prg_32_96; do
        generate_circuit $ty cryptol/goldreich.cry $f
    done
done

echo cabal run --verbose=0 -- -C goldreich
cabal run --verbose=0 -- -C goldreich

# generate GGM circuits
echo cabal run --verbose=0 -- -C ggm
cabal run --verbose=0 -- -C ggm

# generate Applebaum-Raykov circuits
for ty in C2A C2C; do
    for f in f1_16 f1_32 f1_64 f1_128 f3_4; do
        generate_circuit $ty cryptol/applebaum.cry $f
    done
done

for ty in C2A C2C; do
    for f in mapper_2 mapper_4 mapper_8; do
        generate_circuit $ty cryptol/mapper.cry $f
    done
done

# ensure the mapper exists for f3_4
if [ ! -e mappers/mapper_8 ]; then
    mkdir -p mappers
    generate_circuit C2C cryptol/mapper.cry mapper_8
    mv mapper_8.c2c.acirc mappers
fi

echo cabal run --verbose=0 -- -C applebaum
cabal run --verbose=0 -- -C applebaum

# generate tribes circuits
echo cabal run --verbose=0 -- -C tribes
cabal run --verbose=0 -- -C tribes

# generate mimc circuits
for ty in C2A C2C; do
    for f in mimc_16_10r mimc_16_5r mimc_16_1r mimc_8_5r mimc_8_1r; do
        generate_circuit $ty cryptol/mimc.cry $f
    done
done

# package everything
rm -rf /tmp/circuits
mkdir -p /tmp/circuits
cp *.acirc /tmp/circuits
dir=$PWD
cd /tmp
tar czf $dir/circuits.tgz circuits
