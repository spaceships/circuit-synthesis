#!/usr/bin/env bash

circsynth="cabal run --verbose=0 --"

usage () {
    echo "generate-circuits.sh: Generates arithmetic circuits for obfuscation"
    echo ""
    echo "Flags:"
    echo "  -e, --extra  Generate tribes and mimc circuits"
    echo "  -o, --opt    Generate optimized-DSL circuits"
    echo "  -h, --help   Print this info and exit"
    exit "$1"
}

_=$(getopt -o eoh --long extra,opt,help)
if [ $? != 0 ]; then
    echo "Error: failed parsing options"
    usage 1
fi

if ! hash tar 2>/dev/null; then
    echo "Error: tar needs to be installed"
    exit 1
fi

extra='n'
opt='n'

while true; do
    case "$1" in
        -e | --extra )
            extra='y'; shift ;;
        -o | --opt )
            opt='y'; shift ;;
        -h | --help )
            usage 0 ;;
        -- ) shift; break ;;
        *) break ;;
    esac
done

set -ex

scriptdir=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
rootdir=$(readlink -f "$scriptdir/..")
cryptoldir=$(readlink -f "$rootdir/cryptol")

pushd "$rootdir"

function add_tests() {
    $circsynth -A "$1"
}

function generate_circuit() {
    type_of_compilation=$1
    source_file=$2
    func_name=$3

    case $type_of_compilation in
        C2A)
            result_file=$func_name.c2a.acirc
            "$scriptdir"/c2a "$source_file" "$func_name" > "$result_file"
            ;;

        C2V)
            result_file=$func_name.c2v.acirc
            "$scriptdir"/c2v "$source_file" "$func_name" > "$result_file"
            ;;

        *)
            echo "[error] unrecognized compilation mode:" "$type_of_compilation"
            exit 1
            ;;
    esac
    add_tests "$result_file"
    # Rename files if appropriate
    if [[ $func_name == b0 ]]; then
        mv "$result_file" "${result_file/b0/aes1r_128_1}"
    elif [[ $func_name =~ "b0_" ]]; then
        nins=$(echo "$func_name" | cut -d'_' -f2)
        file=$(echo "$result_file" | cut -d'.' -f2-)
        target="aes1r_${nins}_1.${file}"
        mv "$result_file" "$target"
    elif [[ $func_name == "sbox_" ]]; then
        mv "$result_file" "${result_file/sbox_/sbox}"
    fi
}

#
# Generate AES circuits
#
for ty in C2A C2V; do
    for f in aes1r b0 b0_64 b0_32 b0_16 b0_8 b0_4 b0_2 sbox_ linearParts; do
        generate_circuit $ty "$cryptoldir"/AES.cry $f
    done
done
$circsynth -C aes # requires linearParts

#
# Generate Goldreich PRG circuits
#
$circsynth -C goldreich

#
# generate GGM circuits
#
$circsynth -C ggm
$circsynth -C ggmSigma

if [[ $extra == y ]]; then
    for ty in C2A C2V; do
        for f in prg_16_16 prg_16_32 prg_16_48 prg_16_64 prg_32_32 prg_32_64 prg_32_96; do
            generate_circuit $ty "$cryptoldir"/goldreich.cry $f
        done
    done

    #
    # Generate Applebaum-Raykov circuits
    #
    for ty in C2A C2V; do
        for f in f1_16 f1_32 f1_64 f1_128 f3_4; do
            generate_circuit $ty "$cryptoldir"/applebaum.cry $f
        done
        for f in mapper_2 mapper_4 mapper_8; do
            generate_circuit $ty "$cryptoldir"/mapper.cry $f
        done
    done
    # ensure the mapper exists for f3_4
    if [ ! -e mappers/mapper_8 ]; then
        mkdir -p mappers
        cp mapper_8.c2v.acirc mappers
    fi
    $circsynth -C applebaum

    #
    # Generate tribes circuits
    #
    $circsynth -C tribes

    #
    # Generate MIMC circuits
    #
    for ty in C2A C2V; do
        for f in mimc_16_10r mimc_16_5r mimc_16_1r mimc_8_5r mimc_8_1r; do
            generate_circuit $ty "$cryptoldir"/mimc.cry $f
        done
    done
fi

#
# Generate optimized-DSL circuits
#
if [[ $opt == y ]]; then
    for c in ./*.dsl.acirc; do
        _c=$(basename "$c")
        # run -O1 on everything
        $circsynth -O1 "$c" -o "${c/dsl/opt-1}"
        # for -O2, skip ones that take forever
        if [[ $_c =~ ^f \
           || $_c =~ ^ggm_(1|2|3|4) \
           || $_c =~ ^ggm_sigma_(1|2|3|4) ]]; then
            continue
        fi
        $circsynth -O2 "$c" -o "${c/dsl/opt-2}"
        # for -O3, skip all but the ones that'll actually finish
        if [[ $_c =~ ^aes1r_(2|4|8|16|32) \
           || $_c == "sbox" ]]; then
            $circsynth -O3 "$c" -o "${c/dsl/opt-3}"
        fi
    done
fi

#
# Package everything
#
tmpdir=$(mktemp -d)
mv ./*.acirc "$tmpdir"
mkdir "$tmpdir"/sigma
mv "$tmpdir"/*sigma*acirc "$tmpdir"/sigma 2>/dev/null || true
if [[ $extra == y ]]; then
    mkdir "$tmpdir"/other
    mv "$tmpdir"/*mimc*acirc "$tmpdir"/other 2>/dev/null || true
fi

popd

tar --transform="s|$tmpdir|circuits|" -Pczf "$PWD"/circuits.tgz "$tmpdir"
