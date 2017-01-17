set -e

num_test_cases=10

cryptolfile=$(realpath $1)
func=$2
testfile=${2}_tests.cry

cd work

function make_test_cases {
    echo ":l $cryptolfile" > $testfile;
    echo ":set base=2" >> $testfile;
    for i in $(seq 0 $(($num_test_cases-1))); do
        x=$(od -vAn -N$(($1 / 8)) -tu8 < /dev/urandom);
        x=$(echo $x | perl -ne '@s = split; print join(", ", @s)'); # replace spaces with comma
        x=$(echo $x | perl -pe "s/^0+//");   # remove leading zeroes
        echo "let x$i = join [ $x ] : [$1]" >> $testfile;
        echo "x$i" >> $testfile;
        echo "$func (x$i, zero)" >> $testfile;
    done
    outputs=($(cryptol -b $testfile | tail -n $(($num_test_cases * 2)) | perl -pe 's/0b//g'));
    j=0
    for i in $(seq 0 2 $(($num_test_cases * 2 - 1))); do
        tests[$j]=$(echo ${outputs[$i]} | rev);
        results[$j]=${outputs[$(($i + 1))]};
        j=$(($j + 1));
    done
    for i in $(seq 0 $(($num_test_cases-1))); do
        echo -n ":test ${tests[$i]} ";
        if [ ${results[$i]} = "True" ]; then
            echo 1;
        else
            echo 0;
        fi
    done
}

sizes=($(egrep "$func\\s*:\\s*" $cryptolfile | perl -ne '/\(\[(\d+)],\s*\[(\d+)\]\)/; print "$1 $2";'))
xsize=${sizes[0]}
ysize=${sizes[1]}

if [ $[$xsize % 8] -eq 0 ]
then
    make_test_cases $xsize
else
    echo "can only generate tests for functions whose inputs have length some multiple of 8"
fi
