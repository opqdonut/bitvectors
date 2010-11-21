#!/bin/sh
SIZES="5000 10000 20000 40000 80000 160000 320000 640000 1280000 2560000 5120000"
QUERIES=100000

REPEATS=${1:-1}


echo '#' GIT: $(git log --oneline | head -1)
echo '#' REPEATS: $REPEATS
echo '#' QUERIES: $QUERIES
for COMMAND in "./test sg" "./test fd" "./test fdn" "./test fun"\
    "../impls/cst_v_1_0/mytest" "../impls/rlcsa/mytest";
do
    echo '#' $COMMAND " "
    for size in $SIZES; do
	for j in $(seq $REPEATS); do
	    FIRST=$(/usr/bin/time -f " %e" $COMMAND $size 100 2>&1 >/dev/null)
	    SECOND=$(/usr/bin/time -f " %e" $COMMAND $size $QUERIES 2>&1 >/dev/null)
	    echo $size $FIRST $SECOND
	done
    done
    echo; echo; echo  # gnuplot dataset break
done
