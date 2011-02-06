#!/bin/sh
SIZES="5000 10000 20000 40000 80000 160000 320000 640000 1280000 2560000 5120000"
QUERIES=100000

REPEATS=${1:-1}

function test {
    for size in $SIZES; do
	for j in $(seq $REPEATS); do
	    FIRST=$(/usr/bin/time -f " %U" $1 $size 100 2>&1 >/dev/null)
	    SECOND=$(/usr/bin/time -f " %U" $1 $size $QUERIES 2>&1 >/dev/null)
	    echo $size $FIRST $SECOND
	done
    done
}

TEST=dist/build/Test/Test

function testall {
    echo '#' GIT: $(git log --oneline | head -1)
    echo '#' REPEATS: $REPEATS
    echo '#' QUERIES: $QUERIES
    for COMMAND in "$TEST sg" "$TEST s2" "$TEST fd" "$TEST fdn" "$TEST fun"\
   "$TEST fs" "$TEST d" "$TEST sd" "$TEST sed"\
   "../impls/cst_v_1_0/mytest" "../impls/rlcsa/mytest" "../impls/gerlach/dynfmi/mytest";
    do
	echo '#' $COMMAND " "
	test "$COMMAND";
	echo; echo; echo  # gnuplot dataset break
    done
}

if [[ $2 ]]; then
    test "$2"
else
    testall
fi
