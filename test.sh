#!/bin/bash
SIZES="1 2 4 8 16 32 64 128 256 512"
QUERIES=${QUERIES:-100000}

REPEATS=${1:-1}

function test {
    for size in $SIZES; do
	for j in $(seq $REPEATS); do
            file="data/f32s1-${size}k"
	    FIRST=$(/usr/bin/time -f " %U" $1 $file 100 2>&1 >/dev/null)
	    SECOND=$(/usr/bin/time -f " %U" $1 $file $QUERIES 2>&1 >/dev/null)
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
   "$TEST fs" "$TEST fse" "$TEST d" "$TEST sd" "$TEST sed"\
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
