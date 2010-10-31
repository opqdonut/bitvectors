#!/bin/sh
SIZES="1000 10000 100000 1000000 10000000"
QUERIES=100000

REPEATS=${1:-1}


echo GIT: $(git log --oneline | head -1)
echo REPEATS: $REPEATS
echo LENGTHS: $COMMANDS 
for COMMAND in "./test sg" "./test fd" "../impls/cst_v_1_0/mytest" \
    "../impls/rlcsa/mytest"; do
    echo $COMMAND " "
    for size in $SIZES; do
	for j in $(seq $REPEATS); do
	    echo -n $size
	    /usr/bin/time -f " %e" $COMMAND $size $QUERIES > /dev/null
	done
    done
done
