#!/bin/sh
SIZE=$((1024*1024*2))

echo $COMMAND 10 100000
for COMMAND in "./test so" "./test sg" "./test fd" "../impls/cst_v_1_0/mytest" \
    "../impls/rlcsa/mytest"; do
    echo $COMMAND " "
    for i in 10 100000; do
	/usr/bin/time -f " %e" $COMMAND $SIZE $i > /dev/null
    done
done
