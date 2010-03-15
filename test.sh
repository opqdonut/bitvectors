#!/bin/sh
SIZE=$((1024*1024*2))

for i in 10 100000; do
    for COMMAND in "./test s" "./test d" "../cst_v_1_0/mytest"; do
	echo -n $COMMAND $SIZE $i " "
	/usr/bin/time -f "%e %U %s" $COMMAND $SIZE $i > /dev/null
    done
done
