#!/bin/sh
SIZE=$((1024*1024*2))

for i in 10 50 100 500 1000; do
    for x in s d; do
	echo -n $x $SIZE $i " "
	/usr/bin/time -f "%e %U %s" ./test $RUN $x $SIZE $i > /dev/null
    done
done
