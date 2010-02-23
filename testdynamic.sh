N=500

for i in $(seq 100 100 1000); do
    SIZE=$((1024*$i))
    echo -n $SIZE " "
    /usr/bin/time -f "%e %U %s" ./test $RUN d $SIZE $N > /dev/null
done
