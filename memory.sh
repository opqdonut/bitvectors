for TEST in s2 fd fdn fun fs fse d dn ds dse
do
    echo '#' $TEST;
    for siz in 64 128 256 512
    do
	./dist/build/Test/Test $TEST data/f32s1-${siz}k 200000 +RTS -hT >/dev/null
	BYTES=$(awk -f process.awk < Test.hp | tail -4 | head -1 | cut -d' ' -f2)
	ORIG=$(echo 1024 '*' $siz / 8 | bc)
	RATIO=$(echo 'scale=7;' ${BYTES} / $ORIG | bc)
	echo $siz $ORIG $BYTES $RATIO 
    done
done
