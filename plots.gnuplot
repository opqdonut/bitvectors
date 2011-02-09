set key top left
set terminal latex
set log x

set xlabel 'kbits'
set ylabel 't/s'

set output 'statics.tex';
plot\
     '2011-02-09-2.dat' i 1 u 1:($3-$2) t 's2',\
     '' i 10 u 1:($3-$2) t 'cst',\
     '' i 11 u 1:($3-$2) t 'rlcsa';

set output 'dynamic.tex';
plot '2011-02-09-2.dat' index 2 using 1:($3-$2) t 'elias',\
     '' i 3 u 1:($3-$2) t 'nibble',\
     '' i 5 u 1:($3-$2) t 'smallblock',\
     '' i 6 u 1:($3-$2) t 'smallelias',\
     '' i 12 u 1:($3-$2) t 'dynfmi';

set output 'dynamic2.tex';
plot '2011-02-09-2.dat' i 8 u 1:($3-$2) t 'smallblock',\
     '' i 9 u 1:($3-$2) t 'smallelias',\
     '' i 12 u 1:($3-$2) t 'dynfmi';
