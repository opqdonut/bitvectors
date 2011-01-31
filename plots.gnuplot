set key top left
set terminal latex
set log x

set output 'statics.tex';
plot\
     '2011-01-31-3.dat' i 1 u 1:($3-$2) t 's2',\
     '' i 8 u 1:($3-$2) t 'cst',\
     '' i 9 u 1:($3-$2) t 'rlcsa';

set output 'dynamic.tex';
plot '2011-01-31-3.dat' index 2 using 1:($3-$2) t 'elias',\
     '' i 3 u 1:($3-$2) t 'nibble',\
     '' i 4 u 1:($3-$2) t 'unencoded',\
     '' i 5 u 1:($3-$2) t 'smallblock',\
     '' i 6 u 1:($3-$2) t 't',\
     '' i 7 u 1:($3-$2) t 'ts',\
     '' i 10 u 1:($3-$2) t 'dynfmi';

set output 'dynamic2.tex';
plot '2011-01-31-3.dat' i 6 u 1:($3-$2) t 't',\
     '' i 7 u 1:($3-$2) t 'ts',\
     '' i 10 u 1:($3-$2) t 'dynfmi';
