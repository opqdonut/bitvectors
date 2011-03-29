/END_SAMPLE/ {print $2 " " sum}
{sum = sum + $2}
/BEGIN_SAMPLE/ {sum=0}
