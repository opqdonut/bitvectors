/BEGIN_SAMPLE/ {sum=0}
{sum = sum + $2}
/END_SAMPLE/ {print $2 " " sum}
