my $key = "";
my %table;

my $cnt = 0;
my $val = 0;

while (<STDIN>) {
    if (/^#/) {
    } elsif (/ /) {
        @vals = split(/ /);

        if ($cnt > 0 && $vals[0] ne $key) {
            my $res = $val/$cnt;
            $table{$key} .= " " . $res;
            $val = 0;
            $cnt = 0;
            #print "tab ", $vals[0], ": ", $table{$vals[0]}, "\n";
        }

        $val += ($vals[2] - $vals[1]);
        $cnt ++;
        $key = $vals[0];

        #print "val ", $val, "cnt ", $cnt, "\n";
    }
}

my $res = $val/$cnt;
$table{$key} .= " " . $res;

foreach $k (sort { $a <=> $b } keys(%table)) {
    print $k;
    my $s = $table{$k};
    print $s, "\n";
}

#print %table;
#print %counts;
