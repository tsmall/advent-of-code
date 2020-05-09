constant $input = 29_000_000;
constant $goal  = $input div 10;

sub MAIN {
    say '--- Part One ---';
    say part-one;
}

sub part-one(--> Int) {
    my Int @sums[$goal] = 0 xx $goal;
    my Int $housenum = $goal;

    for 1 .. $goal -> $i {
        loop (my $j = $i; $j < $goal; $j += $i) {
            @sums[$j] += $i;
            $housenum = $j if $j < $housenum and @sums[$j] >= $goal;
        }
    }

    return $housenum;
}
