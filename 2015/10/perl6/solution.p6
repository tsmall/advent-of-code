use v6;


my $input := 1113122113;


sub look-and-say($number) {
    my @runs = $number.comb: /(.) $0*/;
    my @reading = gather {
        for @runs -> $run {
            take $run.chars;
            take $run.comb(1).head;
        }
    }
    return @reading.join;
}


my $reading = $input;


for 1..40 {
    $reading = look-and-say($reading);
}

say '--- Part One ---';
say $reading.chars;
say '';


for 1..10 {
    $reading = look-and-say($reading);
}

say '--- Part Two ---';
say $reading.chars;
