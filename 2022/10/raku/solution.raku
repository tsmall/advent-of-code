use v6;

sub MAIN() {
    my $signal-sum = 0;

    my $cycle = 0;
    my $x = 1;
    my $executing = Nil;
    while $cycle < 240 {
        ++$cycle;

        $signal-sum += ($x * $cycle) if $cycle == 20|60|100|140|180|220;

        my $pixel := ($cycle - 1) % 40;
        print "\n" if $pixel == 0;
        print (($x-1) <= $pixel <= ($x+1)) ?? '#' !! '.';

        with $executing {
            $x += $executing;
            $executing = Nil;
        } else {
            $executing = next-instruction();
        }
    }

    print("\n\n");
    say "Part 1: $signal-sum";
    say "Part 2: (see above)";
}

sub next-instruction() {
    state $line-iterator = $*IN.lines.iterator;

    given $line-iterator.pull-one {
        when $_ =:= IterationEnd  { return Nil; }
        when .starts-with('addx') { return +$_.substr(5); }
        default                   { return Nil; }
    }
}
