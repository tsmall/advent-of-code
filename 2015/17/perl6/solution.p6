use v6;


sub combinations(Int $liters, @containers) {
    my @containers-used;

    # The possible combinations is
    # all of the combinations where you *do* use the first container
    # plus all of the combinations where you *don't*
    sub loop(Int $liters, @containers, @used = ()) {
        if $liters == 0 {
            @containers-used.push: @used;
            return;
        }
        if $liters < 0 or !@containers {
            return;
        }

        my $head = @containers.head;
        my $tail = @containers.skip;
        my @with = |@used, $head;
        my @without := @used;

        loop($liters - $head, $tail, @with);
        loop($liters,         $tail, @without);
    }

    loop($liters, @containers);
    return @containers-used;
}


sub part-one(Int $liters, @containers --> Int) {
    return combinations($liters, @containers).elems;
}


sub part-two(Int $liters, @containers --> Int) {
    my @combinations = combinations($liters, @containers);
    my $fewest-used  = @combinations.map({$_.elems}).min;
    return @combinations.grep({$_.elems == $fewest-used}).elems;
}


sub MAIN {
    # Note: This must be set to 25 for the example, 150 for the input.
    constant $liters = 150;

    my @containers = $*IN.lines;
    say '--- Part One ---';
    say part-one($liters, @containers);
    say '';
    say '--- Part Two ---';
    say part-two($liters, @containers);
}
