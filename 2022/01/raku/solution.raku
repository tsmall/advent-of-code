use v6;

sub MAIN() {
    my @sums = [];

    my $sum = 0;
    for $*IN.lines {
        $sum += +$_;
        if !$_ {
            @sums.push($sum);
            $sum = 0;
        }
    }
    @sums.push($sum);

    say "Part 1: @sums.max()";
    say "Part 2: @sums.sort().tail(3).sum()";
}
