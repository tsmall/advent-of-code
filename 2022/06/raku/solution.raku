use v6;

my \packet-window-size := 4;
my \message-window-size := 14;

sub MAIN() {
    my @chars = $*IN.slurp.comb;

    my $packet-count = read-until-no-duplicates(@chars, packet-window-size);
    my $message-count = read-until-no-duplicates(@chars, message-window-size);

    say "Part 1: $packet-count";
    say "Part 2: $message-count";
}

sub read-until-no-duplicates(@chars, $window-size) {
    my $index = 0;

    MAIN-LOOP:
    while ($index < @chars.elems) {
        loop (my $i = $index + $window-size - 1; $i > $index; --$i) {
            my $last-duplicate-index = Nil;
            loop (my $j = $index; $j < $i; ++$j) {
                if @chars[$i] eq @chars[$j] {
                    $last-duplicate-index = $j;
                }
            }
            if $last-duplicate-index {
                $index = $last-duplicate-index + 1;
                next MAIN-LOOP;
            }
        }

        # No duplicates, so we're done.
        last;
    }

    my $result = $index + $window-size;
    return $result;
}
