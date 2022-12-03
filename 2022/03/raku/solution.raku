use v6;

sub MAIN() {
    my $total-priority = 0;
    my $group-priority = 0;

    my $line-number = 0;
    my $group-duplicates = Nil;
    for $*IN.lines -> $line {
        my $middle = $line.chars / 2;
        my $head-rucksack = $line.substr(0, $middle);
        my $tail-rucksack = $line.substr($middle);
        my $duplicates = $head-rucksack.comb ∩ $tail-rucksack.comb;
        $total-priority += priority($duplicates);

        $group-duplicates = ?$group-duplicates
                              ?? $group-duplicates ∩ $line.comb
                              !! $line.comb;
        if ++$line-number %% 3 {
            $group-priority += priority($group-duplicates);
            $group-duplicates = Nil;
        }
    }

    say "Part 1: $total-priority";
    say "Part 2: $group-priority";
}

sub priority(Set $chars) {
    state $a-ord = 'a'.ord;
    state $A-ord = 'A'.ord;

    my $sum = 0;
    for $chars.keys {
        my $ord = $_.ord;
        $sum += ($ord >= $a-ord)
                    ?? $ord - $a-ord + 1
                    !! $ord - $A-ord + 1 + 26;
    }

    return $sum;
}
