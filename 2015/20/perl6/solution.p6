constant $input = 29_000_000;
constant $goal  = $input div 10;

sub MAIN {
    my $p1 = start { part-one(1) };
    my $p2 = start { part-one(3) };
    my $p3 = start { part-one(5) };
    my $p4 = start { part-one(7) };
    my $p5 = start { part-one(9) };

    my Int @results = await $p1, $p2, $p3, $p4, $p5;

    say '--- Part One ---';
    say @results.min;
}

sub part-one(Int $start --> Int) {
    my Int $answer;

    my Int $sum;
    my Int $base   = 0;
    my Int $offset = 0;
    until $answer.defined {
        $sum = divsum($base + $start + $offset);
        if $sum >= $goal {
            $answer = $base + $start + $offset;
        }

        if $offset == 0 {
            $offset++;
        }
        else {
            $offset = 0;
            $base += 10;
        }
    }

    say "DEBUG: thread={$start} answer={$answer}";
    return $answer;
}

sub divsum(Int $n --> Int) {
    my $seen = SetHash.new;
    my $sum  = $n;

    $seen{$n}++;

    for 1 .. $n.sqrt.round -> $i {
        if $n %% $i {
            if $i ∉ $seen {
                $sum += $i;
                $seen{$i}++;
            }
            if ($n div $i) ∉ $seen {
                $sum += $n div $i;
                $seen{$n div $i}++;
            }
        }
    }

    return $sum;
}
