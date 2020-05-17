enum Part (Part1 => 3, Part2 => 4);

sub MAIN(Part $part) {
    my Int @weights      = $*IN.lines».Int;
    my Int $group-count  = $part.value;
    my Int $group-weight = @weights.sum div $group-count;

    my @group-ones;
    {
        my $size = 2;
        until @group-ones {
            for @weights.combinations($size) -> @group {
                if @group.sum == $group-weight {
                    push @group-ones, @group;
                }
            }
            $size++;
        }
    }

    my @entanglements = @group-ones.map: { quantum-entanglement($_) };

    say "--- {$part} ---";
    say @entanglements.min;
}

sub quantum-entanglement(@weights --> Int) {
    return [*] @weights;
}
