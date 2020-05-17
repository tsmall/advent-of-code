sub MAIN {
    my Int @weights      = $*IN.lines».Int;
    my Int $group-weight = @weights.sum div 3;

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

    say '--- Part1 ---';
    say @entanglements.min;
}

sub quantum-entanglement(@weights --> Int) {
    return [*] @weights;
}
