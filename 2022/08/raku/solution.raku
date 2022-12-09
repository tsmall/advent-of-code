use v6;

sub MAIN() {
    my @grid  := parse-grid();
    my $width := @grid.elems;

    my $visible-count = perimeter($width);
    my $highest-scenic-score = 0;
    for 1 ..^ ($width - 1) -> $y {
        for 1 ..^ ($width - 1) -> $x {
            (my $is-visible, my $scenic-score) = analyze(@grid, $y, $x);
            $visible-count += $is-visible;
            $highest-scenic-score max= $scenic-score;
        }
    }

    say "Part 1: $visible-count";
    say "Part 2: $highest-scenic-score";
}

sub parse-grid() {
    ($*IN.lines.map: *.comb.List).List
}

sub perimeter(Int $width) {
    $width + (2 * ($width - 1)) + ($width - 2)
}

# NOTE: There are more elagant solutions to this, but this is the one that
#       finishes the quickest. (I'm trying to keep the time under a second.)
sub analyze(@grid, $y, $x) {
    my $grid-width := @grid.elems;
    my $tree-height := @grid[$y][$x];

    my $is-visible = False;
    my @scores = [];

    # Looking left ...
    {
        my $trees-visible = 0;
        my $is-blocked = False;
        loop (my $xx = $x - 1; $xx >= 0; --$xx) {
            ++$trees-visible;
            if @grid[$y][$xx] >= $tree-height {
                $is-blocked = True;
                last;
            }
        }
        $is-visible ||= !$is-blocked;
        @scores.push($trees-visible);
    }

    # Looking right ...
    {
        my $trees-visible = 0;
        my $is-blocked = False;
        loop (my $xx = $x + 1; $xx < $grid-width; ++$xx) {
            ++$trees-visible;
            if @grid[$y][$xx] >= $tree-height {
                $is-blocked = True;
                last;
            }
        }
        $is-visible ||= !$is-blocked;
        @scores.push($trees-visible);
    }

    # Looking up ...
    {
        my $trees-visible = 0;
        my $is-blocked = False;
        loop (my $yy = $y - 1; $yy >= 0; --$yy) {
            ++$trees-visible;
            if @grid[$yy][$x] >= $tree-height {
                $is-blocked = True;
                last;
            }
        }
        $is-visible ||= !$is-blocked;
        @scores.push($trees-visible);
    }

    # Looking down ...
    {
        my $trees-visible = 0;
        my $is-blocked = False;
        loop (my $yy = $y + 1; $yy < $grid-width; ++$yy) {
            ++$trees-visible;
            if @grid[$yy][$x] >= $tree-height {
                $is-blocked = True;
                last;
            }
        }
        $is-visible ||= !$is-blocked;
        @scores.push($trees-visible);
    }

    my $final-score = [*] @scores;
    return $is-visible, $final-score;
}
