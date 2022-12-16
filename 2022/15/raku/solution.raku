use v6;

sub MAIN(Int $row, Int $search-range) {
    my @covered-ranges;
    my $beacon-xs = SetHash.new;

    my Int $sensor-x; my Int $sensor-y;
    my Int $beacon-x; my Int $beacon-y;
    for $*IN.lines -> $line {
        parse($line, $sensor-x, $sensor-y, $beacon-x, $beacon-y);

        my $distance = manhattan-distance($sensor-x, $sensor-y, $beacon-x, $beacon-y);

        $beacon-xs.set($beacon-x) if $beacon-y == $row;

        my $distance-from-row = abs($sensor-y - $row);
        if $distance-from-row <= $distance {
            my $remainder = abs($distance-from-row - $distance);
            my $range = ($sensor-x - $remainder) .. ($sensor-x + $remainder);
            add-range(@covered-ranges, $range);
        }
    }

    my $non-present-count = ([+] @covered-ranges».elems) - $beacon-xs.elems;

    say "Part 1: $non-present-count";
    say "Part 2: TODO";
}

sub add-range(@ranges, $new-range) {
    for ^∞ Z @ranges -> ($i, $range) {
        # Ignore it if it's already covered by an existing range
        if ($new-range.min >= $range.min) && ($new-range.max <= $range.max) {
            return
        }

        # Replace existing range if this one covers it
        if ($new-range.min <= $range.min) && ($new-range.max >= $range.max) {
            @ranges[$i] = $new-range;
            return;
        }

        # Extend existing range if this one overlaps at the start
        if ($new-range.min <= $range.min) && ($new-range.max <= $range.max) {
            @ranges[$i] = $new-range.min .. $range.max;
            return;
        }

        # Extend existing range if this one overlaps at the end
        if ($new-range.min >= $range.min) && ($new-range.max >= $range.max) {
            @ranges[$i] = $range.min .. $new-range.max;
            return;
        }

    }

    # Otherwise, this is a new, independent range
    @ranges.push($new-range);
}

sub manhattan-distance($x1, $y1, $x2, $y2) {
    abs($x1 - $x2) + abs($y1 - $y2)
}

grammar Line {
    token TOP {
        'Sensor at x=' <sensor-x=.num> ', y=' <sensor-y=.num> ': '
        'closest beacon is at x=' <beacon-x=.num> ', y=' <beacon-y=.num>
    }

    token num    { <sign> <digits> }
    token sign   { '-'? }
    token digits { \d+ }
}

sub parse($line, $sensor-x is rw, $sensor-y is rw, $beacon-x is rw, $beacon-y is rw) {
    my $parsed = Line.parse($line);

    $sensor-x  = +$parsed<sensor-x><digits>;
    $sensor-x *= -1 if $parsed<sensor-x><sign> eq '-';

    $sensor-y  = +$parsed<sensor-y><digits>;
    $sensor-y *= -1 if $parsed<sensor-y><sign> eq '-';

    $beacon-x  = +$parsed<beacon-x><digits>;
    $beacon-x *= -1 if $parsed<beacon-x><sign> eq '-';

    $beacon-y  = +$parsed<beacon-y><digits>;
    $beacon-y *= -1 if $parsed<beacon-y><sign> eq '-';
}
