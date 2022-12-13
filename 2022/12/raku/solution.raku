use v6;

class Point {
    has $.y;
    has $.x;
    has $.char;
    has $.height;

    method WHICH { "Position|$.y|$.x" }
}

sub MAIN() {
    (my $start, my $heightmap) = parse-input();
    my @starts = find-lowest($heightmap);

    my $shortest-path-from-start = shortest-path($heightmap, $start);
    my @shortest-paths = @starts.hyper.map: {
        shortest-path($heightmap, $_, $shortest-path-from-start)
    };
    @shortest-paths.push($shortest-path-from-start);

    say "Part 1: $shortest-path-from-start";
    say "Part 2: @shortest-paths.min()";
}

sub parse-input() {
    my $start;
    my @heightmap;
    my $y = 0;
    for $*IN.lines -> $line {
        my $x = 0;
        my @row;
        for $line.comb -> $char {
            my $point = Point.new(:$y, :$x, :$char, height => height($char));
            $start = $point if $char eq 'S';
            @row.push($point);
            ++$x;
        }
        @heightmap.push(@row);
        ++$y;
    }
    return $start, @heightmap;
}

sub find-lowest(@heightmap) {
    @heightmap».grep(*.char eq 'a').flat;
}

sub shortest-path(@heightmap, $start, $known-min = Nil) {
    my @queue = [$start];
    my $steps = 0;

    my $visited = SetHash.new;
    $visited.set($start);

    MAIN:
    loop {
        my @new-queue;
        while @queue {
            my $point = @queue.shift;
            last MAIN if $point.char eq 'E';

            my $y = $point.y;
            my $x = $point.x;

            check-neighbor(@heightmap, $visited, @new-queue, $point, $y-1, $x);
            check-neighbor(@heightmap, $visited, @new-queue, $point, $y+1, $x);
            check-neighbor(@heightmap, $visited, @new-queue, $point, $y, $x-1);
            check-neighbor(@heightmap, $visited, @new-queue, $point, $y, $x+1);
        }

        ++$steps;
        @queue = @new-queue.unique;

        with $known-min {
            last MAIN if $steps > $known-min;
        }
    }

    return $steps;
}

sub check-neighbor(@heightmap, $visited, @queue, $point, $y, $x) {
    my $neighbor = @heightmap[$y][$x];
    return if !$neighbor;

    return if $neighbor ∈ $visited;

    if ($neighbor.height - $point.height) ≤ 1 {
        @queue.push($neighbor);
        $visited.set($neighbor);
    }
}

sub height($char) {
    given $char {
        when 'S' { 'a'.ord }
        when 'E' { 'z'.ord }
        default { $char.ord }
    }
}
