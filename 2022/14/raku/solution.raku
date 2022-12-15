use v6;

class Point {
    has Int $.x is rw;
    has Int $.y is rw;

    method WHICH { "Point|$.x|$.y" }
}

constant $origin-point = Point.new(:x(500), :y(0));

sub MAIN {
    my $paths = parse-input();
    my $rock-points = expand-path-points($paths);
    my $sand-points = SetHash.new;
    my $floor-y = max(|$rock-points.keys».y) + 2;

    my $part1-sand-count;
    loop {
        my $sand-point = simulate-sand-drop($rock-points, $sand-points, $floor-y);
        $sand-points.set($sand-point);

        last if $sand-point eqv $origin-point;

        if $sand-point.y == $floor-y - 1 && !$part1-sand-count {
            $part1-sand-count = $sand-points.elems - 1;
        }
    }

    my $part2-sand-count = $sand-points.elems;

    say "Part 1: $part1-sand-count";
    say "Part 2: $part2-sand-count";
}

sub simulate-sand-drop($rock-points, $sand-points, $floor-y) {
    my $sand = $origin-point.clone;
    my $next = $sand.clone;
    loop {
        $next.x = $sand.x;
        last if $sand.y == $floor-y - 1;

        $next.y = $sand.y + 1;
        if $next ∉ $rock-points && $next ∉ $sand-points {
            $sand.x = $next.x;
            $sand.y = $next.y;
            next;
        }

        $next.x = $sand.x - 1;
        if $next ∉ $rock-points && $next ∉ $sand-points {
            $sand.x = $next.x;
            $sand.y = $next.y;
            next;
        }

        $next.x = $sand.x + 1;
        if $next ∉ $rock-points && $next ∉ $sand-points {
            $sand.x = $next.x;
            $sand.y = $next.y;
            next;
        }

        last;
    }
    return $sand;
}

sub parse-input {
    gather for $*IN.lines -> $line {
        my @path;
        my $point-strings = $line.split(' -> ');
        for $point-strings».split(',') -> ($x, $y) {
            @path.push: Point.new(x => +$x, y => +$y);
        }
        take @path;
    }
}

sub expand-path-points(@paths) {
    my $result = SetHash.new;
    for @paths -> $path {
        for $path.rotor(2 => -1) -> ($start, $end) {
            my $x-delta = ($end.x - $start.x).sign;
            my $y-delta = ($end.y - $start.y).sign;

            loop (my $x = $start.x, my $y = $start.y;
                  ;
                  $x += $x-delta, $y += $y-delta)
            {
                my $point = Point.new(:$x, :$y);
                $result.set($point);
                last if $point eqv $end;
            }
        }
    }
    return $result;
}

sub draw($rock-points, $sand-points) {
    my $x-range := minmax(|$rock-points.keys».x, |$sand-points.keys».x);
    my $y-range := 0 .. max(|$rock-points.keys».y) + 2;
    my $floor-y = max($y-range);
    my $sand-point = Point.new(:x(500), :y(0));

    for $y-range -> $y {
        for $x-range -> $x {
            my $point = Point.new(:$x, :$y);
            my $char = do given $point {
                when *  ∈  $sand-points { 'o' }
                when *  ∈  $rock-points { '#' }
                when *.y == $floor-y    { '#' }
                when * eqv $sand-point  { '+' }
                default                 { '.' }
            };
            print $char;
        }
        print "\n";
    }
}
