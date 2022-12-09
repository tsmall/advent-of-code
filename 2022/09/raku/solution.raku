use v6;

class Point {
    has Int $.x is rw;
    has Int $.y is rw;

    method origin {
        return self.bless(x => 0, y => 0);
    }

    method WHICH {
        return "Point|$.x|$.y";
    }
}

class Move {
    has Str $.direction is rw;
    has Int $.steps is rw;

    method gist {
        return "=== $.direction $.steps ===";
    }
}

sub MAIN() {
    my @short-rope = Point.origin xx 2;
    my @long-rope = Point.origin xx 10;
    my @ropes = @short-rope, @long-rope;
    my @visits = SetHash.new xx 2;

    my $move = Move.new(direction => Nil, steps => Nil);
    while (next-move($move)) {
        while ($move.steps > 0) {
            for ^@ropes.elems -> $rope-index {
                my @rope := @ropes[$rope-index];
                my $visits := @visits[$rope-index];
                my $head = @rope.head;

                update-head($head, $move);
                for 1 ..^ @rope.elems -> $knot-index {
                    my $tail = @rope[$knot-index];
                    update-tail($tail, $head);
                    $head = $tail;
                }
                $visits.set(@rope.tail.WHICH);
            }
            --$move.steps;
        }
    }

    say "Part 1: @visits[0].elems()";
    say "Part 2: @visits[1].elems()";
}

sub next-move(Move $move) {
    state $iterator = $*IN.lines.iterator;

    my $result = False;
    my $line := $iterator.pull-one;
    unless $line =:= IterationEnd {
        my $parts = $line.split(' ');
        my $direction = $parts[0];
        my $steps = +$parts[1];
        die unless $direction eq 'U'|'D'|'L'|'R';
        die unless $steps > 0;
        $move.direction = $direction;
        $move.steps = $steps;
        $result = True;
    }

    return $result;
}

sub update-head(Point $head, Move $move) {
    given $move.direction {
        when 'U' { --$head.y }
        when 'D' { ++$head.y }
        when 'L' { --$head.x }
        when 'R' { ++$head.x }
    }
}

sub update-tail(Point $tail, Point $head) {
    my $dx := $head.x - $tail.x;
    my $dy := $head.y - $tail.y;
    my $too-far-away := ($dx.abs > 1) || ($dy.abs > 1);

    if $too-far-away {
        if $tail.y == $head.y {
            $tail.x += $dx.sign;
        }
        elsif $tail.x == $head.x {
            $tail.y += $dy.sign;
        }
        else {
            $tail.x += $dx.sign;
            $tail.y += $dy.sign;
        }
    }
}

sub draw-rope(@rope, Point $upper-left, Point $bottom-right) {
    my $origin = Point.origin;
    my $current-point = Point.origin;
    for $upper-left.y .. $bottom-right.y -> $y {
        for $upper-left.x .. $bottom-right.x -> $x {
            $current-point.x = $x;
            $current-point.y = $y;
            my $char = '.';
            $char = 's' if $current-point === $origin;
            loop (my $i = @rope.elems - 1; $i >= 0; --$i) {
                $char = $i if @rope[$i] === $current-point;
            }
            $char = 'H' if $char eq '0';
            $char = 'T' if $char eq @rope.elems-1;
            print $char;
        }
        print "\n";
    }
}
