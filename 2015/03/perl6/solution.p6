use v6;

class Point {
    has Int $.x;
    has Int $.y;

    method WHICH {
        ValueObjAt.new("Point|$!x|$!y");
    }

    method moved-by(Int :$x = 0, Int :$y = 0) {
        Point.new(x => self.x + $x, y => self.y + $y);
    }
}

my $current-point  = Point.new(x => 0, y => 0);
my $points-visited = BagHash.new($current-point);

while not $*IN.eof {
    given $*IN.getc {
        when '^' { $current-point.=moved-by(y => -1) }
        when 'v' { $current-point.=moved-by(y =>  1) }
        when '>' { $current-point.=moved-by(x =>  1) }
        when '<' { $current-point.=moved-by(x => -1) }
        default  { next }
    }

    $points-visited{$current-point} += 1;
}

say $points-visited.values.elems;
