use v6;

class Point {
    has Int $.x;
    has Int $.y;

    method origin {
        self.new(x => 0, y => 0)
    }

    method WHICH {
        ValueObjAt.new("Point|$!x|$!y");
    }

    method moved-by(Int :$x = 0, Int :$y = 0) {
        Point.new(x => self.x + $x, y => self.y + $y);
    }
}

class GiftGiver {
    has Point   $!current-point  = Point.origin;
    has SetHash $!points-visited = SetHash.new($!current-point);

    method move($direction) {
        given $direction {
            when '^' { $!current-point.=moved-by(y => -1) }
            when 'v' { $!current-point.=moved-by(y =>  1) }
            when '>' { $!current-point.=moved-by(x =>  1) }
            when '<' { $!current-point.=moved-by(x => -1) }
            default  { return }
        }
        $!points-visited{$!current-point}++;
    }

    method houses-visited {
        $!points-visited.Set;
    }
}

my $lone-santa = GiftGiver.new;

my $partnered-santa = GiftGiver.new;
my $robo-santa = GiftGiver.new;
my $current-santa = $partnered-santa;

while not $*IN.eof {
    my $next-direction = $*IN.getc;

    # For part one
    $lone-santa.move($next-direction);

    # For part two
    $current-santa.move($next-direction);
    given $current-santa {
        when $partnered-santa { $current-santa = $robo-santa; }
        when $robo-santa      { $current-santa = $partnered-santa; }
    }
}

say "--- Part One ---";
say "";
say "Alone, Santa visited {$lone-santa.houses-visited.elems} houses.";
say "";

my $partnered-santa-houses = $partnered-santa.houses-visited;
my $robo-santa-houses = $robo-santa.houses-visited;
my $total-houses = $partnered-santa-houses ∪ $robo-santa-houses;

say "--- Part Two ---";
say "";
say "Paired, Santa visited {$partnered-santa-houses.elems} houses";
say "and Robo-Santa visited {$robo-santa-houses.elems} houses,";
say "resulting in {$total-houses.elems} distinct houses visited.";
