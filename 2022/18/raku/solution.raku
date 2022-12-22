use v6;

class Cube {
    has Int $.x;
    has Int $.y;
    has Int $.z;
    has Int $.connected-sides is rw = 0;

    method surface-area {
        6 - $.connected-sides
    }
}

sub MAIN {
    my @cubes;
    for parse-input() -> $new-cube {
        # NOTE: To speed this up, I could probably find a way to compare
        #       the new cube to only the cubes near it, instead of checking
        #       all cubes. But this is fast enough for now.
        for @cubes -> $old-cube {
            if are-connected($new-cube, $old-cube) {
                ++$new-cube.connected-sides;
                ++$old-cube.connected-sides;
            }
        }
        @cubes.push($new-cube);
    }

    my $surface-area = [+] @cubes».surface-area;

    say "Part 1: $surface-area";
    say "Part 2: TODO";
}

sub are-connected(Cube $a, Cube $b --> Bool) {
    my $dx = $a.x - $b.x;
    my $dy = $a.y - $b.y;
    my $dz = $a.z - $b.z;

    return (
        ($dx     == 0 && $dy     == 0 && $dz.abs == 1) ||
        ($dx     == 0 && $dy.abs == 1 && $dz     == 0) ||
        ($dx.abs == 1 && $dy     == 0 && $dz     == 0)
    );
}

sub parse-input {
    gather for $*IN.lines -> $line {
        my $coords = $line.split(',');
        take Cube.new(x => +$coords[0], y => +$coords[1], z => +$coords[2]);
    }
}
