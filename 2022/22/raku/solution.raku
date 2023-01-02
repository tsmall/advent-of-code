use v6;

constant \R = 0;
constant \D = 1;
constant \L = 2;
constant \U = 3;

sub MAIN {
    my (%map, @instructions) := parse-input();

    my $dir = R;
    my $x = 9;
    my $y = 1;

    sub print-state($instruction) {
        say "dir=$dir x=$x y=$y";
        say "$instruction";
    }

    for @instructions {
        # print-state($_);

        when 'R' { $dir = ($dir + 1) % 4; }
        when 'L' { $dir = ($dir - 1) % 4; }
        default  {
            for 1..$_ {
                my $nx = $x;
                my $ny = $y;
                given $dir {
                    when R { $nx += 1; }
                    when D { $ny += 1; }
                    when L { $nx -= 1; }
                    when U { $ny -= 1; }
                }

                given %map{"$nx|$ny"} {
                    when '#' { last; }
                    when '.' { }
                    default  {
                        given $dir {
                            when R { $nx = min-x(%map, $ny); }
                            when D { $ny = min-y(%map, $nx); }
                            when L { $nx = max-x(%map, $ny); }
                            when U { $ny = max-y(%map, $nx); }
                        }

                        last if %map{"$nx|$ny"} eq '#';
                    }
                }

                $x = $nx;
                $y = $ny;
            }
        }
    }

    my $password = 1000*$y + 4*$x + $dir;

    say "Part 1: $password";
    say "Part 2: TODO";
}

sub min-x(%map, $y) {
    for 1..∞ -> $x {
        return $x if %map{"$x|$y"} eq '.'|'#';
    }
}

sub max-x(%map, $y) {
    my $min-x = min-x(%map, $y);
    for $min-x..∞ -> $x {
        my $char = %map{"$x|$y"};
        return $x-1 if !$char.defined;
        return $x   if $char !eq '.'|'#';
    }
}

sub min-y(%map, $x) {
    for 1..∞ -> $y {
        return $y if %map{"$x|$y"} eq '.'|'#';
    }
}

sub max-y(%map, $x) {
    my $min-y = min-y(%map, $x);
    for $min-y..∞ -> $y {
        my $char = %map{"$x|$y"};
        return $y-1 if !$char.defined;
        return $y   if $char !eq '.'|'#';
    }
}

sub parse-input {
    my %map;
    for 1..∞ Z $*IN.lines -> ($row, $line) {
        last if !$line;
        for 1..∞ Z $line.comb -> ($col, $char) {
            %map{"$col|$row"} = $char;
        }
    }

    my @instructions = $*IN.lines.head.comb(/\d+ || L || R/);

    return %map.Map, @instructions.List;
}
