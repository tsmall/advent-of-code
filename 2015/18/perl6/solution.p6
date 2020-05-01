class Grid { ... }


enum State <On Off>;


class Light {
    has Int   $.index;
    has Int   @.neighbors = [];
    has State $.state     = Off;
    has Bool  $.stuck     = False;

    method add-neighbor(Int $neighbor) {
        @!neighbors.push($neighbor);
    }

    method on {
        return $.state == On;
    }

    method toggle {
        given $.state {
            when On  { self.turn(Off) }
            when Off { self.turn(On)  }
        }
    }

    method turn(State $new-state) {
        return if $.stuck;
        $!state = $new-state;
    }

    method stuck-on {
        self.turn(On);
        $!stuck = True;
    }

    method animate(Grid $current, Grid $new) {
        my @neighbors = $current.lights[|@.neighbors];
        my @ons       = @neighbors.grep: { .on };

        given $new.lights[$.index] -> $new {
            my $new-on;
            if $.on {
                $new-on = (@ons.elems ∈ (2, 3));
            }
            else {
                $new-on = (@ons.elems == 3);
            }
            $new.turn( $new-on ?? On !! Off );
        }
    }
}


class Grid {
    has Int   $.size;
    has Light @.lights;

    submethod BUILD(Int :$size) {
        $!size = $size;
        my $length = $size * $size;
        @!lights = (0..^$length).map: { Light.new(:index($_)) };

        my Int ($top-left, $top-center, $top-right);
        my Int ($left, $right);
        my Int ($bottom-left, $bottom-center, $bottom-right);

        for @!lights.kv -> $i, $light {
            $top-left      = $i - $size - 1;
            $top-center    = $i - $size;
            $top-right     = $i - $size + 1;
            $left          = $i - 1;
            $right         = $i + 1;
            $bottom-left   = $i + $size - 1;
            $bottom-center = $i + $size;
            $bottom-right  = $i + $size + 1;

            given $light {
                .add-neighbor($top-left)      if self!on-top-row($size, $i, $top-left);
                .add-neighbor($top-center)    if self!on-top-row($size, $i, $top-center);
                .add-neighbor($top-right)     if self!on-top-row($size, $i, $top-right);
                .add-neighbor($left)          if self!on-current-row($size, $i, $left);
                .add-neighbor($right)         if self!on-current-row($size, $i, $right);
                .add-neighbor($bottom-left)   if self!on-bottom-row($size, $i, $bottom-left);
                .add-neighbor($bottom-center) if self!on-bottom-row($size, $i, $bottom-center);
                .add-neighbor($bottom-right)  if self!on-bottom-row($size, $i, $bottom-right);
            }
        }
    }

    method !on-top-row(Int $size, Int $i, Int $other --> Bool) {
        my $min = (($i div $size) * $size) - $size;
        my $max = (($i div $size) * $size) + ($size - 1) - $size;
        return so ($other ∈ $min..$max) & ($other >= 0);
    }

    method !on-current-row(Int $size, Int $i, Int $other --> Bool) {
        my $min = ($i div $size) * $size;
        my $max = (($i div $size) * $size) + ($size - 1);
        return so ($other ∈ $min..$max) & ($other < ($size * $size));
    }

    method !on-bottom-row(Int $size, Int $i, Int $other --> Bool) {
        my $min = (($i div $size) * $size) + $size;
        my $max = (($i div $size) * $size) + ($size - 1) + $size;
        return so ($other ∈ $min..$max) & ($other < ($size * $size));
    }

    method gist {
        my @symbols = @.lights.map: { .on ?? '#' !! '.' };
        my @lines   = @symbols.batch($.size)».join;
        return @lines.join("\n");
    }
}


class GridBuffer {
    has Grid $.current;
    has Grid $.next;

    submethod BUILD(Int :$size) {
        $!current = Grid.new(size => $size);
        $!next    = Grid.new(size => $size);
    }

    method animate {
        $.current.lights».animate($.current, $.next);
        ($!current, $!next) = ($!next, $!current);
    }
}


sub parse-initial-state(@lines, $size) {
    return gather {
        my $offset = 0;
        for @lines -> $line {
            for $line.comb.kv -> $i, $char {
                take $i + $offset if $char eq '#';
            }
            $offset += $size;
        }
    }
}


sub clear-screen {
    print "\e[2J";
}


sub cursor-home {
    print "\e[H";
}


sub MAIN(Int $part, Int $size, Int $steps, Bool :$verbose, Bool :$animated) {
    my $buffer = GridBuffer.new(:$size);

    # Set up initial state
    my @lights-on = parse-initial-state($*IN.lines, $size);
    $buffer.current.lights[|@lights-on]».toggle;

    if $part == 2 {
        my @stuck-positions = (
            0,                                 # top left
            $size - 1,                         # top right
            ($size - 1) * $size,               # bottom left
            ($size - 1) * $size + ($size - 1), # bottom right
        );

        my @lights = $buffer.current.lights[|@stuck-positions];
        @lights».stuck-on;

        @lights = $buffer.next.lights[|@stuck-positions];
        @lights».stuck-on;
    }

    if $verbose {
        say "Initial state:\n";
        say $buffer.current, "\n";
    }

    if $animated {
        clear-screen;
        cursor-home;
        say $buffer.current;
    }

    my $plural = '';
    for 1..$steps -> $i {
        $buffer.animate;

        if $verbose {
            say "\nAfter $i step$plural:\n";
            say $buffer.current, "\n";
            once { $plural = 's' };
        }

        if $animated {
            clear-screen;
            cursor-home;
            say $buffer.current;
            sleep 1/2;
        }
    }

    say "--- Part $part ---";
    say $buffer.current.lights.grep({ .on }).elems;
}
