use v6;

constant \ROWS = 1_000_000;
my @chamber[ROWS; 7];

sub MAIN {
    my @directions = $*IN.lines.head.comb;

    my $max-y-part-one;
    my $max-y-part-two;

    my $max-y = 0;
    my $rock-count = 0;
    while $rock-count < 1_000_000_000_000 {
        $max-y = max-y($max-y);

        if $rock-count == 2022 {
            $max-y-part-one = $max-y;
            last;
        }

        if $rock-count == 1_000_000_000_000 {
            $max-y-part-two = $max-y;
            last;
        }

        my @position = (2, $max-y + 3);
        my $shape = next-shape();
        # say $shape, ' ', @position;
        # say $max-y;

        my $prev-y = @position[1];
        loop {
            my $direction = next-direction(@directions);
            move($shape, @position, $direction, $max-y);
            # say $direction, ' ', @position;

            last if @position[1] == $prev-y;
            $prev-y = @position[1];
        }

        record-final-position($shape, @position);
        # draw-chamber();
        # print("\n");

        ++$rock-count;
    }

    say "Part 1: $max-y-part-one";
    say "Part 2: TODO";
}

constant @shapes = '-', '+', '⅃', '|', '☐';

sub next-shape {
    state $index = 0;

    my $result = @shapes[$index];
    $index += 1;
    $index %= @shapes.elems;
    return $result;
}

sub next-direction(@directions) {
    state $index = 0;

    my $result = @directions[$index];
    $index += 1;
    $index %= @directions.elems;
    return $result;
}

sub max-y($prev-max-y = 0) {
    TOP:
    for $prev-max-y .. ∞ -> $y {
        for ^7 -> $x {
            next TOP if @chamber[$y; $x];
        }
        return $y;
    }
}

sub draw-chamber {
    my $y = max-y();
    while $y >= 0 {
        print('|');
        for ^7 -> $x {
            print(@chamber[$y; $x] ?? '#' !! '.');
        }
        print('|');
        print("\n");
        --$y;
    }
    print('+-------+');
    print("\n");
}

multi record-final-position('-', @position) {
    my $x := @position[0];
    my $y := @position[1];

    @chamber[$y; $_] = True for $x..$x+3;
}

multi record-final-position('+', @position) {
    my $x := @position[0];
    my $y := @position[1];

    @chamber[$y+2; $x+1] = True;
    @chamber[$y+1; $x  ] = True;
    @chamber[$y+1; $x+1] = True;
    @chamber[$y+1; $x+2] = True;
    @chamber[$y  ; $x+1] = True;
}

multi record-final-position('⅃', @position) {
    my $x := @position[0];
    my $y := @position[1];

    @chamber[$y+2; $x+2] = True;
    @chamber[$y+1; $x+2] = True;
    @chamber[$y  ; $x  ] = True;
    @chamber[$y  ; $x+1] = True;
    @chamber[$y  ; $x+2] = True;
}

multi record-final-position('|', @position) {
    my $x := @position[0];
    my $y := @position[1];

    @chamber[$_; $x] = True for $y..$y+3;
}

multi record-final-position('☐', @position) {
    my $x := @position[0];
    my $y := @position[1];

    @chamber[$y+1; $x  ] = True;
    @chamber[$y+1; $x+1] = True;
    @chamber[$y  ; $x  ] = True;
    @chamber[$y  ; $x+1] = True;
}

multi collided('-', @position, $max-y) {
    my $x := @position[0];
    my $y := @position[1];

    return True if $x > 3 || $x < 0 || $y < 0;
    return False if $y > $max-y;

    for $x..$x+3 {
        return True if @chamber[$y; $_];
    }

    return False;
}

multi collided('+', @position, $max-y) {
    my $x := @position[0];
    my $y := @position[1];

    return True if $x > 4 || $x < 0 || $y < 0;
    return False if $y > $max-y;

    return so any(
        | @chamber[$y+2; $x+1]
        | @chamber[$y+1; $x  ]
        | @chamber[$y+1; $x+1]
        | @chamber[$y+1; $x+2]
        | @chamber[$y  ; $x+1]
    );
}

multi collided('⅃', @position, $max-y) {
    my $x := @position[0];
    my $y := @position[1];

    return True if $x > 4 || $x < 0 || $y < 0;
    return False if $y > $max-y;

    return so any(
        | @chamber[$y+2; $x+2]
        | @chamber[$y+1; $x+2]
        | @chamber[$y  ; $x  ]
        | @chamber[$y  ; $x+1]
        | @chamber[$y  ; $x+2]
    );
}

multi collided('|', @position, $max-y) {
    my $x := @position[0];
    my $y := @position[1];

    return True if $x > 6 || $x < 0 || $y < 0;
    return False if $y > $max-y;

    for $y..$y+3 {
        return True if @chamber[$_; $x];
    }

    return False;
}

multi collided('☐', @position, $max-y) {
    my $x := @position[0];
    my $y := @position[1];

    return True if $x > 5 || $x < 0 || $y < 0;
    return False if $y > $max-y;

    return so any(
        | @chamber[$y+1; $x  ]
        | @chamber[$y+1; $x+1]
        | @chamber[$y  ; $x  ]
        | @chamber[$y  ; $x+1]
    );
}

constant %opposite-of = ('>' => '<', '<' => '>');

sub move($shape, @position, $direction, $max-y) {
    push(@position, $direction);
    if collided($shape, @position, $max-y) {
        push(@position, %opposite-of{$direction})
    }

    drop(@position);
    if collided($shape, @position, $max-y) {
        lift(@position)
    }
}

sub drop(@position) { --@position[1] }
sub lift(@position) { ++@position[1] }

multi push(@position, '>') { ++@position[0] }
multi push(@position, '<') { --@position[0] }
