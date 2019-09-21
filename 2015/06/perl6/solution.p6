use v6;

# ------------------------------------------------------------------------------
# Instruction Data

class Point {
    has Int $.x;
    has Int $.y;
}

class Square {
    has Point $.top-left;
    has Point $.bottom-right;
}

enum Action <TurnOn TurnOff Toggle>;

class Instruction {
    has Action $.action;
    has Square $.square;

    method gist {
        return "Instruction<{self.action}, {self.square.gist}>";
    }
}

# ------------------------------------------------------------------------------
# Grid

class LightGrid {
    my $size := 1_000_000;
    my $row-length := 1_000;

    has @!cells = False xx $size;

    method gist {
        my @result;
        for 0 ..^ $row-length -> $row-multiplier {
            my $start-index = $row-multiplier * $row-length;
            my $end-index   = $start-index + $row-length - 1;

            my @row   = @!cells[ $start-index .. $end-index ];
            my @chars = @row.map({ $^on ?? "░" !! "▓" });

            @result.push(@chars.join);
        }
        return @result.join("\n");
    }

    method light-on-count(--> Int) {
        return @!cells.grep(so *).elems;
    }

    method follow(Instruction $instruction) {
        my $action-function;
        given $instruction.action {
            when TurnOn {
                $action-function = { @!cells[$^index] = True };
            }
            when TurnOff {
                $action-function = { @!cells[$^index] = False };
            }
            when Toggle {
                $action-function = { @!cells[$^index] = not @!cells[$index] };
            }
        }
        self!update-square($instruction.square, $action-function);
    }

    method !update-square($square, $action) {
        my $first-row = $square.top-left.y;
        my $last-row  = $square.bottom-right.y;

        my $first-column = $square.top-left.x;
        my $last-column  = $square.bottom-right.x;

        for $first-row .. $last-row -> $row {
            for $first-column .. $last-column -> $column {
                self!update-point: Point.new(x => $column, y => $row), $action;
            }
        }
    }

    method !update-point($p, $action) {
        my $cell-index = $p.x + ($p.y * $row-length);
        $action($cell-index);
    }
}

# ------------------------------------------------------------------------------
# Instruction Parsing

grammar EnglishInstruction {
    token TOP { <action> ' ' <top-left> ' through ' <bottom-right> }

    token action       { 'turn on' | 'turn off' | 'toggle' }
    token top-left     { <point> }
    token bottom-right { <point> }
    token point        { (\d+) ',' (\d+) }
}

class EnglishInstructionActions {
    method TOP($/) {
        make Instruction.new(
            action => $<action>.made,
            square => Square.new(
                top-left => $<top-left>.made,
                bottom-right => $<bottom-right>.made
            )
        );
    }

    method action($/) {
        given $/.Str {
            when "turn on"  { make TurnOn }
            when "turn off" { make TurnOff }
            when "toggle"   { make Toggle }
        }
    }

    method top-left($/) {
        make $<point>.made;
    }

    method bottom-right($/) {
        make $<point>.made;
    }

    method point($/) {
        make Point.new(
            x => $/[0].Int,
            y => $/[1].Int,
        );
    }
}

sub parse(Str $instruction --> Instruction) {
    my $actions = EnglishInstructionActions.new;
    my $result = EnglishInstruction.parse($instruction, actions => $actions);
    return $result.made;
}

# ------------------------------------------------------------------------------
# I/O

my $grid = LightGrid.new;

for $*IN.lines -> $line {
    $grid.follow: $line.&parse;
}

say $grid.light-on-count;
