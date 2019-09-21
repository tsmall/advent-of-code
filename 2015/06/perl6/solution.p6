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

role InstructionInterpreter {
    method interpret($action, $current-value) { ... }
}

class ObviousInterpreter does InstructionInterpreter {
    method interpret($action, $current-value) {
        given $action {
            when TurnOn  { return True }
            when TurnOff { return False }
            when Toggle  { return not $current-value }
        }
    }
}

class AncientNordicElvishInterpreter does InstructionInterpreter {
    method interpret($action, $current-value) {
        given $action {
            when TurnOn  { return $current-value + 1 }
            when TurnOff { return max 0, ($current-value - 1) }
            when Toggle  { return $current-value + 2 }
        }
    }
}

# ------------------------------------------------------------------------------
# Grid

class LightGrid {
    my $size := 1_000_000;
    my $row-length := 1_000;

    has InstructionInterpreter $.interpreter is required;
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

    method total-brightness(--> Int) {
        return @!cells.sum;
    }

    method follow(Instruction $instruction) {
        self!update-square(
            $instruction.square,
            $instruction.action
        );
    }

    method !update-square($square, $action) {
        my $first-row = $square.top-left.y;
        my $last-row  = $square.bottom-right.y;

        my $first-column = $square.top-left.x;
        my $last-column  = $square.bottom-right.x;

        for $first-row .. $last-row -> $row {
            for $first-column .. $last-column -> $column {
                my $point = Point.new(x => $column, y => $row);
                self!update-point($point, $action);
            }
        }
    }

    method !update-point($p, $action) {
        my $index = $p.x + ($p.y * $row-length);
        @!cells[$index] = self.interpreter.interpret($action, @!cells[$index]);
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

my $obvious-grid = LightGrid.new(interpreter => ObviousInterpreter.new);
my $ancient-grid = LightGrid.new(interpreter => AncientNordicElvishInterpreter.new);
my @grids = $obvious-grid, $ancient-grid;

for $*IN.lines -> $line {
    @grids».follow: $line.&parse;
}

say "--- Part One ---";
say $obvious-grid.light-on-count;
say "";
say "--- Part Two ---";
say $ancient-grid.total-brightness;
