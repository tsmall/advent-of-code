use v6;

class Crane {
    has @.stacks;

    method new(@stacks) {
        my @cloned-stacks = @stacks.map: *.clone;
        return self.bless(stacks => @cloned-stacks);
    }

    method move(:$quantity, :$source, :$target) { ... }

    method top-crates {
        return @!stacks.map(*.tail).join();
    }
}

class CrateMover9000 is Crane {
    method move(:$quantity, :$source, :$target) {
        my $source-stack = @.stacks[$source];
        my $target-stack = @.stacks[$target];
        for ^$quantity {
            my $crate = $source-stack.pop();
            $target-stack.push($crate);
        }
    }
}

class CrateMover9001 is Crane {
    method move(:$quantity, :$source, :$target) {
        my $source-stack = @.stacks[$source];
        my @crates = $source-stack.splice($source-stack.elems - $quantity);
        @.stacks[$target].push(|@crates);
    }
}

sub MAIN(Int $stack-count) {
    my @stacks = parse-stacks($stack-count);

    my $crane-one = CrateMover9000(@stacks);
    my $crane-two = CrateMover9001(@stacks);

    for parse-steps() -> %step {
        $crane-one.move(|%step);
        $crane-two.move(|%step);
    }

    say "Part 1: $crane-one.top-crates()";
    say "Part 2: $crane-two.top-crates()";
}

sub parse-stacks($stack-count) {
    my @stacks = [] xx $stack-count;
    for $*IN.lines -> $line {
        last unless $line.contains('[');
        parse-column-line($line, @stacks);
    }
    @stacks = @stacks.map: *.reverse.Array;
    return @stacks;
}

sub parse-column-line(Str $line, @stacks) {
    my $chars = $line.comb;
    my $current-stack = 0;
    loop (my $i = 1; $i < @stacks.elems * 4; $i += 4) {
        my $letter = $chars[$i];
        @stacks[$current-stack].push($letter) if $letter ne ' ';
        ++$current-stack;
    }
}

grammar ProcedureStep {
    token TOP { 'move ' <quantity> ' from ' <source> ' to ' <target> }

    token quantity { \d+ }
    token source   { \d+ }
    token target   { \d+ }
}

class ProcedureStepActions {
    method TOP($/) {
        # Convert base-1 indexes to base-0
        make {
            quantity => +$<quantity>,
            source   => +$<source> - 1,
            target   => +$<target> - 1
        }
    }
}

sub parse-steps() {
    my $parse-actions = ProcedureStepActions.new;
    gather for $*IN.lines -> $line {
        my $match = ProcedureStep.parse($line, actions => $parse-actions);
        next unless $match;
        take $match.made;
    }
}
