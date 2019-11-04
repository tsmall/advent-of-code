use v6;


role PairingList does Positional {
    method where(Str $person-a, Str :is-next-to($person-b)) {
        return self.first: -> $pairing {
            $pairing.person-a eq $person-a and $pairing.person-b eq $person-b
        }
    }
}


class Pairing {
    has Str $.person-a;
    has Str $.person-b;
    has Int $.units;
}


sub find-highest-total(Setty $people, PairingList $pairings) {
    my @permutations = $people.keys.permutations;
    my @totals = @permutations.map: {
        total-units($pairings, $^permutation)
    };
    return @totals.max;
}


sub total-units(PairingList $pairings, @seating) {
    my @units-forward  = units($pairings, @seating);
    my @units-backward = units($pairings, @seating.reverse);

    return @units-forward.sum + @units-backward.sum;
}


sub units(PairingList $pairings, @seating) {
    my @pairs = zip @seating, @seating.rotate;
    my @units = @pairs.map: -> [$a, $b] {
        $pairings.where($a, is-next-to => $b).units
    };
    return @units;
}


sub add-me($people, @pairings) {
    for $people.keys -> $person {
        @pairings.push(
            Pairing.new(
                person-a => 'Me',
                person-b => $person,
                units    => 0
            )
        );
        @pairings.push(
            Pairing.new(
                person-a => $person,
                person-b => 'Me',
                units    => 0
            )
        );
    }
    $people<Me>++;
}


# ------------------------------------------------------------------------------
# Parsing


grammar Parser {
    token TOP {
        <person-a> ' would ' <gain-or-lose> ' ' <units> ' happiness units '
        'by sitting next to ' <person-b> '.'
    }

    token person-a     { \w+ }
    token gain-or-lose { 'gain' | 'lose' }
    token units        { \d+ }
    token person-b     { \w+ }
}


class ParserActions {
    method TOP ($/) {
        make Pairing.new(
            person-a => $<person-a>.made,
            person-b => $<person-b>.made,
            units    => $<units>.made * $<gain-or-lose>.made
        );
    }

    method person-a     ($/) { make ~$/; }
    method gain-or-lose ($/) { make $/ eq 'gain' ?? +1 !! -1; }
    method units        ($/) { make +$/; }
    method person-b     ($/) { make ~$/; }
}


sub parse(Str $line) {
    state $actions = ParserActions.new;
    my $results = Parser.parse($line, actions => $actions);
    return $results.made;
}


# ------------------------------------------------------------------------------
# Main


sub MAIN {
    my SetHash $people .= new;
    my @pairings does PairingList = ();

    for $*IN.lines -> $line {
        my $pairing = parse($line);

        @pairings.push($pairing);

        $people{$pairing.person-a}++;
        $people{$pairing.person-b}++;
    }

    say '--- Part One ---';
    say find-highest-total($people, @pairings);

    say '';

    say '--- Part Two ---';
    add-me($people, @pairings);
    say find-highest-total($people, @pairings);
}
