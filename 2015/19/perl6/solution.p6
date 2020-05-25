enum Part <Part1 Part2>;

sub MAIN(Part $part) {
    my %replacements = parse($part);
    my $molecule = $*IN.get;

    my $answer = run($part, %replacements, $molecule);

    say "--- {$part} ---";
    say $answer;
}

multi parse(Part1) {
    my %replacements is default( () );
    for $*IN.lines -> $line {
        last if not $line;
        my ($key, $value) = $line.split(' => ');
        %replacements{$key} = |%replacements{$key}, $value;
    }

    return %replacements;
}

multi parse(Part2) {
    my %replacements is default( () );
    for $*IN.lines -> $line {
        last if not $line;
        my ($key, $value) = $line.split(' => ');
        %replacements{$value} = |%replacements{$value}, $key;
    }

    return %replacements;
}

multi run(Part1, %replacements, $molecule --> Int) {
    my $results = step(%replacements, $molecule);
    return $results.elems;
}

# This solution only sometimes works. Sometimes it will get stuck in an
# infinite loop because it can't make any more replacements. When it does work
# it finds the answer quickly though. So ... good enough for who it's for!
multi run(Part2, %replacements, $start --> Int) {
    my $steps    = 0;
    my $molecule = $start;

    until $molecule eq 'e' {
        my $key = %replacements.keys.pick;
        $molecule = $molecule.subst: $key, {
            $steps++;
            %replacements{$key}
        };
    }

    return $steps;
}

sub step(%replacements, $start) {
    my $results = SetHash.new;
    my $molecule = $start.clone;

    for %replacements.pairs -> $pair {
        my @indices = $molecule.indices($pair.key);
        my @replacements = $pair.value.values;

        for @indices -> $i {
            for @replacements -> $atom {
                my $ref := $molecule.substr-rw($i, $pair.key.chars);
                $ref = $atom;
                $results{$molecule}++;
                $ref = $pair.key;
            }
        }
    }

    return $results;
}
