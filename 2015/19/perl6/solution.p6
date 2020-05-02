class Molecule {
    has Str $!start;
    has Str @!atoms;

    submethod BUILD(:$start, :@atoms) {
        $!start = $start;
        @!atoms = @atoms;
    }

    method atoms {
        return gather {
            my $atom = '';
            for $!start.comb -> $char {
                if $char ~~ /<upper>/ {
                    take $atom if $atom;
                    $atom = $char;
                }
                else {
                    $atom ~= $char;
                }

                if $atom ∈ @!atoms {
                    take $atom;
                    $atom = '';
                }
            }
        }
    }
}


class FusionPlant {
    has $.molecule;
    has %.replacements;

    method possibilities {
        my @atoms = $.molecule.atoms;
        return gather {
            my @prefix = [];
            my @suffix = @atoms.Array;

            while @suffix {
                my $current = @suffix.shift;
                if %.replacements{$current}:exists {
                    for %.replacements{$current} -> @replacements {
                        for @replacements {
                            my @possibility = flat @prefix, $_, @suffix;
                            take @possibility.join;
                        }
                    }
                }
                @prefix.push($current);
            }
        }
    }
}


sub parse-line(%replacements, $line) {
    my ($key, $value) = $line.split(' => ');
    %replacements{$key} = |%replacements{$key}, $value;
}


sub MAIN {
    my %replacements is default( () );
    for $*IN.lines -> $line {
        last if not $line;
        parse-line(%replacements, $line);
    }

    my $start = $*IN.get;

    my $molecule = Molecule.new(
        start => $start,
        atoms => %replacements.keys.list,
    );

    my $machine = FusionPlant.new(
        :$molecule,
        :%replacements,
    );

    say '--- Part One ---';
    say $machine.possibilities.Set.elems;
}
