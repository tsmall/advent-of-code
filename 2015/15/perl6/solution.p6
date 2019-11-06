use v6;


# ------------------------------------------------------------------------------
# Parsing


grammar Parser {
    token TOP {
        <name> ': '
        'capacity '   <capacity=.number>   ', '
        'durability ' <durability=.number> ', '
        'flavor '     <flavor=.number>     ', '
        'texture '    <texture=.number>    ', '
        'calories '   <calories=.number>
    }

    token name   { \w+ }
    token number { '-'? \d+ }
}


class ParserActions {
    method TOP ($/) {
        make (
            +$<capacity>,
            +$<durability>,
            +$<flavor>,
            +$<texture>,
            +$<calories>,
        );
    }

    method number ($/) { make +$/ }
}


sub parse-input($lines) {
    my $actions = ParserActions.new;
    return $lines.map: { Parser.parse($_, actions => $actions).made };
}


# ------------------------------------------------------------------------------
# Logic


sub possible-mixes(@ingredients) {
    my $length = @ingredients.elems;

    gather {
        for 0 .. 100 -> $x {
            if $length == 2 {
                take [$x, 100 - $x];
            }
            else {
                for 0 .. (100 - $x) -> $y {
                    if $length == 3 {
                        take [$x, $y, 100 - $x - $y];
                    }
                    else {
                        for 0 .. (100 - $x - $y) -> $z {
                            take [$x, $y, $z, 100 - $x - $y - $z];
                        }
                    }
                }
            }
        }
    }
}


sub score(@ingredients, @mix) {
    my @multiples = do for @ingredients.kv -> $i, @props {
        @props »*» @mix[$i]
    }

    my @properties  = [Z] @multiples;
    my @score-props = @properties[0 .. *-2];
    my @sums        = @score-props.map( *.sum max 0 );
    my $score       = [*] @sums;
    my $calories    = @properties[*-1].sum;

    return {score => $score, calories => $calories};
}


# ------------------------------------------------------------------------------
# Main


sub MAIN {
    my @ingredients = parse-input($*IN.lines);               say '✓ 1';
    my @mixes       = possible-mixes(@ingredients);          say '✓ 2';
    my @scores      = @mixes.map: {score(@ingredients, $_)}; say '✓ 3';
    my @healthyish  = @scores.grep(*<calories> == 500);      say '✓ 4';

    say '';
    say '--- Part One ---';
    say @scores.map(*<score>).max;
    say '';
    say '--- Part Two ---';
    say @healthyish.map(*<score>).max;
}
