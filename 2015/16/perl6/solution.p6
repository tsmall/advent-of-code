use v6;


# ------------------------------------------------------------------------------
# Model


class Aunt {
    has $.number;
    has %.details;
}


# ------------------------------------------------------------------------------
# Parsing


grammar Parser {
    token TOP {
        'Sue ' <number> ': '
        [<key> ': ' <value=.number> ', '?]+
    }

    token number { \d+ }
    token key    { \w+ }
}


class ParserActions {
    method TOP ($/) {
        my Str @keys   = $<key>.map(~*);
        my Int @values = $<value>.map(+*);

        make Aunt.new(
            number  => $<number>.made,
            details => (@keys Z=> @values),
        );
    }

    method number ($/) { make +$/; }
}


sub parse-input(@lines) {
    my $actions = ParserActions.new;
    return @lines.map: {Parser.parse($_, actions => $actions).made};
}


# ------------------------------------------------------------------------------
# Main


constant %message = {
    children    => 3,
    cats        => 7,
    samoyeds    => 2,
    pomeranians => 3,
    akitas      => 0,
    vizslas     => 0,
    goldfish    => 5,
    trees       => 3,
    cars        => 2,
    perfumes    => 1,
};


sub is-exact-match(Aunt $aunt --> Bool) {
    for $aunt.details.kv -> $k, $v {
        return False if %message{$k} != $v;
    }
    return True;
}


sub is-rough-match(Aunt $aunt --> Bool) {
    for $aunt.details.kv -> $k, $v {
        if $k eq ('cats'|'trees') {
            return False if $v <= %message{$k};
        }
        elsif $k eq ('pomeranians'|'goldfish') {
            return False if $v >= %message{$k};
        }
        else {
            return False if %message{$k} != $v;
        }
    }
    return True;
}


sub MAIN {
    my @aunts = parse-input($*IN.lines);
    my $wrong = @aunts.first(&is-exact-match);
    my $right = @aunts.first(&is-rough-match);

    say '--- Part One ---';
    say $wrong.number;
    say '';
    say '--- Part Two ---';
    say $right.number;
}
