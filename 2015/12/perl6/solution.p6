use v6;
use JSON::Tiny;


sub MAIN() {
    my $json = $*IN.get;

    say '--- Part One ---';
    say sum-numbers(in => $json);

    say '';

    say '--- Part Two ---';
    my $data = from-json($json);
    my @numbers = find-numbers($data);
    say @numbers.sum;
}


sub sum-numbers(Str :in($string) --> Int) {
    my $matches = $string.match: /('-'? \d+)/, :global;
    my $numbers = $matches.map(*.Int);
    return $numbers.sum;
}


multi sub find-numbers(%hash) {
    # Skip any hashes that have a value of 'red'
    return 0 if %hash.values ∋ 'red';

    return %hash.values.flatmap: -> $each {
        find-numbers($each)
    };
}

multi sub find-numbers(@array) {
    return @array.flatmap: -> $each {
        find-numbers($each)
    };
}

multi sub find-numbers($int where $int ~~ Int) {
    return $int;
}

multi sub find-numbers($string) {
    return 0;
}
