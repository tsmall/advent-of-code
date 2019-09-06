use v6;


# ------------------------------------------------------------------------------
# Rules for Part One

my regex Vowel         { <[aeiou]> };
my regex ThreeVowels   { <Vowel> .* <Vowel> .* <Vowel> };
my regex DoubledLetter { (<:Letter>) $0 };
my regex Forbiddens    { ab | cd | pq | xy };

sub is-nice-by-part-one-rules(Str $string --> Bool:D) {
    $string ~~ (&ThreeVowels & &DoubledLetter) and $string !~~ &Forbiddens;
}


# ------------------------------------------------------------------------------
# Rules for Part Two

my regex PairTwice { (..) .* $0 };
my regex RepeatedWithSeparator { (.) . $0 };

sub is-nice-by-part-two-rules(Str $string --> Bool:D) {
    $string ~~ (&PairTwice & &RepeatedWithSeparator);
}


# ------------------------------------------------------------------------------
# Input / output

my $part-one-nice-count = 0;
my $part-two-nice-count = 0;

for $*IN.lines -> $line {
    if $line.&is-nice-by-part-one-rules {
        $part-one-nice-count++;
    }

    if $line.&is-nice-by-part-two-rules {
        $part-two-nice-count++;
    }
}

say "--- Part One ---";
say $part-one-nice-count;

say "";
say "--- Part Two ---";
say $part-two-nice-count;
