use v6;

my regex Vowel         { <[aeiou]> };
my regex ThreeVowels   { ^ .* <Vowel> .* <Vowel> .* <Vowel> .* $ };
my regex DoubledLetter { (<:Letter>) $0 };
my regex Forbiddens    { ab | cd | pq | xy };

sub is-nice(Str $string --> Bool:D) {
    $string ~~ (&ThreeVowels & &DoubledLetter) and $string !~~ &Forbiddens;
}

say $*IN.lines.grep(&is-nice).elems;
