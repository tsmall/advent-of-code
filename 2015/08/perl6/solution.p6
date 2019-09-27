use v6;


my token CodeString {
    '"'
    (
        || \w
        || \\\\
        || \\'"'
        || \\x <[ 0..9 a..f ]>**2
    )*
    '"'
};


my $memory-count = 0;
my $string-count = 0;
my $encode-count = 0;


for $*IN.lines -> $line {
    my $match = $line ~~ &CodeString;
    my $escape-match = $line ~~ m:global/'"' || '\\'/;

    $memory-count += $line.chars;
    $string-count += $match[0].elems;
    my $enc-len = $line.chars + $escape-match.elems + 2;
    $encode-count += $enc-len;

    say "STR: $line";
    say "RAW: {$line.chars}";
    say "ACT: {$match[0].elems}";
    say "ENC: {$enc-len}";

    say "\n", $match[0], "\n";
    say $escape-match, "\n";
}


say '--- Part One ---';
say '';
say "Memory Total: $memory-count";
say "String Count: $string-count";
say "Answer: ", $memory-count - $string-count;
say '';
say '--- Part Two ---';
say '';
say "Memory Total: $memory-count";
say "Encode Count: $encode-count";
say "Answer: ", $encode-count - $memory-count;
