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


for $*IN.lines -> $line {
    my $match = $line ~~ &CodeString;

    $memory-count += $line.chars;
    $string-count += $match[0].elems;

    say "STR: $line";
    say "RAW: {$line.chars}";
    say "ACT: {$match[0].elems}";

    say "\n", $match[0], "\n";
}


say '--- Part One ---';
say '';
say "Memory Total: $memory-count";
say "String Count: $string-count";
say "Answer: ", $memory-count - $string-count;
