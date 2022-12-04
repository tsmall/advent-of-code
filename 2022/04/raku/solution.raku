use v6;

grammar SectionAssignmentPair {
    token TOP {
        <first-start> '-' <first-end> ',' <second-start> '-' <second-end>
    }

    token first-start  { \d+ }
    token first-end    { \d+ }
    token second-start { \d+ }
    token second-end   { \d+ }
}

sub MAIN() {
    my $fully-contained-count = 0;
    my $overlap-count         = 0;

    for $*IN.lines -> $line {
        my $parts = SectionAssignmentPair.parse($line);

        # NOTE: We depend on the start being before the end.
        die unless $parts<first-start>  ≤ $parts<first-end>;
        die unless $parts<second-start> ≤ $parts<second-end>;

        my $is-fully-contained = (
            (($parts<first-start> ≤ $parts<second-start>) &&
             ($parts<first-end>   ≥ $parts<second-end>))
            ||
            (($parts<second-start> ≤ $parts<first-start>) &&
             ($parts<second-end>   ≥ $parts<first-end>))
        );
        $fully-contained-count++ if $is-fully-contained;

        my $partially-overlaps = (
            (($parts<first-start> ≥ $parts<second-start>) &&
             ($parts<first-start> ≤ $parts<second-end>))
            ||
            (($parts<first-end> ≥ $parts<second-start>) &&
             ($parts<first-end> ≤ $parts<second-end>))
        );
        $overlap-count++ if $partially-overlaps || $is-fully-contained;
    }

    say "Part 1: $fully-contained-count";
    say "Part 2: $overlap-count";
}
