use v6;

my $current-floor = 0;
my $current-position = 0;

while not $*IN.eof {
    $current-position++;

    given $*IN.getc {
        when '(' { $current-floor++ }
        when ')' { $current-floor-- }
    }

    if $current-floor < 0 {
        once { say "First entered basement at position $current-position"; }
    }
}

say "Ended on floor $current-floor";
