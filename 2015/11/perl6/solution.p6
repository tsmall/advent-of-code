use v6;


sub has-increasing-straight(Str:D $password --> Bool:D) {
    state $alphabet     = ('a' .. 'z').join;
    state $straight-run = $alphabet.match(/. ** 3/, :overlap).any;

    for $password.match(/. ** 3/, :overlap) -> $straight {
        return True if $straight eq $straight-run;
    }
    return False;
}


sub has-invalid-letter(Str:D $password --> Bool:D) {
    return so $password ~~ /i|o|l/;
}


sub has-non-overlapping-pairs(Str:D $password --> Bool:D) {
    my $matches = $password.match: /(.) $0/, :global;
    return $matches.elems >= 2;
}


sub is-valid(Str:D $password --> Bool:D) {
    return (
        $password.&has-increasing-straight
        and not $password.&has-invalid-letter
        and $password.&has-non-overlapping-pairs
    );
}


sub next-candidate(Str:D $password --> Str:D) {
    my $next = $password.succ;
    return $next.chars == 8 ?? $next !! $next.chop;
}


sub skip-invalid-letter(Str:D $password --> Str:D) {
    return S/(i|o|l) (.*) $/{$0.Str.succ}{'a' x $1.Str.chars}/ with $password;
}


sub next-password(Str:D $password --> Str:D) {
    my $p = next-candidate($password);
    until $p.&is-valid {
        $p = next-candidate($p);
        if $p.&has-invalid-letter {
            $p = skip-invalid-letter($p);
        }
    }
    return $p;
}


# ------------------------------------------------------------------------------
# Timing


sub prefix:<timed> (Code $block --> Any) {
    role Timed {
        # Store the actual timing...
        has Duration $.timing;

        # When stringified, append the timing...
        my constant FORMAT = "%-20s  [%.3f𝑠]";
        method Str  { sprintf FORMAT, callsame, $.timing }
        method gist { self.Str }
    }

    # Evaluate the block, mixing timing info into result...
    return $block() but Timed(now - ENTER now);
}


# ------------------------------------------------------------------------------
# Main


my $expired-password = 'hepxcrrq';

say '--- Part One ---';
my $first-new-password = timed { next-password($expired-password) };
say $first-new-password;

say '';

say '--- Part Two ---';
my $second-new-password = timed { next-password($first-new-password) };
say $second-new-password;
