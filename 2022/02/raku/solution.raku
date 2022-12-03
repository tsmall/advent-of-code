use v6;

enum Play (
    rock     => 1,
    paper    => 2,
    scissors => 3,
);

enum Outcome (
    lose => 0,
    draw => 3,
    win  => 6,
);

sub MAIN() {
    my $incorrect-total-score = 0;
    my $correct-total-score   = 0;

    for $*IN.lines -> $line {
        my @words = $line.words.list;
        $incorrect-total-score += calculate-score-incorrectly(@words);
        $correct-total-score   += calculate-score-correctly(@words);
    }

    say "Part 1: $incorrect-total-score";
    say "Part 2: $correct-total-score";
}

sub calculate-score-incorrectly(@words) {
    (my Play $them, my Play $me) = @words».&decrypt-incorrectly;
    return score($me, outcome($them, $me));
}

sub calculate-score-correctly(@words) {
    (my Play $them, my Outcome $outcome) = @words».&decrypt-correctly;
    return score(play-needed($them, $outcome), $outcome);
}

sub decrypt-incorrectly(Str $char --> Play) {
    given $char {
        when 'A'|'X' { rock     }
        when 'B'|'Y' { paper    }
        when 'C'|'Z' { scissors }
    }
}

sub decrypt-correctly(Str $char) {
    given $char {
        when 'A' { rock     }
        when 'B' { paper    }
        when 'C' { scissors }
        when 'X' { lose     }
        when 'Y' { draw     }
        when 'Z' { win      }
    }
}

sub outcome(Play $them, Play $me --> Outcome) {
    given $them, $me {
        when rock,     paper    { win  }
        when paper,    scissors { win  }
        when scissors, rock     { win  }
        when rock,     scissors { lose }
        when paper,    rock     { lose }
        when scissors, paper    { lose }
        default                 { draw }
    }
}

sub play-needed(Play $them, Outcome $outcome --> Play) {
    given $them, $outcome {
        when rock,     lose     { scissors }
        when rock,     win      { paper    }
        when paper,    lose     { rock     }
        when paper,    win      { scissors }
        when scissors, lose     { paper    }
        when scissors, win      { rock     }
        default                 { $them    }
    }
}

sub score(Play $me, Outcome $outcome --> Int) {
    return $me.value + $outcome.value;
}
