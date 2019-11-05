use v6;


# ------------------------------------------------------------------------------
# Data Model


class Reindeer {
    has Str $.name;
    has Int $.speed;
    has Int $.fly-time;
    has Int $.rest-time;
}


enum State <Active Resting>;


class ReindeerRun {
    has Reindeer $.reindeer;
    has Int      $.state-timer = 0;
    has Int      $.distance    = 0;
    has State    $.state       = Active;

    submethod TWEAK() {
        $!state-timer = $!reindeer.fly-time;
    }

    method tick() {
        $!state-timer--;
        $!distance += $.reindeer.speed if $.state == Active;
        self!change-state if $.state-timer == 0;
    }

    method !change-state() {
        if $.state == Active {
            $!state       = Resting;
            $!state-timer = $.reindeer.rest-time;
        } else {
            $!state       = Active;
            $!state-timer = $.reindeer.fly-time;
        }
    }
}


class ReindeerScorer {
    has BagHash $.scores;

    method score(@runs) {
        my $leader = @runs.max(*.distance);
        my $name   = $leader.reindeer.name;

        $!scores{$name}++;
    }

    method highest-score {
        return $.scores.values.max;
    }
}


# ------------------------------------------------------------------------------
# Input Parsing


grammar Parser {
    token TOP {
        <name> ' can fly ' <speed> ' km/s for ' <fly-time> ' seconds, '
        'but then must rest for ' <rest-time> ' seconds.'
    }

    token name      { \w+ }
    token speed     { \d+ }
    token fly-time  { \d+ }
    token rest-time { \d+ }
}


class ParserActions {
    method TOP ($/) {
        make Reindeer.new(
            name      => ~$<name>,
            speed     => +$<speed>,
            fly-time  => +$<fly-time>,
            rest-time => +$<rest-time>
        );
    }
}


sub parse-input($lines) {
    my $actions = ParserActions.new;
    return $lines.map: { parse-line($_, $actions) };
}


sub parse-line($line, $actions) {
    my $result = Parser.parse($line, actions => $actions);
    return $result.made;
}


# ------------------------------------------------------------------------------
# Main


sub MAIN(Int $seconds) {
    my @reindeer = parse-input($*IN.lines);
    my @runs     = @reindeer.map: { ReindeerRun.new(reindeer => $_) };
    my $scorer   = ReindeerScorer.new;

    for 1 .. $seconds {
        @runs».tick;
        $scorer.score(@runs);
    }

    say '--- Part One ---';
    say @runs».distance.max;
    say '';
    say '--- Part Two ---';
    say $scorer.highest-score;
}
