use v6;

class Blueprint {
    has Int $.ore-ore-cost;
    has Int $.cly-ore-cost;
    has Int $.obs-ore-cost;
    has Int $.obs-cly-cost;
    has Int $.geo-ore-cost;
    has Int $.geo-obs-cost;

    has Int $.max-ore-cost;
    has Int $.max-cly-cost;
    has Int $.max-obs-cost;
}

class State {
    has Int $.ore-count is rw = 0;
    has Int $.cly-count is rw = 0;
    has Int $.obs-count is rw = 0;
    has Int $.geo-count is rw = 0;

    has Int $.ore-bots is rw = 1;
    has Int $.cly-bots is rw = 0;
    has Int $.obs-bots is rw = 0;
    has Int $.geo-bots is rw = 0;
}

sub MAIN {
    my @blueprints := parse-blueprints();
    my @qualities := [];
    my @geos := [];

    for 1..∞ Z @blueprints -> ($id, $blueprint) {
        @qualities.push: start {
            my $state = State.new;

            my %most-geo-so-far;
            my $geo = most-geo(24, $blueprint, $state, %most-geo-so-far);
            my $quality = $geo * $id;

            $quality
        }
    }

    for 1..∞ Z @blueprints.head(3) -> ($id, $blueprint) {
        @geos.push: start {
            my $state = State.new;

            my %most-geo-so-far;
            my $geo = most-geo(32, $blueprint, $state, %most-geo-so-far);

            $geo
        }
    }

    my $quality-sum = [+] await @qualities;
    my $geo-product = [*] await @geos;

    say "Part 1: $quality-sum";
    say "Part 2: $geo-product";
}

sub most-geo(Int $t, Blueprint $b, State $s, %most-geo-so-far) {
    return $s.geo-count if $t == 0;

    # Prune this branch if it doesn't beat the best seen so far (with +1 tolerance).
    return $s.geo-count if $s.geo-count + 1 < (%most-geo-so-far{$t+1} // 0);
    %most-geo-so-far{$t+1} max= $s.geo-count;

    my $best = $s.geo-count;

    # Buy geo bots if possible.
    if $s.obs-bots {
        my $ns = $s.clone;
        my $nt = $t;

        while $nt > 0 {
            $nt -= 1;
            if $ns.ore-count ≥ $b.geo-ore-cost && $ns.obs-count ≥ $b.geo-obs-cost {
                tick $ns;
                $ns.ore-count -= $b.geo-ore-cost;
                $ns.obs-count -= $b.geo-obs-cost;
                $ns.geo-bots += 1;
                $best max= most-geo($nt, $b, $ns, %most-geo-so-far);
                last;
            }
            tick $ns;
        }
    }

    # Buy obs bots if possible and worth it.
    if $s.cly-bots && $s.obs-bots < $b.max-obs-cost {
        my $ns = $s.clone;
        my $nt = $t;

        while $nt > 0 {
            $nt -= 1;
            if $ns.ore-count ≥ $b.obs-ore-cost && $ns.cly-count ≥ $b.obs-cly-cost {
                tick $ns;
                $ns.ore-count -= $b.obs-ore-cost;
                $ns.cly-count -= $b.obs-cly-cost;
                $ns.obs-bots += 1;
                $best max= most-geo($nt, $b, $ns, %most-geo-so-far);
                last;
            }
            tick $ns;
        }
    }

    # Buy cly bots if worth it.
    if $s.cly-bots < $b.max-cly-cost {
        my $ns = $s.clone;
        my $nt = $t;

        while $nt > 0 {
            $nt -= 1;
            if $ns.ore-count ≥ $b.cly-ore-cost {
                tick $ns;
                $ns.ore-count -= $b.cly-ore-cost;
                $ns.cly-bots += 1;
                $best max= most-geo($nt, $b, $ns, %most-geo-so-far);
                last;
            }
            tick $ns;
        }
    }

    # Buy ore bots if worth it.
    if $s.ore-bots < $b.max-ore-cost {
        my $ns = $s.clone;
        my $nt = $t;

        while $nt > 0 {
            $nt -= 1;
            if $ns.ore-count ≥ $b.ore-ore-cost {
                tick $ns;
                $ns.ore-count -= $b.ore-ore-cost;
                $ns.ore-bots += 1;
                $best max= most-geo($nt, $b, $ns, %most-geo-so-far);
                last;
            }
            tick $ns;
        }
    }

    return $best;
}

sub tick(State $s) {
    $s.ore-count += $s.ore-bots;
    $s.cly-count += $s.cly-bots;
    $s.obs-count += $s.obs-bots;
    $s.geo-count += $s.geo-bots;
}

grammar InputLine {
    token TOP {
        'Blueprint ' \d+ ': '
        'Each ore robot costs ' <ore-ore-cost=.num> ' ore. '
        'Each clay robot costs ' <cly-ore-cost=.num> ' ore. '
        'Each obsidian robot costs ' <obs-ore-cost=.num> ' ore '
        'and ' <obs-cly-cost=.num> ' clay. '
        'Each geode robot costs ' <geo-ore-cost=.num> ' ore '
        'and ' <geo-obs-cost=.num> ' obsidian.'
    }

    token num { \d+ }
}

sub parse-blueprints {
    my @result;
    for $*IN.lines {
        my $parsed = InputLine.parse($_);
        @result.push: Blueprint.new(
            ore-ore-cost => +$parsed<ore-ore-cost>,
            cly-ore-cost => +$parsed<cly-ore-cost>,
            obs-ore-cost => +$parsed<obs-ore-cost>,
            obs-cly-cost => +$parsed<obs-cly-cost>,
            geo-ore-cost => +$parsed<geo-ore-cost>,
            geo-obs-cost => +$parsed<geo-obs-cost>,
            max-ore-cost => +max($parsed<ore-ore-cost cly-ore-cost obs-ore-cost geo-obs-cost>),
            max-cly-cost => +$parsed<obs-cly-cost>,
            max-obs-cost => +$parsed<geo-obs-cost>,
        );
    }
    return @result;
}
