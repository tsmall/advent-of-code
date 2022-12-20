use v6;

sub MAIN {
    my %rooms     := parse-input();
    my %distances := distances-between-rooms(%rooms);
    my $remaining := %rooms.keys.grep({ %rooms{$_}<flow> != 0 }).Set;

    my $max-flow = max-flow(:%rooms, :%distances, :$remaining,
                            :room('AA'), :time(30));

    say "Part 1: $max-flow";
    say "Part 2: TODO";
}

sub max-flow(:%rooms, :%distances, :$remaining, :$room, :$time, :$flow = 0) {
    # state $indent = 0;

    if $time ≤ 0 {
        # print(' ') for ^$indent;
        # say "state=out-of-time time=$time flow=$flow";
        # $indent -= 2;
        return $flow;
    }

    # print(' ') for ^$indent;
    # say "state=entering room=$room";
    # $indent += 2;

    my $max-flow = $flow;
    for $remaining.keys -> $destination {
        my $distance = %distances{$room}{$destination};
        my $time-after = $time - $distance - 1;
        next if $time-after ≤ 0;

        my %destination-room = %rooms{$destination};
        my $flow-after = $flow + (%destination-room<flow> * $time-after);
        my $remaining-after = $remaining ∖ $destination;

        $max-flow max= max-flow(
            rooms     => %rooms,
            distances => %distances,
            remaining => $remaining-after,
            room      => $destination,
            time      => $time-after,
            flow      => $flow-after,
        );
    }

    # print(' ') for ^$indent;
    # say "state=returning flow=$flow";
    # $indent -= 2;
    return $max-flow;
}

sub distances-between-rooms(%rooms) {
    my %result;
    for %rooms.keys -> $source {
        for %rooms.keys -> $target {
            %result{$source}{$target} = distance(%rooms, :$source, :$target);
        }
    }
    return %result;
}

sub distance(%rooms, :$source, :$target) {
    my %room = %rooms{$source};
    my @next = %room<tunnels>.values;
    my $steps = 1;
    loop {
        return $steps if $target ∈ @next;
        @next = @next.map({ %rooms{$_}<tunnels>.values }).flat.unique;
        ++$steps;
    }
}

grammar InputLine {
    token TOP {
        'Valve ' <name> ' has flow rate=' <flow> ';'
        ' tunnel' 's'?
        ' lead' 's'?
        ' to valve' 's'?
        ' ' <tunnels>
    }

    token name { \w\w }
    token flow { \d+  }
    token tunnels { [ \w\w ', '? ]+ }
}

sub parse-input {
    my %rooms;
    for $*IN.lines -> $line {
        my $result  := InputLine.parse($line);
        my $name    := $result<name>;
        my $flow    := +$result<flow>;
        my @tunnels := $result<tunnels>.split(', ').List;

        %rooms{$name} = {:$flow, :@tunnels};
    }
    return %rooms;
}
