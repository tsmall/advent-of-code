use v6;

class PacketIter {
    has @.chars;

    has $.index = 0;
    has $.last = Nil;
    has $.queue = [];

    method next {
        return $.queue.shift if $.queue;

        # Skip commas ...
        ++$!index if self.char eq ',';

        my $result = 0;
        if self.char eq ('['|']') {
            $result = self.char;
            ++$!index;
        } else {
            $result = +self.char;
            loop {
                ++$!index;
                last if self.char !~~ '0'..'9';
                $result *= 10;
                $result += self.char;
            }
        }

        $!last = $result;
        return $result;
    }

    method char {
        return @.chars[$.index];
    }

    method promote {
        $.queue.unshift(']');
        $.queue.unshift($.last);
    }
}

class Packet {
    has $.string;

    method new($string) {
        return self.bless(:$string);
    }

    method iter {
        return PacketIter.new(chars => $.string.comb);
    }
}

sub MAIN {
    my $part1-answer = 0;
    my @packets;

    for 1..* Z $*IN.lines.batch(3) -> ($index, $batch) {
        my $l = Packet.new($batch[0]);
        my $r = Packet.new($batch[1]);

        @packets.push($l);
        @packets.push($r);

        my $result = $l cmp $r;
        $part1-answer += $index if $result eqv Less;
    }

    my @dividers = Packet.new('[[2]]'), Packet.new('[[6]]');
    @packets.push(|@dividers);

    my $part2-answer = 1;
    for 1..* Z @packets.sort(&infix:<cmp>) -> ($index, $packet) {
        $part2-answer *= $index if $packet ∈ @dividers;
    }

    say "Part 1: $part1-answer";
    say "Part 2: $part2-answer";
}

multi sub infix:<cmp>(Packet $l, Packet $r) {
    my $l-iter = $l.iter;
    my $r-iter = $r.iter;

    loop {
        given ($l-iter.next, $r-iter.next) {
            when ('[', '[') {}
            when (']', ']') {}
            when (']', Any) { return Less }
            when (Any, ']') { return More }
            when ('[', Int) { $r-iter.promote }
            when (Int, '[') { $l-iter.promote }
            when (Int, Int) {
                if $_[0] < $_[1] { return Less }
                if $_[0] > $_[1] { return More }
            }
        }
    }
}
