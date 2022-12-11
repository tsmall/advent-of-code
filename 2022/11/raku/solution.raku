use v6;

enum Op <sqrt mul add>;

class Monkey {
    has @.items;
    has $.operation;
    has $.operand;
    has $.divisor;
    has $.true-monkey;
    has $.false-monkey;
    has $.inspection-count is rw = 0;

    method clone {
        nextwith items => @.items.clone, |%_;
    }
}

sub MAIN() {
    my @part1-monkeys = parse-input();
    my @part2-monkeys = @part1-monkeys».clone;

    simulate(monkeys => @part1-monkeys, worry-divisor => 3, rounds => 20);
    simulate(monkeys => @part2-monkeys, worry-divisor => 1, rounds => 10_000);

    my $part1-monkey-business-level = [*] @part1-monkeys».inspection-count.sort.tail(2);
    my $part2-monkey-business-level = [*] @part2-monkeys».inspection-count.sort.tail(2);

    say "Part 1: $part1-monkey-business-level";
    say "Part 2: $part2-monkey-business-level";
}

sub simulate(:@monkeys, :$worry-divisor, :$rounds) {
    my $worry-mod = [*] @monkeys».divisor;
    for 1 .. $rounds {
        for @monkeys -> $monkey {
            $monkey.inspection-count += $monkey.items.elems;
            while $monkey.items.elems > 0 {
                my $worry-level = $monkey.items.shift;
                if    $monkey.operation eqv Op::sqrt { $worry-level *= $worry-level }
                elsif $monkey.operation eqv Op::mul  { $worry-level *= $monkey.operand }
                elsif $monkey.operation eqv Op::add  { $worry-level += $monkey.operand }
                $worry-level div= $worry-divisor;
                $worry-level %= $worry-mod;

                my $receiver = $worry-level %% $monkey.divisor
                                ?? $monkey.true-monkey
                                !! $monkey.false-monkey;
                @monkeys[$receiver].items.push($worry-level);
            }
        }
    }
}

sub parse-input() {
    my @monkeys;

    loop {
        my $block = $*IN.lines.head(7).Array;
        last if !$block;
        @monkeys.push: parse-block($block);
    }

    return @monkeys;
}

sub parse-block($lines) {
    my $operation = $lines[2].comb()[23] eq '+' ?? Op::add !! Op::mul;
    my $operand   = do given $lines[2].substr(25) { $_ eq 'old' ?? 0 !! +$_ };
    $operation = Op::sqrt if $operand == 0;

    return Monkey.new(
        items        => $lines[1].substr(18).split(', ')».Int,
        operation    => $operation,
        operand      => $operand,
        divisor      => +$lines[3].substr(21),
        true-monkey  => +$lines[4].substr(29),
        false-monkey => +$lines[5].substr(30),
    );
}
