use v6;

sub MAIN {
    my %monkeys := parse-input();

    my $part1 := yelled(%monkeys, 'root');
    my $part2 := needed(%monkeys, 'root');

    say "Part 1: $part1";
    say "Part 2: $part2";
}

class Monkey {
    has $.number       = Nil;
    has @.dependencies = Nil;
    has $.operation    = Nil;
}

sub yelled(%monkeys, $name) {
    constant \OPS = {
        '+' => &infix:<+>,
        '-' => &infix:<->,
        '*' => &infix:<*>,
        '/' => &infix:</>,
    };

    given %monkeys{$name} {
        return .number if .number.defined;

        my @nums = .dependencies.map: { yelled(%monkeys, $^name) };
        my &op = OPS{.operation};
        return @nums[0] [&op] @nums[1];
    }
}

sub needed(%monkeys, $name) {
    my $next;
    my $needed;

    my ($l, $r) = %monkeys{$name}.dependencies;
    if depends-on-humn(%monkeys, $l) {
        $needed = yelled(%monkeys, $r);
        $next   = $l;
    } else {
        $needed = yelled(%monkeys, $l);
        $next   = $r;
    }

    until $next eq 'humn' {
        ($l, $r) = %monkeys{$next}.dependencies;
        my $op   = %monkeys{$next}.operation;
        my &op   = OPS{$op};
        if depends-on-humn(%monkeys, $l) {
            my $num = yelled(%monkeys, $r);
            $needed = do given $op {
                when '+' { $needed - $num }
                when '-' { $needed + $num }
                when '*' { $needed / $num }
                when '/' { $needed * $num }
            };
            $next   = $l;
        } else {
            my $num = yelled(%monkeys, $l);
            $needed = do given $op {
                when '+' { $needed - $num }
                when '-' { ($needed - $num) * -1 }
                when '*' { $needed / $num }
                when '/' { (1/$needed) * $num }
            };
            $next   = $r;
        }
    }

    return $needed;
}

sub depends-on-humn(%monkeys, $name) {
    return True if $name eq 'humn';

    my $monkey  = %monkeys{$name};
    return False if $monkey.number.defined;

    my ($l, $r) = $monkey.dependencies;
    return (
        ('humn' ∈ $monkey.dependencies)
        || depends-on-humn(%monkeys, $l)
        || depends-on-humn(%monkeys, $r);
    );
}

sub parse-input {
    my %monkeys;
    for $*IN.lines -> $line {
        when $line ~~ /(\w+) ': ' (\d+)/ {
            my $name   := ~$/[0];
            my $number := +$/[1];
            %monkeys{$name} = Monkey.new(:$number);
        }
        when $line ~~ /(\w+) ': ' (\w+) ' ' (.) ' ' (\w+)/ {
            my $name         := ~$/[0];
            my @dependencies := (~$/[1], ~$/[3]);
            my $operation    := ~$/[2];
            %monkeys{$name} = Monkey.new(:@dependencies, :$operation);
        }
    }
    return %monkeys;
}
