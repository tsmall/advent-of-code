constant $input = 29_000_000;
constant $goal  = $input div 10;

enum Parts <Part1 Part2>;

sub MAIN(Parts $part) {
    given $part {
        when Part1 {
            say '--- Part One ---';
            say part-one;
        }
        when Part2 {
            say '--- Part Two ---';
            say part-two;
        }
    }
}

sub part-one(--> Int) {
    my Int @sums[$goal] = 0 xx $goal;
    my Int $housenum = $goal;
    my Int $j;

    for 1 .. $goal -> $i {
        loop ($j = $i; $j < $goal; $j += $i) {
            @sums[$j] += $i;
            $housenum = $j if $j < $housenum and @sums[$j] >= $goal;
        }
    }

    return $housenum;
}

sub part-two(--> Int) {
    my Int @sums[$input] = 0 xx $input;
    my Int $housenum = $input;
    my Int $j;

    for 1 .. $input -> $i {
        loop ($j = $i; $j <= $i * 50 and $j < $input; $j += $i) {
            @sums[$j] += $i * 11;
            $housenum = $j if $j < $housenum and @sums[$j] >= $input;
        }
    }

    return $housenum;
}
