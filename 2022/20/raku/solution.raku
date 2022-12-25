use v6;

class Node {
    has Int  $.value;
    has Node $.next is rw = Nil;
    has Node $.prev is rw = Nil;
}

class List {
    has Node @.nodes;
    has Node $.head;
    has Int  $.elems;
    has Node $!zero = Nil;

    method new(:@nodes) {
        return self.bless(
            nodes => @nodes,
            head  => @nodes.head,
            elems => @nodes.elems,
        );
    }

    method mix {
        self.move($_) for @.nodes;
    }

    method move(Node $n) {
        return if $n.value == 0;

        $!head = $.head.next if $n === $.head;

        # Remove before iterating.
        $n.prev.next = $n.next;
        $n.next.prev = $n.prev;

        my $t = self!advance($n, $n.value);

        # Insert self into new position.
        $t.next.prev = $n;
        $n.next = $t.next;
        $t.next = $n;
        $n.prev = $t;
    }

    method !advance(Node $n, Int $times) {
        my $steps := do given $times {
            when * < 0 { $times % (($.elems - 1) * -1) }
            when * > 0 { $times % ($.elems - 1) }
        };

        my $target = $n;
        if $steps > 0 {
            $target = $target.next for 1..$steps;
        } else {
            $target = $target.prev for $steps..0;
        }
        return $target;
    }

    method AT-POS($index) {
        my $target = $!zero;
        if !$target {
            $target = $.head;
            $target = $target.next while $target.value != 0;
            $!zero = $target;
        }

        $target = $target.next for ^$index;
        return $target.value;
    }
}

sub MAIN {
    my @nums := $*IN.lines».Int;

    my $part1 = start {
        my @nodes := parse-nodes(@nums);
        my $list := List.new(:@nodes);
        $list.mix;
        [+] $list[1000, 2000, 3000]
    }

    my $part2 = start {
        my @nodes := parse-nodes(@nums, multiplier => 811589153);
        my $list := List.new(:@nodes);
        $list.mix for ^10;
        [+] $list[1000, 2000, 3000]
    }

    my $sum1 = await $part1;
    say "Part 1: $sum1";

    my $sum2 = await $part2;
    say "Part 2: $sum2";
}

sub parse-nodes(@nums, :$multiplier = 1) {
    my @nodes := [];

    for @nums {
        @nodes.push: Node.new(:value($_ * $multiplier));
    }

    my $prev = @nodes[*-1];
    for ^∞ Z @nodes -> ($i, $n) {
        $n.prev = $prev;
        $n.next = @nodes[($i+1) % @nodes.elems];
        $prev = $n;
    }

    return @nodes.List;
}
