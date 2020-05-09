enum Part <Part1 Part2>;
enum Character <Player Boss>;
class Stats {...};

sub MAIN(Part $part) {
    my $boss = parse-input();
    run($part, $boss);
}

multi sub run(Part1, Stats $boss-stats) {
    my @costs = gather for item-permutations() -> @items {
        my $player = build-player(@items);
        my $boss   = $boss-stats.clone();

        if winner(:$player, :$boss) == Player {
            take @items.map({.cost}).sum;
        }
    }

    say '--- Part One ---';
    say @costs.min;
}

multi sub run(Part2, Stats $boss-stats) {
    my @costs = gather for item-permutations() -> @items {
        my $player = build-player(@items);
        my $boss   = $boss-stats.clone();

        if winner(:$player, :$boss) == Boss {
            take @items.map({.cost}).sum;
        }
    }

    say '--- Part Two ---';
    say @costs.max;
}

# Fighting ---------------------------------------------------------------------

class Stats {
    has Int $.hit-points;
    has Int $.damage;
    has Int $.armor;

    method dead(--> Bool) {
        return $.hit-points <= 0;
    }

    method attack(Stats $other) {
        $other.take-damage($.damage);
    }

    method take-damage(Int $damage) {
        $!hit-points -= ($damage - $.armor) max 1;
    }
}

sub winner(Stats :$player, Stats :$boss --> Character) {
    my Character $turn = Player;

    until $player.dead or $boss.dead {
        given $turn {
            when Player { $player.attack($boss); }
            when Boss   { $boss.attack($player); }
        }
        $turn = $turn == Player ?? Boss !! Player;
    }

    return $player.dead ?? Boss !! Player;
}

sub build-player(@items --> Stats) {
    my Int $damage = @items.map({.damage}).sum;
    my Int $armor  = @items.map({.armor}).sum;

    return Stats.new(:hit-points(100), :$damage, :$armor);
}

# Shopping ---------------------------------------------------------------------

class Item {
    has Str $.name;
    has Int $.cost;
    has Int $.damage;
    has Int $.armor;
}

constant @weapons = (
    Item.new(:name<Dagger>,     :cost(8),  :damage(4), :armor(0)),
    Item.new(:name<Shortsword>, :cost(10), :damage(5), :armor(0)),
    Item.new(:name<Warhammer>,  :cost(25), :damage(6), :armor(0)),
    Item.new(:name<Longsword>,  :cost(40), :damage(7), :armor(0)),
    Item.new(:name<Greataxe>,   :cost(74), :damage(8), :armor(0)),
);

constant @armor = (
    Item.new(:name<Leather>,    :cost(13),  :damage(0), :armor(1)),
    Item.new(:name<Chainmail>,  :cost(31),  :damage(0), :armor(2)),
    Item.new(:name<Splintmail>, :cost(53),  :damage(0), :armor(3)),
    Item.new(:name<Bandedmail>, :cost(75),  :damage(0), :armor(4)),
    Item.new(:name<Platemail>,  :cost(102), :damage(0), :armor(5)),
);

constant @rings = (
    Item.new(:name('Damage +1'),  :cost(25),  :damage(1), :armor(0)),
    Item.new(:name('Damage +2'),  :cost(50),  :damage(2), :armor(0)),
    Item.new(:name('Damage +3'),  :cost(100), :damage(3), :armor(0)),
    Item.new(:name('Defense +1'), :cost(20),  :damage(0), :armor(1)),
    Item.new(:name('Defense +2'), :cost(40),  :damage(0), :armor(2)),
    Item.new(:name('Defense +3'), :cost(80),  :damage(0), :armor(3)),
);

sub item-permutations(--> Seq) {
    return gather {
        for @weapons -> $weapon {
            take ($weapon,);

            for @armor -> $armor {
                take $weapon, $armor;

                for @rings -> $ring {
                    take $weapon, $armor, $ring;
                }

                for @rings.combinations(2) -> @rings {
                    take $weapon, $armor, |@rings;
                }
            }

            for @rings -> $ring {
                take $weapon, $ring;
            }

            for @rings.combinations(2) -> @rings {
                take $weapon, |@rings;
            }
        }
    }
}

# Input Parsing ----------------------------------------------------------------

grammar Input {
    token TOP {
        <hit-points> \s+
        <damage> \s+
        <armor> .*
    }

    token hit-points { 'Hit Points: ' <number> }
    token damage     { 'Damage: ' <number> }
    token armor      { 'Armor: ' <number> }

    token number { \d+ }
}

class InputActions {
    method TOP ($/) {
        my Int $hit-points = $<hit-points>.made;
        my Int $damage     = $<damage>.made;
        my Int $armor      = $<armor>.made;

        make Stats.new(:$hit-points, :$damage, :$armor);
    }

    method hit-points ($/) { make $<number>.made; }
    method damage ($/)     { make $<number>.made; }
    method armor ($/)      { make $<number>.made; }

    method number ($/) { make +$/; }
}

sub parse-input(--> Stats) {
    my $actions = InputActions.new;
    return Input.parse($*IN.slurp, :$actions).made;
}
