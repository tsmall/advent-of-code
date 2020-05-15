# Types ------------------------------------------------------------------------

enum Spell <MagicMissile Drain Shield Poison Recharge>;

class Effect {
    has Spell $.spell;
    has Int   $.turns-left;

    method ticked {
        return self.clone(turns-left => $.turns-left - 1);
    }
}

class GameState {
    has Int    $.player-hp      is rw;
    has Int    $.player-mana    is rw;
    has Int    $.player-armor   is rw = 0;
    has Int    $.mana-spent     is rw = 0;
    has Int    $.boss-hp        is rw;
    has Int    $.boss-damage;
    has Effect @.active-effects is rw = ();

    method clone { nextwith :active-effects(@!active-effects.clone), |%_ }
}


# Main -------------------------------------------------------------------------

enum Part <Part1 Part2>;

sub MAIN(Part $part) {
    run($part);
}

multi run(Part1) {
    my Int $least-mana-spent = 9_999_999;

    my $initial-state = GameState.new(
        player-hp   => 50,
        player-mana => 500,
        boss-hp     => 51,
        boss-damage => 9,
    );

    play-all-possibilities($initial-state, $least-mana-spent);

    say '--- Part One ---';
    say $least-mana-spent;
}

sub play-all-possibilities(
    GameState $current-state,
    Int       $least-mana-spent is rw,
    Int       $turn = 1
) {
    my @spells = MagicMissile, Drain, Shield, Poison, Recharge;

    for @spells -> $spell {
        with play-turn($current-state, $spell) -> $new-state {
            if $new-state.boss-hp <= 0 and $new-state.player-hp > 0 {
                if $new-state.mana-spent < $least-mana-spent {
                    $least-mana-spent = $new-state.mana-spent;
                }
            }

            if $new-state.player-hp > 0
            and $new-state.boss-hp > 0
            and $new-state.mana-spent < $least-mana-spent {
                play-all-possibilities(
                    $new-state,
                    $least-mana-spent,
                    $turn + 1
                );
            }
        }
    }
}


# Game logic -------------------------------------------------------------------

sub play-turn(GameState $state, Spell $chosen-spell --> GameState) {
    fail "Cannot cast spell" if not can-cast($state, $chosen-spell);

    my $new-state = $state.clone;

    # Player turn
    my Effect @effects = apply-effects($new-state, $new-state.active-effects);
    with cast-spell($new-state, $chosen-spell) -> $new-effect {
        push @effects, $new-effect;
    }

    # Boss turn
    $new-state.active-effects = apply-effects($new-state, @effects);
    if $new-state.boss-hp > 0 {
        boss-attack($new-state);
    }

    return $new-state;
}

sub can-cast(GameState $state, Spell $spell --> Bool) {
    return (
        cost($spell) <= $state.player-mana
        and
        $spell ∉ $state.active-effects.map({ $_.spell })
    );
}

sub apply-effects(GameState $state, Effect @effects) {
    gather for @effects -> $effect {
        my $result = effect($effect.spell);
        $state.player-mana += $result.mana-gain;
        $state.boss-hp -= $result.damage;

        if $effect.turns-left == 1 {
            $result = effect-ended($effect.spell);
            $state.player-armor += $result.armor-adjustment;
        } else {
            take $effect.ticked;
        }
    }
}

sub cast-spell(GameState $state, Spell $spell --> Effect) {
    my $result = cast($spell);
    $state.mana-spent += cost($spell);
    $state.player-hp += $result.hp-gain;
    $state.player-mana -= cost($spell) + $result.mana-gain;
    $state.player-armor += $result.armor-adjustment;
    $state.boss-hp -= $result.damage;

    return $result.new-effect;
}

sub boss-attack(GameState $state) {
    $state.player-hp -= ($state.boss-damage - $state.player-armor) max 1;
}



# Spells -----------------------------------------------------------------------

constant %cost-of = (
    MagicMissile => 53,
    Drain        => 73,
    Shield       => 113,
    Poison       => 173,
    Recharge     => 229,
);

sub cost(Spell $spell --> Int) {
    return %cost-of{$spell};
}

class SpellResult {
    has Int    $.hp-gain          = 0;
    has Int    $.mana-gain        = 0;
    has Int    $.damage           = 0;
    has Int    $.armor-adjustment = 0;
    has Effect $.new-effect       = Effect;

    my $.null = SpellResult.new;
}

multi cast(MagicMissile) {
    return SpellResult.new(damage => 4);
}
multi cast(Drain) {
    return SpellResult.new(
        damage  => 2,
        hp-gain => 2,
    );
}
multi cast(Shield) {
    return SpellResult.new(
        armor-adjustment => 7,
        new-effect => Effect.new(
            spell => Shield,
            turns-left => 6,
        ),
    );
}
multi cast(Poison) {
    return SpellResult.new(
        new-effect => Effect.new(
            spell => Poison,
            turns-left => 6,
        ),
    );
}
multi cast(Recharge) {
    return SpellResult.new(
        new-effect => Effect.new(
            spell => Recharge,
            turns-left => 5,
        ),
    );
}

multi effect(Poison) {
    return SpellResult.new(damage => 3);
}
multi effect(Recharge) {
    return SpellResult.new(mana-gain => 101);
}
multi effect(Spell) {
    return SpellResult.null;
}

multi effect-ended(Shield) {
    return SpellResult.new(armor-adjustment => -7);
}
multi effect-ended($spell) {
    return SpellResult.null;
}


# Parsing ----------------------------------------------------------------------

