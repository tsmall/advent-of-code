# Spells -----------------------------------------------------------------------

class Effect    { ... }
class GameState { ... }

class SpellResult {
    has Int    $.hp-gain          = 0;
    has Int    $.mana-gain        = 0;
    has Int    $.damage           = 0;
    has Int    $.armor-adjustment = 0;
    has Effect $.new-effect       = Effect;

    my $.null = SpellResult.new;

    method apply(GameState $state) {
        $state.player-hp    += $.hp-gain;
        $state.player-mana  += $.mana-gain;
        $state.player-armor += $.armor-adjustment;
        $state.boss-hp      -= $.damage;
    }
}

class Spell {
    has Str         $.name;
    has Int         $.cost;
    has SpellResult $.cast-result;
    has SpellResult $.effect-result = SpellResult.null;

    method cast(GameState $state --> Effect) {
        $.cast-result.apply($state);

        $state.mana-spent  += $.cost;
        $state.player-mana -= $.cost;

        return $.cast-result.new-effect;
    }
}

class Effect {
    has Str         $.name;
    has Int         $.turns-left;
    has SpellResult $.active-result;
    has SpellResult $.end-result;

    method apply(GameState $state --> Effect) {
        fail "Effect already ended" if $.turns-left <= 0;

        $.active-result.apply($state);
        $.end-result.apply($state) if $.turns-left == 1;
        return self!ticked;
    }

    method !ticked {
        return self.clone(turns-left => $.turns-left - 1);
    }
}

my @spells = (
    Spell.new(
        name => 'Magic Missile',
        cost => 53,
        cast-result => SpellResult.new(
            damage => 4
        ),
    ),
    Spell.new(
        name => 'Drain',
        cost => 73,
        cast-result => SpellResult.new(
            damage => 2,
            hp-gain => 2
        ),
    ),
    Spell.new(
        name => 'Shield',
        cost => 113,
        cast-result => SpellResult.new(
            armor-adjustment => 7,
            new-effect => Effect.new(
                name => 'Shield',
                turns-left => 6,
                active-result => SpellResult.null,
                end-result => SpellResult.new(:armor-adjustment(-7)),
            ),
        ),
    ),
    Spell.new(
        name => 'Poison',
        cost => 173,
        cast-result => SpellResult.new(
            new-effect => Effect.new(
                name => 'Poison',
                turns-left => 6,
                active-result => SpellResult.new(:damage(3)),
                end-result => SpellResult.null,
            ),
        )
    ),
    Spell.new(
        name => 'Recharge',
        cost => 229,
        cast-result => SpellResult.new(
            new-effect => Effect.new(
                name => 'Recharge',
                turns-left => 5,
                active-result => SpellResult.new(:mana-gain(101)),
                end-result => SpellResult.null,
            ),
        )
    ),
);


# Game Logic -------------------------------------------------------------------

role Observer { ... }

class GameState {
    has Int      $.player-hp      is rw;
    has Int      $.player-mana    is rw;
    has Int      $.player-armor   is rw = 0;
    has Int      $.mana-spent     is rw = 0;
    has Int      $.boss-hp        is rw;
    has Int      $.boss-damage;
    has Bool     $.hard-mode;
    has Effect   @.active-effects is rw = ();
    has Observer @.observers            = ();

    method clone { nextwith :active-effects(@!active-effects.clone), |%_ }

    method play-all-possibilities() {
        for @spells -> $spell {
            next if not self!can-cast($spell);

            given self.clone -> $next {
                $next!play-turn($spell);
                $next.play-all-possibilities() if $next!should-continue;
            }
        }
    }

    method !can-cast(Spell $spell --> Bool) {
        return (
            $spell.cost <= $.player-mana
            and
            $spell.name ∉ $.active-effects.map({ $_.name })
        );
    }
    
    method !should-continue(--> Bool) {
        return so (
                $.player-hp > 0
            and $.boss-hp > 0
            and $.observers».should-continue(self).all
        );
    }

    method !play-turn(Spell $spell) {
        # Player turn
        if $.hard-mode {
            $!player-hp -= 1;
            return if $.player-hp <= 0;
        }
        self!apply-effects();
        with $spell.cast(self) -> $new-effect {
            push @!active-effects, $new-effect;
        }

        # Boss turn
        self!apply-effects();
        if $.boss-hp > 0 {
            $!player-hp -= ($.boss-damage - $.player-armor) max 1
        }

        # See if the game is over
        if $.boss-hp <= 0 and $.player-hp > 0 {
            $.observers».player-won(self);
        }
    }

    method !apply-effects() {
        @!active-effects = gather for @!active-effects -> $effect {
            given $effect.apply(self) -> $new-effect {
                take $new-effect if $new-effect.turns-left > 0;
            }
        };
    }
}

role Observer {
    method player-won(GameState $state) { ... }
    method should-continue(GameState $state --> Bool) { ... }
}

class LeastManaSpentObserver does Observer {
    has Int $.least-mana-spent = 9_999_999;

    method player-won(GameState $state) {
        $!least-mana-spent = $.least-mana-spent min $state.mana-spent;
    }

    method should-continue(GameState $state --> Bool) {
        return $state.mana-spent < $.least-mana-spent;
    }
}


# Main -------------------------------------------------------------------------

enum Part <Part1 Part2>;

sub MAIN(Part $part) {
    my $observer = LeastManaSpentObserver.new;
    my $game = GameState.new(
        player-hp   => 50,
        player-mana => 500,
        boss-hp     => 51,
        boss-damage => 9,
        hard-mode   => $part eqv Part2,
        observers   => $observer,
    );

    $game.play-all-possibilities();

    say "--- {$part} ---";
    say $observer.least-mana-spent;
}
