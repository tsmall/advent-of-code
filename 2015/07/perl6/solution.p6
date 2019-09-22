use v6;

# ------------------------------------------------------------------------------
# Gates

role Gate {
    has $.circuit is rw;
    method value (--> Int) { ... }
}

role SingleWireGate does Gate {
    has Str $.wire is required;

    method wire-value {
        return self.circuit.wire-value(self.wire);
    }
}

role DoubleWireGate does Gate {
    has Str $.wire1 is required;
    has Str $.wire2 is required;

    method wire1-value {
        return self.circuit.wire-value(self.wire1);
    }

    method wire2-value {
        return self.circuit.wire-value(self.wire2);
    }
}

class ConstantGate does Gate {
    has Int $.value is required;
}

class AndGate does DoubleWireGate {
    method value (--> Int) {
        return self.wire1-value +& self.wire2-value;
    }
}

class LiteralAndGate does SingleWireGate {
    has Int $.number is required;

    method value (--> Int) {
        return self.number +& self.wire-value;
    }
}

class OrGate does DoubleWireGate {
    method value (--> Int) {
        return self.wire1-value +| self.wire2-value;
    }
}

class LeftShiftGate does SingleWireGate {
    has Int $.amount is required;

    method value (--> Int) {
        return self.wire-value +< self.amount;
    }
}

class RightShiftGate does SingleWireGate {
    has Int $.amount is required;

    method value (--> Int) {
        return self.wire-value +> self.amount;
    }
}

class NotGate does SingleWireGate {
    method value (--> Int) {
        my $i = +^ self.wire-value;
        my $n = 65_536;
        return (($i % $n) + $n) % $n;
    }
}

class Passthrough does SingleWireGate {
    method value (--> Int) {
        return self.wire-value;
    }
}


# ------------------------------------------------------------------------------
# Wire

class Wire {
    has Str $.name is required;
    has Gate $.input is required;
    has Int $!cached-value;

    method value (--> Int) {
        unless $!cached-value {
            $!cached-value = self.input.value;
        }
        return $!cached-value;
    }
}


# ------------------------------------------------------------------------------
# Circuit

class Circuit {
    has %!wires;

    method add-wire (Str:D $name, Gate:D $gate) {
        $gate.circuit = self;
        %!wires{$name} = Wire.new(
            name => $name,
            input => $gate
        );
    }

    method wire-value (Str $name) {
        die "Missing wire: $name" unless %!wires{$name}:exists;
        my $wire = %!wires{$name};
        return $wire.value;
    }
}


# ------------------------------------------------------------------------------
# Input Parsing

class Instruction {
    has Str $.output-wire is required;
    has Gate $.gate is required;

    method apply-to ($circuit) {
        $circuit.add-wire(self.output-wire, self.gate);
    }
}

grammar InstructionParser {
    token TOP      { <input> ' -> ' <output> }

    token input    { <value> | <and> | <or> | <shift> | <negate> | <pass> }
    token value    { <number> }
    token and      { <lit-and> | <wire-and> }
    token lit-and  { <number> ' AND ' <wire> }
    token wire-and { <wire1=.wire> ' AND ' <wire2=.wire> }
    token or       { <wire1=.wire> ' OR ' <wire2=.wire> }
    token shift    { <lshift> | <rshift> }
    token lshift   { <wire> ' LSHIFT ' <amount=.number> }
    token rshift   { <wire> ' RSHIFT ' <amount=.number> }
    token negate   { 'NOT ' <wire> }
    token pass     { <wire> }

    token output   { <wire> }

    token wire     { \w+ }
    token number   { \d+ }
}

class InstructionParserActions {
    method TOP ($/) {
        make Instruction.new(
            output-wire => $<output>.made,
            gate => $<input>.made
        );
    }

    method input ($/) {
        make (
               $<value>.made
            || $<and>.made
            || $<or>.made
            || $<shift>.made
            || $<negate>.made
            || $<pass>.made
        );
    }

    method value ($/) {
        make ConstantGate.new(value => $<number>.made);
    }

    method and ($/) {
        make $<lit-and>.made || $<wire-and>.made;
    }

    method lit-and ($/) {
        make LiteralAndGate.new(
            number => $<number>.made,
            wire => $<wire>.made
        );
    }

    method wire-and ($/) {
        make AndGate.new(
            wire1 => $<wire1>.made,
            wire2 => $<wire2>.made
        );
    }

    method or ($/) {
        make OrGate.new(
            wire1 => $<wire1>.made,
            wire2 => $<wire2>.made
        );
    }

    method shift ($/) {
        make $<lshift>.made || $<rshift>.made;
    }

    method lshift ($/) {
        make LeftShiftGate.new(
            wire => $<wire>.made,
            amount => $<amount>.made
        );
    }

    method rshift ($/) {
        make RightShiftGate.new(
            wire => $<wire>.made,
            amount => $<amount>.made
        );
    }

    method negate ($/) {
        make NotGate.new(
            wire => $<wire>.made
        );
    }

    method pass ($/) {
        make Passthrough.new(
            wire => $<wire>.made
        );
    }

    method output ($/) {
        make $<wire>.made;
    }

    method wire   ($/) { make ~$/; }
    method number ($/) { make +$/; }
}

sub parse(Str $line) {
    state $actions = InstructionParserActions.new;
    my $result = InstructionParser.parse($line, actions => $actions);
    return $result.made;
}


# ------------------------------------------------------------------------------
# I/O

my $circuit = Circuit.new;

for $*IN.lines -> $line {
    my $instruction = $line.&parse;
    $instruction.apply-to($circuit);
}

say '--- Part One ---';
say 'a: ', $circuit.wire-value('a');
