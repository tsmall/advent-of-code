class CPU { ... }

subset Natural of Int where * >= 0;

enum ICode <hlf tpl inc jmp jie jio>;
enum RCode <a b>;

class Instruction {
    has ICode $.code;
    has RCode $.register;

    method run(CPU $cpu) {
        $cpu.execute($.code, $.register);
    }
}

class JumpInstruction is Instruction {
    has Int $.offset;

    method run(CPU $cpu) {
        $cpu.execute($.code, $.register, $.offset);
    }
}

class CPU {
    has Int         @!registers[2];
    has Instruction @.program = ();
    has Natural     $.counter = 0;

    method BUILD {
        @!registers = (0, 0);
    }

    method load(Instruction @program) {
        @!program = @program;
        $!counter = 0;
    }

    method run() {
        while $.counter < @.program.elems {
            @.program[$.counter].run(self);
        }
    }

    multi method execute(hlf, RCode $r) {
        self!register($r) div= 2;
        $!counter += 1;
    }

    multi method execute(tpl, RCode $r) {
        self!register($r) *= 3;
        $!counter += 1;
    }

    multi method execute(inc, RCode $r) {
        self!register($r) += 1;
        $!counter += 1;
    }

    multi method execute(jmp, RCode $r, Int $offset) {
        $!counter += $offset;
    }

    multi method execute(jie, RCode $r, Int $offset) {
        if self!register($r) %% 2 {
            $!counter += $offset;
        } else {
            $!counter += 1;
        }
    }

    multi method execute(jio, RCode $r, Int $offset) {
        if self!register($r) == 1 {
            $!counter += $offset;
        } else {
            $!counter += 1;
        }
    }

    method !register(RCode $r --> Int) is rw {
        my $i = $r eqv a ?? 0 !! 1;
        return-rw @!registers[$i];
    }

    method value(RCode $r --> Int) {
        return self!register($r);
    }
}

enum Part <Part1 Part2>;

sub MAIN(Part $part) {
    my Instruction @program = (parse($_) for $*IN.lines);

    my $cpu = CPU.new;
    $cpu.execute(inc, a) if $part eqv Part2;
    $cpu.load(@program);
    $cpu.run();

    say "--- {$part} ---";
    say 'a = ', $cpu.value(a);
    say 'b = ', $cpu.value(b);
}

sub parse(Str $instruction) {
    my ($code, $register, $offset) = $instruction.comb: / ('+'|'-')? \w+ /;

    given $code {
        when 'jmp' {
            return JumpInstruction.new(
                code   => jmp,
                offset => $register.Int,
            );
        }
        when /^j/ {
            return JumpInstruction.new(
                code     => ICode::{$code},
                register => RCode::{$register},
                offset   => $offset.Int,
            );
        }
        default {
            return Instruction.new(
                code     => ICode::{$code},
                register => RCode::{$register},
            );
        }
    }
}
