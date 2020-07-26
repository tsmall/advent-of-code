<?php
declare(strict_types=1);



class Elevator {
    private $floor;

    public function __construct() {
        $this->floor = 0;
    }

    public function moveUp() {
        $this->floor += 1;
    }

    public function moveDown() {
        $this->floor -= 1;
    }

    public function currentFloor(): int {
        return $this->floor;
    }

    public function isInBasement(): bool {
        return $this->floor < 0;
    }
}



interface Instruction {
    function run(Elevator $elevator);
}

$moveUpInstruction = new class implements Instruction {
    public function run(Elevator $elevator) {
        $elevator->moveUp();
    }
};

$moveDownInstruction = new class implements Instruction {
    public function run(Elevator $elevator) {
        $elevator->moveDown();
    }
};

$nullInstruction = new class implements Instruction {
    public function run(Elevator $elevator) {
        // Intentionally do nothing
    }
};



class Instructions {
    private $string;
    private $index;

    public function __construct(string $s) {
        $this->string = $s;
        $this->index = 0;
    }

    public function next(): ?Instruction {
        if ($this->index >= strlen($this->string)) {
            return null;
        }
        $char = $this->string[ $this->index++ ];
        return $this->parse($char);
    }

    private function parse(string $char): Instruction {
        switch ($char) {
        case '(':
            global $moveUpInstruction;
            return $moveUpInstruction;
        case ')':
            global $moveDownInstruction;
            return $moveDownInstruction;
        default:
            global $nullInstruction;
            return $nullInstruction;
        }
    }
}



class Observer {
    private $index = 1;
    private $hasVisitedBasement = false;

    public function observe(Elevator $elevator) {
        if ($this->hasVisitedBasement) {
            return;
        }
        if ($elevator->isInBasement()) {
            $this->hasVisitedBasement = true;
            return;
        }
        $this->index++;
    }

    public function firstBasementIndex(): int {
        return $this->index;
    }
}



function main() {
    $line = fgets(STDIN);
    $instructions = new Instructions($line);
    $elevator = new Elevator();
    $observer = new Observer();

    while (($instruction = $instructions->next()) != null) {
        $instruction->run($elevator);
        $observer->observe($elevator);
    }

    printf("Part 1: %d\n", $elevator->currentFloor());
    printf("Part 2: %d\n", $observer->firstBasementIndex());
}

main();
