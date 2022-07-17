package main

import (
	"day2/cpu"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

func main() {
	program := ParseInput(os.Stdin)
	cpu := cpu.NewCPU(program)

	cpu.SetInput(12, 2)
	cpu.Run()
	fmt.Printf("Part 1: %d\n", cpu.Output())

	answer := 19_690_720
OuterLoop:
	for noun := 0; noun <= 99; noun += 1 {
		for verb := 0; verb <= 99; verb += 1 {
			cpu.Reset()
			cpu.SetInput(noun, verb)
			cpu.Run()
			if cpu.Output() == answer {
				fmt.Printf("Part 2: %d\n", 100*noun+verb)
				break OuterLoop
			}
		}
	}
}

func ParseInput(input io.Reader) []int {
	bytes, err := io.ReadAll(input)
	if err != nil {
		panic(fmt.Sprintf("unable to read input: %s", err.Error()))
	}
	words := strings.Split(string(bytes[:]), ",")
	nums := make([]int, len(words))
	for i, w := range words {
		n, err := strconv.Atoi(strings.TrimSpace(w))
		if err != nil {
			panic(fmt.Sprintf("encountered invalid char: %v", w))
		}
		nums[i] = n
	}
	return nums
}
