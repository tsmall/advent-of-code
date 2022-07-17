package cpu

import "testing"

type programTestCase struct {
	program, expected []int
}

var testPrograms = []programTestCase{
	programTestCase{[]int{1, 0, 0, 0, 99}, []int{2, 0, 0, 0, 99}},
	programTestCase{[]int{2, 3, 0, 3, 99}, []int{2, 3, 0, 6, 99}},
	programTestCase{[]int{2, 4, 4, 5, 99, 0}, []int{2, 4, 4, 5, 99, 9801}},
	programTestCase{[]int{1, 1, 1, 4, 99, 5, 6, 0, 99}, []int{30, 1, 1, 4, 2, 5, 6, 0, 99}},
}

func TestExamplePrograms(t *testing.T) {
	for i, test := range testPrograms {
		cpu := NewCPU(test.program)
		cpu.Run()
		for j, n := range test.expected {
			if n != cpu.Memory[j] {
				t.Fatalf("Program %d failed: expected %v but got %v",
					i+1, test.expected, cpu.Memory)
			}
		}
	}
}
