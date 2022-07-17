package main

import (
	"strings"
	"testing"
)

type parseTestCase struct {
	input    string
	expected []int
}

var testInputs = []parseTestCase{
	parseTestCase{"1,0,0,0,99\n", []int{1, 0, 0, 0, 99}},
	parseTestCase{"2,3,0,3,99\n", []int{2, 3, 0, 3, 99}},
	parseTestCase{"2,4,4,5,99,0\n", []int{2, 4, 4, 5, 99, 0}},
	parseTestCase{"1,1,1,4,99,5,6,0,99\n", []int{1, 1, 1, 4, 99, 5, 6, 0, 99}},
}

func TestParsing(t *testing.T) {
	for i, test := range testInputs {
		r := strings.NewReader(test.input)
		actual := ParseInput(r)
		if len(actual) != len(test.expected) {
			t.Fatalf("Input %d failed: expected %v but got %v",
				i+1, test.expected, actual)
		}
		for j, n := range test.expected {
			if n != actual[j] {
				t.Fatalf("Input %d failed: expected %v but got %v",
					i+1, test.expected, actual)
			}
		}
	}
}
