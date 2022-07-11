package main

import (
	"strings"
	"testing"
)

type TestCase struct {
	input    int
	expected int
}

var fuelTests = []TestCase{
	{12, 2},
	{14, 2},
	{1969, 654},
	{100756, 33583},
}

func TestFuelCalculation(t *testing.T) {
	for _, test := range fuelTests {
		actual := FuelRequired(test.input)
		if actual != test.expected {
			t.Fatalf("Expected %d but got %d", test.expected, actual)
		}
	}
}

func TestInputParsing(t *testing.T) {
	input := "12\n14\n1969\n100756\n"
	reader := strings.NewReader(input)
	actual := ParseInput(reader)
	if n := len(actual); n != 4 {
		t.Fatalf("Expected 4 values but got %d", n)
	}

	expected := []int{12, 14, 1969, 100756}
	for i := range expected {
		if actual[i] != expected[i] {
			t.Fatalf(
				"Expected value at index %d to be %d but got %d",
				i, expected[i], actual[i])
		}
	}
}

var fuelForFuelTests = []TestCase{
	{2, 0},
	{654, 312},
	{33583, 16763},
}

func TestFuelForFuel(t *testing.T) {
	for _, test := range fuelForFuelTests {
		actual := FuelForFuel(test.input)
		if actual != test.expected {
			t.Fatalf("Expected %d but got %d", test.expected, actual)
		}
	}
}
