package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
)

func main() {
	masses := ParseInput(os.Stdin)
	fuel := TotalFuelRequired(masses, false)
	fmt.Printf("Part 1: %d\n", fuel)

	fuel = TotalFuelRequired(masses, true)
	fmt.Printf("Part 2: %d\n", fuel)
}

func ParseInput(r io.Reader) []int {
	s := bufio.NewScanner(r)
	ns := []int{}
	for s.Scan() {
		n, err := strconv.Atoi(s.Text())
		if err != nil {
			panic(fmt.Sprintf("Invalid line: %s", s.Text()))
		}
		ns = append(ns, n)
	}
	if err := s.Err(); err != nil {
		panic("unable to read input")
	}
	return ns
}

func TotalFuelRequired(masses []int, includeFuel bool) int {
	total := 0
	for _, mass := range masses {
		fuel := FuelRequired(mass)
		if includeFuel {
			fuel += FuelForFuel(fuel)
		}
		total += fuel
	}
	return total
}

func FuelForFuel(fuel int) int {
	newFuel := FuelRequired(fuel)
	if newFuel > 0 {
		return newFuel + FuelForFuel(newFuel)
	} else {
		return 0
	}
}

func FuelRequired(mass int) int {
	fuel := math.Floor(float64(mass)/3.0) - 2
	return int(fuel)
}
