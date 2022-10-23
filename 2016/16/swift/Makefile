.PHONY: run clean

run: solution
	time ./solution

solution: solution.swift
	swiftc -O solution.swift

clean:
	rm -f solution
