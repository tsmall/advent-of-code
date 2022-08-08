run: solution
	time cat ../input.txt | ./solution 4294967295

example: solution
	time cat ../example.txt | ./solution 9

solution: solution.odin
	odin build solution.odin -file -out:solution

tests: solution.odin
	odin test solution.odin -file

clean:
	rm -f solution solution.bin
