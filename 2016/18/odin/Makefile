run: solution ../input.txt
	time cat ../input.txt | ./solution 40 400000

example: solution ../example.txt
	time cat ../example.txt | ./solution 10 10

solution: solution.odin
	odin build solution.odin -file -out:solution

clean:
	rm -f solution
