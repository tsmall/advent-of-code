build: solution

solution: solution.stanza
	stanza solution.stanza -o solution

clean:
	rm -f solution

test: solution
	../runtests ./solution

part1: solution
	cat ../input.txt | ./solution -v sum

part2: solution
	cat ../input.txt | ./solution -v repeat
