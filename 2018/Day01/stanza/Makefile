LIBS = ../../../lib/stanza/*.stanza

build: solution

solution: $(LIBS) solution.stanza
	stanza $(LIBS) solution.stanza -o solution

clean:
	rm -f solution

test: solution
	../runtests ./solution

part1: solution
	cat ../input.txt | ./solution -v sum

part2: solution
	cat ../input.txt | ./solution -v repeat
