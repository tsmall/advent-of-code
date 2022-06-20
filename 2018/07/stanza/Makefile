LIBS = ../../../lib/stanza/*.stanza

run: solution
	time cat ../input.txt | ./solution 5 60

example: solution
	time cat ../example.txt | ./solution 2 0

solution: $(LIBS) solution.stanza
	stanza $(LIBS) solution.stanza -o solution

clean:
	rm -f solution
