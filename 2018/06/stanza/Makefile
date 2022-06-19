LIBS = ../../../lib/stanza/*.stanza

run: solution
	time cat ../input.txt | ./solution 10000

example: solution
	time cat ../example.txt | ./solution 32

solution: $(LIBS) solution.stanza
	stanza $(LIBS) solution.stanza -o solution

clean:
	rm -f solution
