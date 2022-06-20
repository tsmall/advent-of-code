LIBS = ../../../lib/stanza/*.stanza

run: solution
	time cat ../input.txt | ./solution

example: solution
	time cat ../example.txt | ./solution

solution: $(LIBS) solution.stanza
	stanza $(LIBS) solution.stanza -o solution

clean:
	rm -f solution
