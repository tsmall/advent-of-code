LIBS = ../../../lib/stanza/*.stanza

run: solution
	cat ../input.txt | ./solution

solution: $(LIBS) solution.stanza
	stanza $(LIBS) solution.stanza -o solution

clean:
	rm -f solution
