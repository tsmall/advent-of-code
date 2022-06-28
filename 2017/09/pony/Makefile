src := $(wildcard *.pony)

run: solution
	time cat ../input.txt | ./solution

example: solution
	time cat ../example.txt | ./solution

solution: $(src)
	ponyc --bin-name=solution .

clean:
	rm -f solution
