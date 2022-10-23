.PHONY: run clean

run: solution
	time ./solution

solution: solution.m
	clang -fobjc-arc -ObjC -o solution solution.m

clean:
	rm -f solution
