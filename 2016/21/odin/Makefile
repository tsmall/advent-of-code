run: solution
	time cat ../input.txt | ./solution abcdefgh fbgdceah

example: solution
	time cat ../example.txt | ./solution abcde decab

solution: solution.odin
	odin build solution.odin -file -out:solution

clean:
	rm -f solution solution.bin
