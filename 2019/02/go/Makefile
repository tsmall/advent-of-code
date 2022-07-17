run: solution.go
	time cat ../input.txt | go run solution.go

test: cpu/*.go solution.go solution_test.go
	go test day2/cpu
	go test day2
