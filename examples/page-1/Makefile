SHELL := bash
test: test.htg
	htg < test.htg > test.html
	cat test.html | ./tidy | grep -v '^$$' > test-pretty.html
