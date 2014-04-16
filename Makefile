all: report

report: 
	racket reports/make-report.rkt "bench/"
	./file-report.sh
clean:
	rm index.md
	rm report.md
	find reports/* -type d | rm -r
