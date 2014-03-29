all: report

report: casio/make-report.rkt
	racket casio/make-report.rkt "bench/"
	./file-report.sh
