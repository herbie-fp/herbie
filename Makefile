all: report

report: casio/make-report.rkt
	racket casio/make-report.rkt "bench/"
	racket casio/make-index.rkt $(find * -maxdepth 0)
	./file-report.sh
