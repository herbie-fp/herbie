all: report

report: casio/make-report.rkt
	racket casio/make-report.rkt "bench/"
	cd "reports/"
	REPORTS=$(find * -maxdepth 0)
	cd ".."
	racket casio/make-index.rkt REPORTS
	./file-report.sh
