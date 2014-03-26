all: report

report: make-report.rkt
	git stash
	racket make-report.rkt
	git stash
	BRANCH=`git rev-parse --abbrev-ref HEAD`
	git checkout gh-pages
	git stash apply
	git stage report.html
	git commit -m "Added New Report"
	git checkout $BRANCH
	git stash apply
