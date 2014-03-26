all: report

report: make-report.rkt
	git stash
	racket make-report.rkt
	git stash
	BRANCH=`git rev-parse --abbrev-rev HEAD`
	git checkout gh-pages
	git stash apply
	git checkout $BRANCH
	git stash apply
