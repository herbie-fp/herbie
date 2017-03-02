.PHONY: report publish www compile clean loc deploy

all:
	@echo "Type 'make install' to install Herbie as a Racket package,"
	@echo "just run 'racket src/herbie.rkt' to run Herbie."

install:
	raco pkg install --auto $TRAVIS_BUILD_DIR/src

clean:
	rm -f cost
	rm -rf graphs/

publish:
	bash infra/publish.sh upload graphs/
	bash infra/publish.sh index

start-server:
	racket src/herbie.rkt web --seed '#(2775764126 3555076145 3898259844 1891440260 2599947619 1948460636)' --timeout 1 --demo --prefix /demo/ --port 4053 --save-session www/demo/ --log infra/server.log --quiet 2>&1

loc:
	find herbie/ -type f -exec cat {} \; | wc -l

# This rule is run by herbie.uwplse.org on every commit to Github.
# It does not restart the demo server, but it does pull new static content
deploy:
	cd $(shell ~/uwplse/getdir) && git pull

cost: infra/cost.c
	$(CC) -O0 $^ -lm -o $@

