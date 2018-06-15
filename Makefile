.PHONY: all install update nightly index clean publish start-server package loc deploy

all:
	@echo "Type 'make install' to install Herbie as a Racket package,"
	@echo "Run 'racket src/herbie.rkt shell' to run Herbie."

install:
	raco pkg install --name herbie src/

update:
	raco pkg update --name herbie src/

nightly:
	bash infra/nightly.sh
	bash infra/nightly.sh --enable rules:numerics
	$(MAKE) index

index:
	bash infra/publish.sh index

herbie.zip herbie.zip.CHECKSUM:
	raco pkg create src/
	mv src.zip herbie.zip
	mv src.zip.CHECKSUM herbie.zip.CHECKSUM

clean:
	rm -f cost
	rm -rf graphs/

publish:
	bash infra/publish.sh upload graphs/
	bash infra/publish.sh index

start-server:
	racket src/herbie.rkt web --seed '#(2775764126 3555076145 3898259844 1891440260 2599947619 1948460636)' --timeout 60 --num-iters 2 --demo --prefix /demo/ --port 4053 --save-session www/demo/ --log infra/server.log --quiet 2>&1

package:
	raco pkg

loc:
	find herbie/ -type f -exec cat {} \; | wc -l

# This rule is run by herbie.uwplse.org on every commit to Github.
# It does not restart the demo server, but it does pull new static content
deploy:
	cd $(shell ~/uwplse/getdir) && git pull

cost: infra/cost.c
	$(CC) -O0 $^ -lm -o $@

