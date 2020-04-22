.PHONY: all install update nightly index publish start-server package deploy

all:
	@echo "Type 'make install' to install Herbie"
	@echo "Then type 'racket src/herbie.rkt web' to run Herbie."

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

start-server:
	$(MAKE) update || $(MAKE) install
	racket src/herbie.rkt web --seed 1 --timeout 150 --num-iters 2 \
		--demo --public --prefix /demo/ --port 4053 --save-session www/demo/ \
		--log infra/server.log --quiet 2>&1

# This rule is run by herbie.uwplse.org on every commit to Github.
# It does not restart the demo server, but it does pull new static content
deploy:
	cd $(shell ~/uwplse/getdir) && git pull

herbie.zip herbie.zip.CHECKSUM:
	raco pkg create src/
	mv src.zip herbie.zip
	mv src.zip.CHECKSUM herbie.zip.CHECKSUM
