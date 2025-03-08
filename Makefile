.PHONY: help install egglog nightly index start-server deploy

help:
	@echo "Type 'make install' to install Herbie"
	@echo "Then type 'racket src/herbie.rkt web' to run it."

install: clean egglog update

clean:
	raco pkg remove --force herbie && echo "Uninstalled old herbie" || :

update:
	raco pkg install --skip-installed --auto --name herbie src/
	raco pkg update --name herbie --deps search-auto src/

egglog:
	cargo build --release --manifest-path=src/egglog/egg-smol/Cargo.toml

distribution: minimal-distribution
	cp -r bench herbie-compiled/

minimal-distribution:
	mkdir -p herbie-compiled/
	cp README.md herbie-compiled/
	cp LICENSE.md herbie-compiled/
	cp logo.png herbie-compiled/
	raco exe -o herbie --orig-exe --embed-dlls --vv src/herbie.rkt
	[ ! -f herbie.exe ] || (raco distribute herbie-compiled herbie.exe && rm herbie.exe)
	[ ! -f herbie.app ] || (raco distribute herbie-compiled herbie.app && rm herbie.app)
	[ ! -f herbie ] || (raco distribute herbie-compiled herbie && rm herbie)

nightly: install
	bash infra/nightly.sh reports

start-server: install
	racket src/herbie.rkt web --seed 1 --timeout 150 --num-iters 2 \
		--demo --public --prefix /demo/ --port 4053 --save-session www/demo/ \
		--log infra/server.log --quiet 2>&1

# This rule is run by herbie.uwplse.org on every commit to Github.
# It does not restart the demo server, but it does pull new static content
deploy:
	git -C $(shell ~/uwplse/getdir) pull

herbie.zip herbie.zip.CHECKSUM:
	raco pkg create src/
	mv src.zip herbie.zip
	mv src.zip.CHECKSUM herbie.zip.CHECKSUM
