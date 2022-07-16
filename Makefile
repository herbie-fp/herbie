.PHONY: help install egg-herbie nightly index start-server deploy

help:
	@echo "Type 'make install' to install Herbie"
	@echo "Then type 'racket src/herbie.rkt web' to run it."

install: egg-herbie update

update:
	raco pkg install --skip-installed --auto --name herbie src/
	raco pkg update --name herbie src/

egg-herbie:
	cargo build --release --manifest-path=egg-herbie/Cargo.toml
	raco pkg remove --force egg-herbie && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force egg-herbie-linux && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force egg-herbie-windows && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force egg-herbie-osx && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg install ./egg-herbie

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
