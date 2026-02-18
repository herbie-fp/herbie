UNAME_S := $(shell uname -s)
LIB_EXT := so
LIB_FLAGS := -shared -fPIC

ifeq ($(UNAME_S), Darwin)
    LIB_EXT := dylib
    LIB_FLAGS := -dynamiclib
endif

.PHONY: help install egg-herbie nightly index start-server deploy compile-accelerators time-ops evaluate-proj evaluate-basilisk

help:
	@echo "Type 'make install' to install Herbie"
	@echo "Then type 'racket -l herbie web' to run it."

install: clean egg-herbie egglog-herbie update

clean:
	raco pkg remove --force --no-docs herbie && echo "Uninstalled old herbie" || :
	raco pkg remove --force --no-docs egg-herbie && echo "Uninstalled old egg-herbie" || :
	raco pkg remove --force --no-docs egg-herbie-linux && echo "Uninstalled old egg-herbie" || :
	raco pkg remove --force --no-docs egg-herbie-windows && echo "Uninstalled old egg-herbie" || :
	raco pkg remove --force --no-docs egg-herbie-osx && echo "Uninstalled old egg-herbie" || :
	raco pkg remove --force --no-docs egg-herbie-macosm1 && echo "Uninstalled old egg-herbie" || :

update:
	raco pkg install --update-deps --no-docs --auto --name herbie src/
	raco pkg update --auto rival
	raco pkg update --name herbie --deps search-auto src/

egg-herbie:
	cargo build --release --manifest-path=egg-herbie/Cargo.toml
	raco pkg remove --force --no-docs egg-herbie && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force --no-docs egg-herbie-linux && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force --no-docs egg-herbie-windows && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force --no-docs egg-herbie-osx && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force --no-docs egg-herbie-macosm1 && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg install ./egg-herbie

egglog-herbie:
	cargo install --git "https://github.com/egraphs-good/egglog-experimental" --branch main egglog-experimental


distribution: minimal-distribution
	cp -r bench herbie-compiled/

minimal-distribution:
	mkdir -p herbie-compiled/
	cp README.md herbie-compiled/
	cp LICENSE.md herbie-compiled/
	cp logo.png herbie-compiled/
	raco exe -o herbie --orig-exe --embed-dlls --vv src/main.rkt
	[ ! -f herbie.exe ] || (raco distribute herbie-compiled herbie.exe && rm herbie.exe)
	[ ! -f herbie.app ] || (raco distribute herbie-compiled herbie.app && rm herbie.app)
	[ ! -f herbie ] || (raco distribute herbie-compiled herbie && rm herbie)

nightly:
	make compile-accelerators
	make evaluate-basilisk

time-ops:
	make compile-accelerators
	python3 growlibm/timing/time_ops.py

evaluate-proj:
	bash growlibm/evaluate.sh bench/proj/ reports proj 

evaluate-basilisk:
	bash growlibm/evaluate.sh bench/orbital-motion.fpcore reports basilisk 

compile-accelerators:
	clang $(LIB_FLAGS) -O3 -o growlibm/accelerators/libaccelerators.$(LIB_EXT) \
		growlibm/accelerators/accelerators.c \
		growlibm/accelerators/cosquot.c \
		growlibm/accelerators/e_rem_pio2.c \
		growlibm/accelerators/powcos.c \
		growlibm/accelerators/invgud.c

upgrade:
	git pull
	$(MAKE) install

start-server:
	racket -y src/main.rkt web --seed 1 --timeout 150 --threads 8 \
		--demo --public --prefix /demo/ --port 4053 --save-session www/demo/ \
		--log infra/server.log --quiet 2>&1

fmt:
	@raco fmt -i $(shell find egg-herbie/ src/ infra/ -name '*.rkt' -not -path 'src/platforms/*.rkt' -not -path "infra/softposit.rkt")

herbie.zip herbie.zip.CHECKSUM:
	raco pkg create src/
	mv src.zip herbie.zip
	mv src.zip.CHECKSUM herbie.zip.CHECKSUM

random-file:
	@find src infra -path '*/compiled/*' -prune \
		    -o -path 'infra/survey/*' -prune \
		    -o -type f -print | \
	sort -R | head -n1
