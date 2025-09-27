.PHONY: nightly hook

nightly:
	bash infra/nightly.sh perf

fmt:
	@raco fmt -i $(shell find infra/ ops/ eval/ ./ -name '*.rkt')

distribution:
	mkdir -p rival-compiled/
	cp README.md rival-compiled/
	cp LICENSE rival-compiled/
	raco make main.rkt
	raco exe -o rival --orig-exe --embed-dlls --vv main.rkt
	[ ! -f rival.exe ] || (raco distribute rival-compiled rival.exe && rm rival.exe)
	[ ! -f rival.app ] || (raco distribute rival-compiled rival.app && rm rival.app)
	[ ! -f rival ] || (raco distribute rival-compiled rival && rm rival)
