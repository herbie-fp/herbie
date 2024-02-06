SEED ?= 0
TIMEOUT ?= 150
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)

distance :
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/distance.fpcore ../infra/reports/distance-$(BRANCH)
2sin2cos:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/2sin.fpcore ../infra/reports/2sin2cos-$(BRANCH)
hamming:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/hamming ../infra/reports/hamming-$(BRANCH)
harley:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/mathematics/dirichlet-mixture-model.fpcore ../infra/reports/harley-$(BRANCH)
shell:
	racket -y herbie.rkt shell --seed $(SEED)
