SEED ?= 0
TIMEOUT ?= 150

distance:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/distance.fpcore ../infra/reports/distance
2sin2cos:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/2sin.fpcore ../infra/2sin2cos
hamming:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/hamming ../infra/reports/hamming
harley:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/mathematics/dirichlet-mixture-model.fpcore ../infra/reports/harley
shell:
	racket -y herbie.rkt shell --seed $(SEED)
