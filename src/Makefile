SEED ?= 0

distance:
	racket -y herbie.rkt report --seed $(SEED) ../bench distance.fpcore ../infra/reports/distance
2sin2cos:
	racket -y herbie.rkt report --seed $(SEED) ../bench/2sin.fpcore ../infra/2sin2cos
hamming:
	racket -y herbie.rkt report --seed $(SEED) ../bench/hamming ../infra/reports/hamming
shell:
	racket -y herbie.rkt shell --seed $(SEED)
