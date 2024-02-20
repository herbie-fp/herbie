SEED ?= 0
TIMEOUT ?= 150
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
CMP_BRANCH ?= mix-histograms



distance:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/distance.fpcore ../infra/reports/distance-$(BRANCH)
2sin2cos: car-print
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/2sin.fpcore ../infra/reports/2sin2cos-$(BRANCH)
2sin2cos-cmp:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/2sin.fpcore ../infra/reports/2sin2cos-$(BRANCH)
	git checkout $(CMP_BRANCH)
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/2sin.fpcore ../infra/reports/2sin2cos-$(CMP_BRANCH)
	git checkout $(BRANCH)
hamming:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/hamming ../infra/reports/hamming-$(BRANCH)
hamming-cmp-main:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/hamming ../infra/reports/hamming-$(BRANCH)
	git checkout main
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/hamming ../infra/reports/hamming-main
	git checkout $(BRANCH)
harley:
	racket -y herbie.rkt report --seed $(SEED) --timeout $(TIMEOUT) ../bench/mathematics/dirichlet-mixture-model.fpcore ../infra/reports/harley-$(BRANCH)
shell:
	racket -y herbie.rkt shell --seed $(SEED)
car-print:
	@echo "        __-------__"
	@echo "      / _---------_ \\"
	@echo "     / /           \ \\"
	@echo "     | |           | |"
	@echo "     |_|___________|_|"
	@echo " /-\|                 |/-\\"
	@echo "| _ |\       0       /| _ |"
	@echo "|(_)| \      !      / |(_)|"
	@echo "|___|__\_____!_____/__|___|"
	@echo "[_________|ARTEM|_________] "
	@echo " ||||    ~~~~~~~~     ||||"
	@echo " \`--'                 \`--'"
