
.PHONY: report publish link clean 

report: 
	racket reports/make-report.rkt bench/

publish:
	bash reports/publish.sh

link:
	raco link casio
	raco link reports

cost:
	$(CC) -O0 cost.c -lm -o cost


clean:
	rm -f index.html
	rm -f cost
	rm -rf graphs/
