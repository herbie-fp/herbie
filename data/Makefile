%:
	python2 graph.py $@ > $@.tex
	pdflatex $@.tex

clean:
	rm -f *.pdf *.tex *.aux *.log *.out *~

help:
	python2 graph.py

update:
	$(MAKE) time && cp time.pdf ../asplos15/fig/eval-casio-time.pdf
	$(MAKE) bits && cp bits.pdf ../asplos15/fig/eval-mpfr-bits.pdf
	$(MAKE) rect-f && cp rect-f.pdf ../asplos15/fig/eval-rect-f.pdf
	$(MAKE) rect-d && cp rect-d.pdf ../asplos15/fig/eval-rect-d.pdf
	$(MAKE) err && cp err.pdf ../asplos15/fig/eval-err.pdf
