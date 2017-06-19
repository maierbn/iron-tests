default:
	xelatex tests
	bibtex  tests
	xelatex tests
	xelatex tests

tex:
	latexmk -pdf -pvc tests

run-tests:
	cd examples && bash run_examples.sh

clean:
	rm -rf *aux doc/*aux *.bbl *.blg *.fdb_latexmk  *.fls *.lof *.log *.lot *.out *.toc *-eps-converted-to.pdf
