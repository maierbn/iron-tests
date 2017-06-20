default:
	xelatex tests
	bibtex  tests
	xelatex tests
	xelatex tests

tex:
	latexmk -pdf -pvc tests

run-tests-debug:
	cd examples && bash run_examples.sh debug

run-tests-release:
	cd examples && bash run_examples.sh release

collect-results:
	cd examples; bash collect_results.sh

clean:
	rm -rf *aux doc/*aux *.bbl *.blg *.fdb_latexmk  *.fls *.lof *.log *.lot *.out *.toc
	rm -rf *-eps-converted-to.pdf
	rm -rf examples/example-*/doc/*.aux
	rm -rf examples/example-*/*.out
	rm -rf examples/example-*/*.diag
	rm -rf examples/example-*/debug
	rm -rf examples/example-*/release
	rm -rf examples/example-*/results/current_run
	rm -rf examples/example-*/results/failed.tests
	rm -rf examples/example-*/results/results.summary
	rm -rf failed.tests results.summary
