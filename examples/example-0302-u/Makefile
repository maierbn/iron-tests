default:
	make clean
	mkdir -p results/current_run
	make debug; echo "debug"
	make release; echo "release";
	make compare
	make create-figures

debug:
	bash run_example.sh debug

release:
	bash run_example.sh release

compare:
	cd results; echo "All tests failed." > failed.tests; echo "Passed tests: 0 / 3" > results.summary
#	cd results; rm -f failed.tests results.summary; python compare_solutions.py

create-figures:
	cd examples; bash create_figures.sh

clean:
	rm -rf *.diag *.log *.aux *.out debug/ release/
	rm -rf results/current_run
	rm -rf results/failed.tests results/results.summary
	rm -f  failed.tests results.summary
	rm -rf doc/figures/*-eps-converted-to.pdf
