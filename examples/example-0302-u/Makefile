default:
	make clean
	mkdir -p results/current_run
	make release-fast
	make compare
	make create-figures

debug-fast:
	bash run_example.sh debug fast

debug-big:
	bash run_example.sh debug big

release-fast:
	bash run_example.sh release fast

release-big:
	bash run_example.sh release big

compare:
	cd results; echo "All tests failed." > failed.tests; echo "Passed tests: 0 / 12" > results.summary
#	cd results; rm -f failed.tests results.summary; python compare_solutions.py

create-figures:
	cd examples; bash create_figures.sh

clean:
	rm -rf *.diag *.log *.aux *.out debug/ release/
	rm -rf results/current_run
	rm -rf results/failed.tests results/results.summary
	rm -f  failed.tests results.summary
	rm -rf doc/figures/*-eps-converted-to.pdf
