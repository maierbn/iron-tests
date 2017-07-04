default:
	make clean
	mkdir -p results/current_run
	make debug
	make release
	make compare
	make visualize

debug:
	bash run_example.sh debug

release:
	bash run_example.sh release

compare:
	cd results; python compare_solutions.py

create-figures:
	cd results; bash create_animations.sh

clean:
	rm -rf *.diag *.log *.aux *.out debug/ release/
	rm -rf results/current_run
	rm -rf results/failed.tests results/results.summary
	rm -f  failed.tests results.summary
	rm -rf doc/figures/*-eps-converted-to.pdf
	rm -rf Time*
	rm -rf tmp*
