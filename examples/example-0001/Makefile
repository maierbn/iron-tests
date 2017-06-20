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
	python results/compare_solutions.py

visualize:
	cmgui results/cmgui.com

clean:
	rm -rf *.diag *.log *.aux *.out debug/ release/
	rm -rf results/current_run
	rm -rf results/failed.tests results/results.summary
	rm -f  failed.tests results.summary
	rm -rf doc/figures/*-eps-converted-to.pdf
