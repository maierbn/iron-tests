default:
	make debug
	make release
	make compare
	make visualize

debug:
	bash run_example.sh debug

release:
	bash run_example.sh release

compare:
	bash results/compare_iron_abaqus.sh

visualize:
	cmgui results/cmgui.com

clean:
	rm -rf *.diag *.log *.aux *.out debug/ release/ results/current_run/*.exnode results/current_run/*.exelem doc/figures/*-eps-converted-to.pdf
