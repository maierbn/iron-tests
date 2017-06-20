# iron-tests
Collection of tests for OpenCMISS-iron

## Makefile targets
	default: compile and run tests, collect results, create figures, compile documentation using XeLaTex
	tex: compile documentation using XeLaTeX
	latexmk: compile documentation using latexmk
	run-tests-debug: compile and run examples (debug version)
	run-tests-release: compile and run examples (release version)
	collect-results: collect testing results and summary
	create-figures: generate figures for summary
	clean: remove temporary files

