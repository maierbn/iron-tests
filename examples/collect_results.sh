cat `ls example-*/results/failed.tests` | tee ../failed.tests
cat `ls example-*/results/results.summary` | awk '{s+=$3;t+=$5}END{print "Passed tests: " s " / " t}' | tee ../results.summary
