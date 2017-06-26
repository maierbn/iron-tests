# CMake generated Testfile for 
# Source directory: /home/emamynt/opencmiss/iron-tests/examples/example-0401/src
# Build directory: /home/emamynt/opencmiss/iron-tests/examples/example-0401/release/src
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(example "/home/emamynt/opencmiss/iron-tests/examples/example-0401/release/src/example")
set_tests_properties(example PROPERTIES  ENVIRONMENT "LD_LIBRARY_PATH=/home/emamynt/opencmiss/iron/install/x86_64_linux/gnu-5.4-F5.4/openmpi_release/release/bin:")
