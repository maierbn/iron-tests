# CMake generated Testfile for 
# Source directory: /store/software/opencmiss/iron-tests/examples/example-0401/src
# Build directory: /store/software/opencmiss/iron-tests/examples/example-0401/debug/src
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(example "/store/software/opencmiss/iron-tests/examples/example-0401/debug/src/example")
set_tests_properties(example PROPERTIES  ENVIRONMENT "LD_LIBRARY_PATH=/store/software/opencmiss/iron_maierbn/install/x86_64_linux/gnu-5.4-F5.4/openmpi_release/debug/bin:")
