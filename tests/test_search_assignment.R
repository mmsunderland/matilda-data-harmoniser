# Run with (from harmoniser/ directory):
#   library(shinytest2)
#   test_app(".")
#
# shinytest2::test_app() looks in tests/testthat/ — this file is an alias.
# The actual tests are in tests/testthat/test_search_assignment.R

source(file.path(dirname(sys.frame(1)$ofile), "testthat", "test_search_assignment.R"))
