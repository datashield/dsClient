context("dsClient::ds.numNA")

options(datashield.variables=list("LAB_HDL"))
source("setup.R")
#
# Tests
#

#assume danger function is available - fetch the data
context("dsClient::ds.numNA")
danger.LAB_HDL <- ds.danger('D$LAB_HDL')
miss_count <- vector(mode = 'numeric', length = 0)
for (i in length(danger.LAB_HDL)){
  miss_count[i] <- sum(is.na(danger.LAB_HDL[[i]]))
}

#run the function and compare to actual data
output <- ds.numNA(x='D$LAB_HDL')
test_that("numNA HDL", {
  for (i in length(miss_count)){
    expect_equal(miss_count[i],output[[i]])
  }
})

context("dsClient::ds.numNA() test errors")
test_that("numNA_errors", {

  expect_error(ds.numNA(), "Please provide the name of a vector!", fixed=TRUE)
  expect_error(ds.numNA(x='not_there'), "Error : The input object(s) not_there is(are) not defined on one or more of the studies!\n", fixed=TRUE)

  
})

#
# Tear down
#

source("teardown.R")