context("dsClient::ds.replaceNA")

options(datashield.variables=list("LAB_HDL"))
source("setup.R")
#
# Tests
#

#assume ds.danger function is available - fetch the data
context("dsClient::ds.replaceNA")
danger.LAB_HDL.before <- ds.danger('D$LAB_HDL')


#replace the NAs with means on the client side
danger.LAB_HDL.after <- danger.LAB_HDL.before
size <- length(danger.LAB_HDL.before)
means <- vector(mode = "numeric", length = size)
#inds <- vector(mode = "numeric", length = size)
inds <- list()
for (i in length(danger.LAB_HDL.before)){
  means[i] <- mean(x = danger.LAB_HDL.before[[i]],na.rm = TRUE)
  inds[[i]] <- as.numeric(is.na(danger.LAB_HDL.after[[i]]))
  danger.LAB_HDL.after[[i]][is.na(danger.LAB_HDL.after[[i]])] <- means[i]
}
names(x = inds) <- names(x = danger.LAB_HDL.after)

#replace the NAs with means using the DS function
meanVals <- ds.mean(x='D$LAB_HDL', type='split')
ds.replaceNA(x='D$LAB_HDL', forNA=meanVals, newobj='HDL.noNA', indobj = 'HDL.ind')
danger.HDL.noNA <- ds.danger('HDL.noNA')

#compare the results, they should be the same
test_that("replaceNA HDL", {
  expect_equal(danger.LAB_HDL.after, danger.HDL.noNA)
})

#fetch the indicator vector
danger.HDL.ind <- ds.danger('HDL.ind')

#compare the results, they should be the same
test_that("replaceNA HDL", {
  expect_equal(inds, danger.HDL.ind)
})

context("dsClient::ds.replaceNA() test errors")
test_that("replaceNA_erros", {
    expect_error(ds.replaceNA(), "Please provide the name of a vector!", fixed=TRUE)

})
#
# Tear down
#

source("teardown.R")