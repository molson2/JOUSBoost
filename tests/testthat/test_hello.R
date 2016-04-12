
test_that("index_over", {
  npos = 100
  nneg = 50
  out = index_over(1:npos, 1:nneg, q = (1:9)/10)
  expect_equal(sapply(out$ix_pos_cut, length),
               npos*c(8, 7, 6, 5, 0, 3, 2, 1, 0))
  expect_equal(sapply(out$ix_neg_cut, length),
               nneg*c(0, 1, 2, 3, 0, 5, 6, 7, 8))
})

test_that("index_under",{
  npos = 100
  nneg = 50
  out = index_under(1:npos, 1:nneg, q = (1:9)/10, 10)
  expect_equal(sapply(out$ix_pos_cut, length),
               floor(npos*(c(9, 8, 7, 6, 10, 4, 3, 2, 1)/10)))
  expect_equal(sapply(out$ix_neg_cut, length),
               floor(nneg*(c(1, 2, 3, 4, 10, 6, 7, 8, 9)/10)))
})

test_that("grid_probs", {
  q = c(1/3, .5, 2/3)
  delta = 3
  X = rbind(c(1, 1, -1),
            c(1, 1, 1),
            c(-1, -1, 1),
            c(1, -1, 1))

  expect_error(grid_probs(X, q, delta, 0))
  expect_equal(grid_probs(X, q, delta, 1),
               c(2/3-1/6, 1-1/6, 1/6, 1/3+1/6))
})

test_that("JOUS.predict", {

})

test_that("jous",{
  # load data set
  data(sonar)
  # prediction/classification functions
  jous_obj = jous()



})

###############################################################################
#                              ITEMS TO FIX
###############################################################################

# Probably still need to find better way to incorporate including median ...
# maybe leave it out separately? (This is going to fuck with the sampling part)






