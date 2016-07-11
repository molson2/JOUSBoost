
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

test_that("adaBoost",{
  N = 250
  X = matrix(rnorm(N*5), N)
  y = 2*rbinom(N, 1, .2*(rowSums(X) > 0) + .8*(rowSums(X) < 0)) - 1
  ada = adaBoost(X = X, y = y, tree_depth = 5, n_rounds = 100)
  y_hat = predict(ada, X)

})

test_that("jous",{

  # load data set
  data(sonar)
  # prediction/classification functions
  library(randomForest)
  class_func = function(X, y, ...) randomForest(X, as.factor(y), proximity = F,
                                           oob.prox = F, ...)
  pred_func = function(obj, newdata) {
    zz = predict(obj, newdata)
    as.numeric(levels(zz)[as.integer(zz)])
  }
  # over
  jous_obj = jous(as.matrix(sonar[,-61]), sonar[,61],
                  class_func,
                  pred_func,
                  type="over",
                  delta = 10,
                  nu=1)
  rm(jous_obj)

  # under
  #jous_obj = jous(as.matrix(sonar[,-61]), sonar[,61],
  #                class_func,
  #                pred_func,
  #                type="under",
  #                delta = 10,
  #                nu=1)
  rm(jous_obj)

})

###############################################################################
#                              ITEMS TO FIX
###############################################################################

# Make most functions private
# make adaboost leaner !!!


