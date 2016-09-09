
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

test_that("index_over_2", {

  npos = 430
  nneg = 222
  delta = 10
  q = (1:(delta-1))/delta
  out = index_over(1:npos, (npos+1):(npos+nneg), q)

  out_test_plus = sapply(1:(delta-1), function(x) length(out$ix_pos_cut[[x]]) + npos )
  out_test_neg = sapply(1:(delta-1), function(x) length(out$ix_neg_cut[[x]]) + nneg )

  out_expect_plus = (delta*(1-q)*npos); out_expect_plus[q == 0.5] = npos
  out_expect_neg = (delta*q*nneg); out_expect_neg[q == 0.5] = nneg

  expect_equal(out_test_plus/out_expect_plus, rep(1, delta-1), tol=0.01)
  expect_equal(out_test_neg/out_expect_neg, rep(1, delta-1), tol=0.01)

})

test_that("index_under_2",{
  npos = 557
  nneg = 253
  delta = 6
  q = (1:(delta-1))/delta
  out = index_under(1:npos, (npos+1):(npos+nneg), q, delta)

  out_test_plus = sapply(1:(delta-1), function(x) length(out$ix_pos_cut[[x]]))
  out_test_neg = sapply(1:(delta-1), function(x) length(out$ix_neg_cut[[x]]))

  out_expect_plus = (1-q)*npos; out_expect_plus[q == 0.5] = npos
  out_expect_neg = q*nneg; out_expect_neg[q == 0.5] = nneg

  expect_equal(out_test_plus/out_expect_plus, rep(1, delta-1), tol=0.05)
  expect_equal(out_test_neg/out_expect_neg, rep(1, delta-1), tol=0.05)
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

test_that("adaBoost",{
  N = 1000
  dat = friedman_data(N)
  ada = adaBoost(X = dat$X, y = dat$y, tree_depth = 2, n_rounds = 100)
  y_hat = predict(ada, dat$X)
})



