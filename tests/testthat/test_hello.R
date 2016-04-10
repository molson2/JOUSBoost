test_that("grid_probs", {
  delta = 10
  q = 1:(delta-1)/delta
  median_loc = 5
  expect_error(grid_probs(X, q, delta, median_loc))
})

test_that("index_over",{
  delta = 10
  ix_pos = 1:10
  ix_neg = 11:15
  nplus = length(ix_pos)
  nneg = length(ix_neg)
  out = index_over(ix_pos, ix_neg, delta)
  # check lengths
  expect_equal(sapply(out$ix_pos_cut, length), (8:0)*nplus)
  expect_equal(sapply(out$ix_neg_cut, length), (0:8)*nneg)
  # check nesting
})

test_that("index_under",{
  delta = 10
  ix_pos = 1:10
  ix_neg = 11:20
  nplus = length(ix_pos)
  nneg = length(ix_neg)
  out = index_under(ix_pos, ix_neg, delta)
  # check length


  # check nesting
})
