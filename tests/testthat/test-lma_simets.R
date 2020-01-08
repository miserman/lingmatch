context('lma_simets')

test_that('entry order is arbitrary for vector-matrix comparisons', {
  dtm = Matrix(rpois(500, 1), 20, sparse = TRUE)
  comp = rpois(25, 1)
  sims_ab = lma_simets(dtm, comp)
  sims_ba = lma_simets(comp, dtm)
  expect_true(all(dim(sims_ab) == c(20, 5) & dim(sims_ba) == c(20, 5)))
  expect_equivalent(sims_ab, sims_ba)
})

test_that('results align with r implementation', {
  manual = function(a, b) c(
    jaccard = sum(a & b) / sum(a | b),
    euclidean = 1 / (1 + sqrt(sum((a - b) ^ 2))),
    canberra = 1 - mean(abs(a - b) / (abs(a) + abs(b))),
    cosine = sum(a * b) / sqrt(sum(a ^ 2 * sum(b ^ 2))),
    pearson = (mean(a * b) - (mean(a) * mean(b))) / sqrt(mean(a ^ 2) - mean(a) ^ 2) /
      sqrt(mean(b ^ 2) - mean(b) ^ 2)
  )
  lapply(seq_len(10), function(i){
    a = rnorm(1000)
    b = rnorm(1000)
    expect_equivalent(lma_simets(a, b), manual(a, b))
  })
})

test_that('pearson aligns with cor', {
  a = matrix(rnorm(500), 20)
  expect_true(all(abs(
    lma_simets(a, metric = 'pearson', symmetric = TRUE)[[1]] - cor(t(a))
  ) < 1e-13))
})

test_that('groups work as expected', {
  dtm = as.data.frame(matrix(rpois(500, 1), 50))
  group = rep(seq_len(10), each = 5)
  expect_equal(
    lma_simets(dtm, metric = 'pearson', group = group, agg = FALSE)[, 1],
    vapply(1:9 * 5, function(r) cor(as.numeric(dtm[r, ]), as.numeric(dtm[r + 1, ])), 0)
  )
  agg = do.call(rbind, lapply(split(dtm, group), colMeans))
  expect_equal(
    lma_simets(dtm, metric = 'pearson', group = group)[, 1],
    vapply(2:10, function(r) cor(agg[r, ], agg[r - 1, ]), 0)
  )
})
