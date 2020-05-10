context('lma_simets')

manual = function(a, b) as.numeric(c(
  jaccard = sum(a & b) / sum(a | b),
  euclidean = 1 / (1 + sqrt(sum((a - b) ^ 2))),
  canberra = 1 - mean(abs(a - b) / (abs(a) + abs(b) + 1e-9)),
  cosine = sum(a * b) / sqrt(sum(a ^ 2 * sum(b ^ 2))),
  pearson = (mean(a * b) - (mean(a) * mean(b))) / sqrt(mean(a ^ 2) - mean(a) ^ 2) /
    sqrt(mean(b ^ 2) - mean(b) ^ 2)
))

test_that('results align with r implementation', {
  lapply(seq_len(10), function(i){
    a = Matrix(rpois(20, 1), nrow = 1, sparse = TRUE)
    b = Matrix(rpois(20, 1), nrow = 1, sparse = TRUE)
    expect_equal(as.numeric(lma_simets(a, b)), manual(a, b), tolerance = 1e-7)
  })
})

test_that('many a to one b comparisons work', {
  dtm = Matrix(rpois(500, .5), 5, sparse = TRUE)
  comp = rpois(100, .5)
  sims_ab = lma_simets(dtm, comp)
  sims_sq = vapply(lma_simets(rbind(comp, dtm)), function(r) r[-1, 1], numeric(5))
  sims_m = do.call(rbind, lapply(1:5, function(r) manual(dtm[r,], comp)))
  expect_equal(as.numeric(sims_sq), as.numeric(sims_m))
  expect_equal(as.numeric(as.matrix(sims_ab)), as.numeric(sims_m))
})

test_that('a row to b row comparisons work', {
  dtm = Matrix(rpois(200, .5), 4, sparse = TRUE)
  comp = Matrix(rpois(200, .5), 4, sparse = TRUE)
  sims_ab = lma_simets(dtm, comp)
  sims_m = as.data.frame(do.call(rbind, lapply(1:4, function(r) manual(dtm[r,], comp[r,]))))
  expect_equivalent(sims_ab, sims_m, tolerance = 1e-7)
})

test_that('entry order is arbitrary for vector-matrix comparisons', {
  dtm = Matrix(rpois(1000, .5), 10, sparse = TRUE)
  comp = Matrix(rpois(100, .5), nrow = 1, sparse = TRUE)
  sims_ab = lma_simets(dtm, comp)
  sims_ba = lma_simets(comp, dtm)
  expect_true(all(dim(sims_ab) == c(10, 5) & dim(sims_ba) == c(10, 5)))
  expect_equal(sims_ab, sims_ba)
})

test_that('pearson aligns with cor', {
  a = matrix(rpois(500, 1), 20)
  expect_equal(as.numeric(lma_simets(a, metric = 'pearson', symmetric = TRUE)), as.numeric(cor(t(a))))
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
