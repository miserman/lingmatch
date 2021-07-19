context('lma_simets')

manual = function(a, b) as.numeric(c(
  jaccard = sum(a & b) / sum(a | b),
  euclidean = 1 / (1 + sqrt(sum((a - b) ^ 2))),
  canberra = 1 - mean(abs(a - b) / (abs(a) + abs(b) + 1e-13)),
  cosine = sum(a * b) / sqrt(sum(a ^ 2 * sum(b ^ 2))),
  pearson = (mean(a * b) - (mean(a) * mean(b))) / sqrt(mean(a ^ 2) - mean(a) ^ 2) /
    sqrt(mean(b ^ 2) - mean(b) ^ 2)
))

test_that('results align with r implementation', {
  lapply(seq_len(10), function(i){
    a = Matrix(rpois(20, 1), nrow = 1, sparse = TRUE)
    b = Matrix(rpois(20, 1), nrow = 1, sparse = TRUE)
    expect_equal(as.numeric(lma_simets(a, b)), manual(a, b), tolerance = 1e-13)
  })
})

test_that('vectors and rows are the same', {
  a = b = matrix(0, 2, 10)
  a[2,] = rpois(10, 1)
  b[2,] = rnorm(10)
  pa = lma_simets(rbind(a, b))
  al = lma_simets(a, b)
  expect_equal(as.numeric(al[1,]), unname(vapply(pa, '[', 0, 3)))
  expect_equal(as.numeric(al[2,]), unname(vapply(pa, '[', 0, 8)))
  cr = rbind(lma_simets(a[1,], b[2,]), lma_simets(a[2,], b[1,]))
  cr[is.na(cr)] = 0
  mcr = rbind(manual(a[1,], b[2,]), manual(a[2,], b[1,]))
  mcr[is.nan(mcr)] = 0
  expect_equivalent(cr, mcr)
  expect_equal(cr, vapply(pa, '[', numeric(2), c(4, 7)))
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

test_that('a vector to b matrix works', {
  expect_equal(as.numeric(lma_simets(c(0, 1, 1), matrix(c(0, 0, 1, 1, 1, 0, 1, 1, 0), 3), 'cor')), c(1, 1, -1))
})

test_that('a row to b row comparisons work', {
  dtm = Matrix(rpois(200, .5), 4, sparse = TRUE)
  comp = Matrix(rpois(200, .5), 4, sparse = TRUE)
  sims_ab = lma_simets(dtm, comp)
  sims_m = as.data.frame(do.call(rbind, lapply(1:4, function(r) manual(dtm[r,], comp[r,]))))
  expect_equivalent(sims_ab, sims_m, tolerance = 1e-7)
})

test_that('text inputs and differing a-b columns works', {
  words = vapply(1:50, function(i) paste(sample(letters, sample(7), TRUE), collapse = ''), '')
  text = vapply(1:10, function(i) paste(sample(words[seq(1, if(i < 5) 50 else 40)], 50, TRUE), collapse = ' '), '')
  dtm = lma_dtm(text[1:5])
  expect_equal(as.numeric(lma_simets(text[1:5], 'cos')), as.numeric(lma_simets(dtm, 'cos')))
  comp = lma_dtm(text[5:10])
  expect_equal(
    as.numeric(lma_simets(text[1:5], text[5:10], 'cos')),
    as.numeric(lma_simets(dtm, comp, 'cos'))
  )
})

test_that('entry order is arbitrary for vector-matrix comparisons', {
  dtm = Matrix(rpois(1000, .5), 10, sparse = TRUE)
  comp = Matrix(rpois(100, .5), nrow = 1, sparse = TRUE)
  sims_ab = lma_simets(dtm, comp)
  sims_ba = lma_simets(comp, dtm)
  expect_equivalent(sims_ab, sims_ba)
  sims_ab = lma_simets(dtm, as.numeric(comp))
  sims_ba = lma_simets(as.numeric(comp), dtm)
  expect_equivalent(sims_ab, sims_ba)
})

test_that('pearson aligns with cor', {
  a = matrix(rnorm(500), 20)
  expect_equal(as.numeric(lma_simets(a, metric = 5, symmetric = TRUE)), as.numeric(cor(t(a))))
  a = matrix(rpois(500, 1), 20)
  expect_equal(as.numeric(lma_simets(a, metric = 'correlation', symmetric = TRUE)), as.numeric(cor(t(a))))
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

test_that('lag works', {
  dtm = Matrix(rpois(200, 1), 10, sparse = TRUE)
  dtm[2, c(1, 20)] = 1

  # +1
  expect_equal(
    as.numeric(lma_simets(dtm[1,], metric = 'pearson', lag = 1)),
    cor(dtm[1,], c(0, dtm[1, -20]))
  )
  expect_equal(
    as.numeric(lma_simets(dtm[1,], dtm[2,], metric = 'pearson', lag = 1)),
    cor(dtm[1,], c(0, dtm[2, -20]))
  )
  expect_equal(
    as.numeric(lma_simets(dtm[1:5,], dtm[6:10,], metric = 'pearson', lag = 1)),
    c(0, lma_simets(dtm[2:5,], dtm[6:9,], metric = 'pearson'))
  )

  # -1
  expect_equal(
    as.numeric(lma_simets(dtm[1,], dtm[2,], metric = 'pearson', lag = -1)),
    cor(dtm[1,], c(dtm[2, -1], 0))
  )
  expect_equal(
    as.numeric(lma_simets(dtm[1:5,], dtm[6:10,], metric = 'pearson', lag = -1)),
    c(lma_simets(dtm[1:4,], dtm[7:10,], metric = 'pearson'), 0)
  )

  # +oversize
  expect_equal(
    as.numeric(lma_simets(dtm[1,], dtm[2,], metric = 'pearson', lag = 99)),
    cor(dtm[1,], c(numeric(19), dtm[2, 1]))
  )
  expect_equal(
    as.numeric(lma_simets(dtm[1:5,], dtm[6:10,], metric = 'pearson', lag = 99)),
    c(numeric(4), unname(lma_simets(dtm[5,], dtm[6,], metric = 'pearson')))
  )

  # -oversize
  expect_equal(
    as.numeric(lma_simets(dtm[1,], dtm[2,], metric = 'pearson', lag = -99)),
    cor(dtm[1,], c(dtm[2, 20], numeric(19)))
  )
  expect_equal(
    as.numeric(lma_simets(dtm[1:5,], dtm[6:10,], metric = 'pearson', lag = -99)),
    c(unname(lma_simets(dtm[1,], dtm[10,], metric = 'pearson')), numeric(4))
  )
})
