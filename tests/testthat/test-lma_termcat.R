context('lma_termcat')

words = vapply(seq_len(5e3), function(w)
  paste0(sample(letters, sample(9, 1)), collapse = ''), '')

test_that('term.weights work', {
  category = structure(rnorm(100), names = sample(words, 100))
  sepcat = list(terms = names(category), weights = as.numeric(category))
  dtm = matrix(rpois(100 * 10, 1), 100,
    dimnames = list(NULL, sample(names(category), 10)))
  score = (dtm %*% category[colnames(dtm)])[, 1]
  expect_equal(lma_termcat(dtm, category)[, 1], score)
  expect_equal(lma_termcat(dtm, list(a = category), list(a = category))[, 1], score)
  expect_equal(lma_termcat(dtm, sepcat$terms, sepcat$weights)[, 1], score)
})

test_that('bias works', {
  category = c(`_intercept` = 5, structure(rep(1, 100), names = sample(words, 100)))
  sepcat = list(terms = names(category), weights = as.numeric(category))
  dtm = matrix(rpois(100 * 10, 1), 100,
    dimnames = list(NULL, sample(names(category)[-1], 10)))
  score = (dtm %*% category[colnames(dtm)])[, 1] + 5
  expect_equal(lma_termcat(dtm, category)[, 1], score)
  expect_equal(lma_termcat(dtm, category[-1], bias = 5)[, 1], score)
  expect_equal(lma_termcat(dtm, list(a = category), list(a = category))[, 1], score)
  expect_equal(lma_termcat(dtm, sepcat$terms, sepcat$weights)[, 1], score)
  expect_equal(lma_termcat(dtm, sepcat$terms[-1], sepcat$weights[-1], 5)[, 1], score)
})

test_that('escape works', {
  dtm = matrix(c(1, 0, 0, 1, 1, 0), 2, dimnames = list(NULL, c('aba', 'a.a', ':[')))
  expect_equal(lma_termcat(dtm, 'a.a')[, 1], c(1, 1))
  expect_equal(lma_termcat(dtm, 'a.a', escape = TRUE)[, 1], c(0, 1))
  expect_equal(lma_termcat(dtm, ':[')[, 1], c(1, 0))
  expect_equal(lma_termcat(dtm, ':[', escape = TRUE)[, 1], c(1, 0))
})

test_that('partial works', {
  dtm = matrix(c(1, 0, 0, 1), 2, dimnames = list(NULL, c('aba', 'abaaa')))
  expect_equal(lma_termcat(dtm, 'aba')[, 1], c(1, 0))
  expect_equal(lma_termcat(dtm, 'aba', partial = TRUE)[, 1], c(1, 1))
})

test_that('term.filter works', {
  dtm = matrix(c(1, 0, 0, 1), 2, dimnames = list(NULL, c('aba_', 'abaaa_dwe')))
  expect_equal(lma_termcat(dtm, 'aba', term.filter = '_.*')[, 1], c(1, 0))
  expect_equal(lma_termcat(dtm, 'aba', partial = TRUE, term.filter = '_.*')[, 1], c(1, 1))
})

test_that('term.break works', {
  dict = list(short = c('1', '2'), long = words)
  dtm = matrix(c(1, 0, 0, 1), 2, dimnames = list(NULL, c(dict$long[1], '1')))
  full = lma_termcat(dtm, dict)
  expect_equivalent(full, lma_termcat(dtm, dict, term.break = 1e3))
  expect_equal(full[, 2], lma_termcat(dtm, dict$long, term.break = 1e3)[, 1])
  expect_equal(as.numeric(full), c(0, 1, 1, 0))
})
