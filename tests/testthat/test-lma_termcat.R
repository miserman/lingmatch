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
  expect_equal(lma_termcat(dtm, as.data.frame(sepcat), 'weights')[, 1], score)
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
  expect_equal(lma_termcat(dtm, 'a.a', escape = FALSE)[, 1], c(1, 1))
  expect_equal(lma_termcat(dtm, 'a.a')[, 1], c(0, 1))
  expect_equal(lma_termcat(dtm, ':[', escape = FALSE)[, 1], c(1, 0))
  expect_equal(lma_termcat(dtm, ':[')[, 1], c(1, 0))
})

test_that('partial/glob works', {
  dtm = matrix(c(1, 0, 0, 1), 2, dimnames = list(NULL, c('aba', 'ababb')))
  expect_equal(as.numeric(lma_termcat(dtm, list('aba', 'aba*'))), c(1, 0, 1, 1))
  expect_equal(lma_termcat(dtm, 'aba*', glob = FALSE)[, 1], c(0, 0))
  expect_equal(lma_termcat(dtm, 'aba', partial = TRUE)[, 1], c(1, 1))
  expect_equal(as.numeric(lma_termcat(dtm, list('aba.*', 'aba[b]*', 'aba.?.?', 'aba.{0,2}'))), rep(1, 8))
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

test_that('wide dict format works', {
  dict = data.frame(term = c('a', 'b', 'c'), w1 = c(1, 2, 3), w2 = c(.1, .2, .3))
  dtm = matrix(rep(1:5, 3), 5, 3, dimnames = list(NULL, c('a', 'b', 'c')))
  manual = cbind(dtm %*% dict$w1, dtm %*% dict$w2)
  expect_equivalent(lma_termcat(dtm, dict), manual)
  expect_equivalent(lma_termcat(dtm, dict$term, dict), manual)
  expect_equivalent(lma_termcat(dtm, dict$term, dict[, -1]), manual)
})

textdir = '../'
if(!file.exists(paste0(textdir, 'texts.txt'))) textdir = paste0('../../', textdir)
skip_if(!file.exists(paste0(textdir, 'texts.txt')), paste('texts.txt not found in', normalizePath(textdir)))

dicts = list.files(getOption('lingmatch.dict.dir'), '(?:csv|dic)$', full.names = TRUE)
skip_if(!length(dicts), 'no .csv or .dic files in the dictionaries directory')

test_that('applied dictionaries work', {
  text = sample(readLines(paste0(textdir, 'texts.txt')), 10)
  dtm = lma_dtm(text)
  for(d in dicts){
    dict = (if(grepl('.csv', d, fixed = TRUE)) read.csv else read.dic)(d)
    opt = lma_termcat(dtm, dict)
    opp = lma_patcat(text, dict)
    expect_equal(dim(opt), dim(opp))
    expect_true(all(colnames(opt) %in% colnames(opp)))
  }
})
