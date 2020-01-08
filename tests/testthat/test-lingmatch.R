context('lingmatch')

words = c(
  sub('*', '', unlist(lma_dict(1:9, as.regex = FALSE), use.names = FALSE), fixed = TRUE),
  vapply(seq_len(200), function(w) paste0(sample(letters, sample(9, 1)), collapse = ''), '')
)
texts = vapply(seq_len(50), function(d){
  paste0(sample(words, sample(100, 1), TRUE), collapse = ' ')
}, '')
dtm = lma_dtm(texts)

test_that('different input formats have the same results', {
  manual = lma_simets(dtm, colMeans(dtm), metric = 'cosine')[[1]]
  expect_equal(lingmatch(texts)$sim[[1]], manual)
  expect_equal(lingmatch(dtm)$sim[[1]], manual)
})

test_that('different input formats have the same results (LSM)', {
  wdtm = lma_weight(dtm, percent = TRUE)
  cdtm = lma_termcat(wdtm)
  manual = lma_simets(cdtm, colMeans(cdtm), metric = 'canberra')[[1]]
  dict = lma_dict(1:9)
  expect_equal(lingmatch(texts, dict = dict, type = 'lsm')$sim[[1]], manual)
  expect_equal(lingmatch(dtm, type = 'lsm')$sim[[1]], manual)
  expect_equal(lingmatch(wdtm, type = 'lsm')$sim[[1]], manual)
  expect_equal(lingmatch(cdtm, type = 'lsm')$sim[[1]], manual)
})

test_that('different input formats have the same results (LSA)', {
  space = lma_lspace(dtm)
  wdtm = lma_weight(dtm, 'tfidf')
  cdtm = lma_lspace(wdtm, space)
  manual = lma_simets(cdtm, colMeans(cdtm), metric = 'cosine')[[1]]
  expect_equal(lingmatch(texts, space = space, type = 'lsa')$sim[[1]], manual)
  expect_equal(lingmatch(dtm, space = space, type = 'lsa')$sim[[1]], manual)
  expect_equal(lingmatch(wdtm, space = space, type = 'lsa')$sim[[1]], manual)
})
