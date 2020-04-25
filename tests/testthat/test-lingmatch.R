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
  manual = lma_simets(dtm, colMeans(dtm), metric = 'cosine')
  expect_equivalent(lingmatch(texts)$sim, manual)
  expect_equivalent(lingmatch(dtm)$sim, manual)
})

test_that('different input formats have the same results (LSM)', {
  wdtm = lma_weight(dtm, percent = TRUE)
  cdtm = lma_termcat(wdtm)
  manual = lma_simets(cdtm, colMeans(cdtm), 'canberra')
  expect_equivalent(lingmatch(texts, type = 'lsm')$sim, manual)
  expect_equivalent(lingmatch(dtm, type = 'lsm')$sim, manual)
  expect_equivalent(lingmatch(wdtm, type = 'lsm')$sim, manual)
  expect_equivalent(lingmatch(cdtm, type = 'lsm')$sim, manual)
})

test_that('different input formats have the same results (LSA)', {
  space = lma_lspace(dtm)
  wdtm = lma_weight(dtm, 'tfidf')
  cdtm = lma_lspace(wdtm, space)
  manual = lma_simets(cdtm, colMeans(cdtm), metric = 'cosine')
  expect_equivalent(lingmatch(texts, space = space, type = 'lsa')$sim, manual)
  expect_equivalent(lingmatch(dtm, space = space, type = 'lsa')$sim, manual)
  expect_equivalent(lingmatch(wdtm, space = space, type = 'lsa')$sim, manual)
})
