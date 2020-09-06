context('lma_weights')

dtm = sparseMatrix(sample(50, 900, TRUE), sample(100, 900, TRUE), x = rpois(900, 2) + 1)
dtm[sample(length(dtm), 5)] = NA

test_that('normalization work', {
  manual = dtm
  wc = rowSums(manual, na.rm = TRUE)
  su = wc != 0
  manual[, su] = manual[su,] / wc[su]
  i = which(colSums(dtm, na.rm = TRUE) != 0)[1]
  expect_equal(dtm[i,] / sum(dtm[i,], na.rm = TRUE), manual[i,])
  manual = as.numeric(manual)
  wdtm = lma_weight(dtm)
  expect_equivalent(rowSums(wdtm, na.rm = TRUE), rep(1, nrow(dtm)))
  expect_equal(as.numeric(wdtm), manual)
  expect_equal(as.numeric(lma_weight(as.matrix(dtm))), manual)
  expect_equal(as.numeric(lma_weight(dtm, percent = TRUE)), manual * 100)
})

test_that('document weighting works', {
  manual = dtm
  df = colSums(manual > 0, na.rm = TRUE)
  manual = manual * rep(df, each = nrow(manual))
  expect_equal(dtm[, 1] * df[1], manual[, 1])
  manual = as.numeric(manual)
  expect_equal(as.numeric(lma_weight(dtm, 'df', FALSE)), manual)
  expect_equal(as.numeric(lma_weight(as.matrix(dtm), 'df', FALSE)), manual)
})

test_that('term-document weighting works', {
  idf = lma_weight(dtm, 'idf', FALSE, doc.only = TRUE)
  expect_equal(log10(nrow(dtm) / colSums(dtm != 0, na.rm = TRUE)), idf)
  manual = dtm * rep(idf, each = nrow(dtm))
  expect_equal(dtm[, 1] * idf[1], manual[, 1])
  manual = as.numeric(manual)
  expect_equal(as.numeric(lma_weight(dtm, 'tfidf', FALSE)), manual)
  expect_equal(as.numeric(lma_weight(as.matrix(dtm), 'tf-idf', FALSE)), manual)
})

test_that('pmi works', {
  manual = dtm / sum(dtm, na.rm = TRUE)
  manual = as.numeric(log2(manual /
    outer(rowSums(manual, na.rm = TRUE), colSums(manual, na.rm = TRUE))))
  manual[!is.finite(manual)] = 0
  expect_equal(as.numeric(lma_weight(dtm, 'pmi')), manual)
  expect_equal(as.numeric(lma_weight(as.matrix(dtm), 'pmi')), manual)
})
