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
  manual = as.numeric(lma_simets(dtm, metric = 'cosine'))
  expect_equal(as.numeric(lingmatch(texts)$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm)$sim), manual)
})

test_that('different input formats have the same results (LSM)', {
  wdtm = lma_weight(dtm, percent = TRUE)
  cdtm = lma_termcat(wdtm)
  manual = as.numeric(lma_simets(cdtm, 'canberra'))
  expect_equal(as.numeric(lingmatch(texts, type = 'lsm')$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm, type = 'lsm')$sim), manual)
  expect_equal(as.numeric(lingmatch(wdtm, type = 'lsm')$sim), manual)
  expect_equal(as.numeric(lingmatch(cdtm, type = 'lsm')$sim), manual)
})

test_that('different input formats have the same results (LSA)', {
  space = lma_lspace(dtm)
  wdtm = lma_weight(dtm, 'tfidf')
  cdtm = lma_lspace(wdtm, space)
  manual = as.numeric(lma_simets(cdtm, metric = 'cosine'))
  expect_equal(as.numeric(lingmatch(texts, space = space, type = 'lsa')$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm, space = space, type = 'lsa')$sim), manual)
  expect_equal(as.numeric(lingmatch(wdtm, space = space, type = 'lsa')$sim), manual)
  expect_equal(as.numeric(lingmatch(cdtm, type = 'lsa')$sim), manual)
})

test_that('comparison columns align (named)', {
  wdtm = lma_weight(dtm, percent = TRUE)
  cdtm = lma_termcat(wdtm)
  manual = as.numeric(lma_simets(cdtm, cdtm[1:5,], 'canberra'))
  expect_equal(as.numeric(lingmatch(texts, cdtm[1:5,], type = 'lsm')$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm, cdtm[1:5,], type = 'lsm')$sim), manual)
  expect_equal(as.numeric(lingmatch(wdtm, cdtm[1:5,], type = 'lsm')$sim), manual)
  expect_equal(as.numeric(lingmatch(cdtm, cdtm[1:5,], type = 'lsm')$sim), manual)
})

test_that('comparison columns align (unnamed)', {
  space = lma_lspace(dtm)
  wdtm = lma_weight(dtm, 'tfidf')
  cdtm = lma_lspace(wdtm, space)
  manual = as.numeric(lma_simets(cdtm, cdtm[1:5,], metric = 'cosine'))
  expect_equal(as.numeric(lingmatch(texts, cdtm[1:5,], space = space, type = 'lsa')$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm, cdtm[1:5,], space = space, type = 'lsa')$sim), manual)
  expect_equal(as.numeric(lingmatch(wdtm, cdtm[1:5,], space = space, type = 'lsa')$sim), manual)
  expect_equal(as.numeric(lingmatch(cdtm, cdtm[1:5,], space = space, type = 'lsa')$sim), manual)
})

test_that('function comparisons work', {
  expect_equal(
    as.numeric(lingmatch(dtm, sum)$sim),
    as.numeric(lingmatch(dtm, colSums(dtm))$sim)
  )
})

test_that('profile comparisons work', {
  novels = c(
    ppron = 10.35, ipron = 4.79, article = 8.35, auxverb = 7.77, adverb = 4.17,
    prep = 14.27, conj = 6.28, negate = 1.68, quant = 1.8
  )
  expect_equal(
    as.numeric(lingmatch(dtm, 'novels', type = 'lsm')$sim),
      as.numeric(lingmatch(dtm, novels, type = 'lsm')$sim)
  )
})

test_that('index/logical comparisons work', {
  ind = sample(nrow(dtm), 5)
  su = seq_len(nrow(dtm)) %in% ind
  expect_equal(
    as.numeric(lingmatch(dtm, ind)$sim),
      as.numeric(lingmatch(dtm, dtm[ind,])$sim[-ind,])
  )
  expect_equal(
    as.numeric(lingmatch(dtm, su)$sim),
    as.numeric(lingmatch(dtm, dtm[su,])$sim[!su,])
  )
})

test_that('group comparisons work', {
  wdtm = lma_weight(dtm, percent = TRUE)
  cdtm = lma_termcat(wdtm)
  groups = rep(c('a', 'b'), each = nrow(dtm) / 2)
  group_means = rbind(
    a = colMeans(cdtm[groups == 'a',]),
    b = colMeans(cdtm[groups == 'b',])
  )
  expect_equal(
    lingmatch(dtm, group = groups, type = 'lsm')$sim[, 2],
    rowSums(lingmatch(dtm, group_means, type = 'lsm')$sim * rep(
      c(1, 0, 1), nrow(dtm) / c(2, 1, 2)
    ))
  )
})

test_that('groups work through data', {
  data = data.frame(
    id = as.factor(rep(seq_len(nrow(dtm) / 2), 2)),
    group = as.factor(rep(c('a', 'b'), each = nrow(dtm) / 2)),
    lma_termcat(lma_weight(dtm, percent = TRUE))
  )
  categories = colnames(data)[-(1:2)]
  group_means = t(vapply(split(data[, categories], data$group), colMeans, numeric(9)))
  expect_equal(
    lingmatch(data, group = group, type = 'lsm')$sim[, 2],
    as.numeric(c(
      lma_simets(data[data$group == 'a', categories], group_means['a',], 'canberra'),
      lma_simets(data[data$group == 'b', categories], group_means['b',], 'canberra')
    ))
  )
  id_means = t(vapply(split(data[, categories], data$id), colMeans, numeric(9)))
  expect_equal(
    lingmatch(data, group = id, type = 'lsm')$sim[, 2],
    vapply(seq_len(nrow(data)), function(i){
      r = data[i,]
      lma_simets(r[categories], id_means[r[[1]],], 'canberra')
    }, 0)
  )
})

test_that('sequential comparisons work', {
  data = data.frame(
    pair = sort(sample(1:4, nrow(dtm), TRUE)),
    phase = 0,
    speaker = sample(c('a', 'b'), nrow(dtm), TRUE),
    lma_termcat(lma_weight(dtm, percent = TRUE))
  )
  for(p in 1:4){
    su = data$pair == p
    data$phase[su] = sort(sample(1:3, sum(su), TRUE))
  }
  h = list(
    lingmatch(data, 'seq', group = speaker)$sim,
    lingmatch(data, 'seq', group = 'speaker', agg = FALSE)$sim,
    lingmatch(data, 'seq', group = c('pair', 'speaker'))$sim,
    lingmatch(data, 'seq', group = data[, c('pair', 'speaker')], agg = FALSE)$sim,
    lingmatch(data, 'seq', group = data[, c('pair', 'phase', 'speaker')])$sim,
    lingmatch(data, 'seq', group = c('pair', 'phase', 'speaker'), agg = FALSE)$sim
  )
  expect_true(any(h[[1]][, 'cosine'] - h[[2]][, 'cosine'] > .01))
  expect_true(any(h[[3]][, 'cosine'] - h[[4]][, 'cosine'] > .01))
  expect_true(any(h[[5]][, 'cosine'] - h[[6]][, 'cosine'] > .01))
  for(comp in h){
    manual = vapply(strsplit(rownames(comp), ' <-> ', fixed = TRUE), function(s){
      if(length(s) == 1) 1 else{
        s = strsplit(s, ', ', fixed = TRUE)
        lma_simets(
          colMeans(data[as.numeric(s[[1]]), -(1:3), drop = FALSE]),
          colMeans(data[as.numeric(s[[2]]), -(1:3), drop = FALSE]),
          'cosine'
        )
      }
    }, 0)
    expect_equal(comp[, 'cosine'], manual)
  }
})
