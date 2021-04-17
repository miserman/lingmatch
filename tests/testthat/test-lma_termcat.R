context('lma_termcat')

options(encoding = 'latin1', stringsAsFactors = FALSE)
words = vapply(seq_len(5e3), function(w) paste0(sample(letters, 5), collapse = ''), '')

test_that('term.weights work', {
  category = structure(rnorm(100), names = sample(words, 100))
  dtm = matrix(rpois(100 * 10, 1), 100, dimnames = list(NULL, sample(names(category), 10)))
  score = (dtm %*% category[colnames(dtm)])[, 1]
  names(category) = sub('\\w$', '*', names(category))
  sepcat = data.frame(terms = names(category), weights = as.numeric(category))
  expect_equal(lma_termcat(dtm, category)[, 1], score)
  expect_equal(lma_termcat(dtm, list(a = category), list(a = category))[, 1], score)
  expect_equal(lma_termcat(dtm, sepcat$terms, sepcat$weights)[, 1], score)
  expect_equal(lma_termcat(dtm, paste0('_', sepcat$terms), sepcat$weights)[, 1], numeric(100))
  expect_equal(lma_termcat(dtm, sepcat, 'weights')[, 1], score)
  expect_equal(lma_termcat(dtm, sepcat, 'weights')[, 1], score)
  s2 = lma_termcat(dtm, sepcat$terms, data.frame(a = sepcat$weights, b = sepcat$weights))
  expect_equal(s2[, 1], score)
  expect_equal(s2[, 1], s2[, 2])
  sepcat$value = sepcat$weights * 2
  expect_equal(lma_termcat(dtm, structure(sepcat[, -1], row.names = as.character(sepcat$terms)))[, 1], score)
  expect_equal(lma_termcat(dtm, rbind(sepcat, sepcat[1:4,]))[, 2], score * 2)
  expect_equal(lma_termcat(dtm, sepcat$terms, as.matrix(unname(sepcat[, -1])))[, 2], score * 2)
  expect_equal(lma_termcat(dtm, list(cat = sepcat$terms), list(sepcat$weights))[, 1], score)
  expect_equal(lma_termcat(dtm, list(weights = sepcat$terms), as.list(sepcat[, -1]))[, 1], score)
})

test_that('bias works', {
  category = c(`_intercept` = 5, structure(rep(1, 100), names = sample(words, 100)))
  sepcat = list(terms = names(category), weights = as.numeric(category))
  dtm = matrix(rpois(100 * 10, 1), 100, dimnames = list(NULL, sample(names(category)[-1], 10)))
  score = (dtm %*% category[colnames(dtm)])[, 1] + 5
  expect_equal(lma_termcat(dtm, category)[, 1], score)
  expect_equal(lma_termcat(dtm, category[-1], bias = 5)[, 1], score)
  expect_equal(lma_termcat(dtm, list(a = category), list(a = category))[, 1], score)
  expect_equal(lma_termcat(dtm, sepcat$terms, sepcat$weights)[, 1], score)
  expect_equal(lma_termcat(dtm, sepcat$terms[-1], sepcat$weights[-1], 5)[, 1], score)
  expect_equal(lma_termcat(dtm, as.data.frame(sepcat))[, 1], score)
})

test_that('escape works', {
  dtm = matrix(c(1, 0, 0, 1, 1, 0), 2, dimnames = list(NULL, c('aba', 'a.a', ':[')))
  expect_equal(lma_termcat(dtm, list('a.a'), escape = FALSE)[, 1], c(1, 1))
  expect_equal(lma_termcat(dtm, list('a.a'))[, 1], c(0, 1))
  expect_equal(lma_termcat(dtm, list(':['), escape = FALSE)[, 1], c(1, 0))
  expect_equal(lma_termcat(dtm, list(':['))[, 1], c(1, 0))
})

test_that('partial/glob works', {
  dtm = matrix(c(1, 0, 0, 1), 2, dimnames = list(NULL, c('aba', 'ababb')))
  expect_equal(as.numeric(lma_termcat(dtm, list('aba', 'aba*'))), c(1, 0, 1, 1))
  expect_equal(lma_termcat(dtm, list('aba*'))[, 1], c(1, 1))
  expect_equal(lma_termcat(dtm, list('aba*'), glob = FALSE)[, 1], c(0, 0))
  expect_equal(lma_termcat(dtm, list('aba'))[, 1], c(1, 0))
  expect_equal(lma_termcat(dtm, list('aba'), partial = TRUE)[, 1], c(1, 1))
  expect_equal(as.numeric(lma_termcat(dtm, list('aba.*', 'aba[b]*', 'aba.?.?', 'aba.{0,2}'))), rep(1, 8))
})

test_that('case conversion works', {
  text = vapply(1:5, function(i) paste0(sample(gsub('\\b(\\w)', '\\U\\1', words, perl = TRUE), 50), collapse = ' '), '')
  dtm = lma_dtm(text, to.lower = FALSE)
  dict = colnames(dtm)
  wc = rowSums(dtm)
  expect_equal(lma_termcat(dtm, dict)[, 1], wc)
  expect_equal(lma_patcat(text, dict)[, 1], wc)
  expect_equal(lma_termcat(dtm, tolower(dict))[, 1], wc)
  expect_equal(lma_patcat(text, tolower(dict))[, 1], wc)
  expect_equal(lma_termcat(text, toupper(dict))[, 1], wc)
  expect_equal(lma_patcat(text, toupper(dict))[, 1], wc)
  expect_equal(lma_termcat(dtm, dict, to.lower = FALSE)[, 1], wc)
  expect_equal(lma_patcat(text, dict, to.lower = FALSE)[, 1], wc)
  expect_equal(lma_termcat(dtm, tolower(dict), to.lower = FALSE)[, 1], wc)
  wc = numeric(nrow(dtm))
  expect_equal(lma_patcat(text, tolower(dict), to.lower = FALSE)[, 1], wc)
  expect_equal(lma_termcat(tolower(text), dict, to.lower = FALSE)[, 1], wc)
  expect_equal(lma_patcat(tolower(text), dict, to.lower = FALSE)[, 1], wc)
  expect_equal(lma_termcat(toupper(text), dict, to.lower = FALSE)[, 1], wc)
  expect_equal(lma_patcat(toupper(text), dict, to.lower = FALSE)[, 1], wc)
})

test_that('term.filter works', {
  dtm = matrix(c(1, 0, 0, 1), 2, dimnames = list(NULL, c('aba_', 'abaaa_dwe')))
  expect_equal(lma_termcat(dtm, list('aba'), term.filter = '_.*')[, 1], c(1, 0))
  expect_equal(lma_termcat(dtm, list('aba'), partial = TRUE, term.filter = '_.*')[, 1], c(1, 1))
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
  expect_equivalent(lma_termcat(dtm, structure(dict[, -1], row.names = dict$term)), manual)
  expect_equivalent(lma_termcat(dtm, data.frame(c('a', 'a', 'b'), dict)), manual)
})

test_that('text input works', {
  words = unique(sample(words, 150))
  dict = paste(words[1:50], words[51:100])
  text = vapply(1:10, function(i) paste(sample(dict, 100, TRUE), collapse = ' '), '')
  dtm = lma_dtm(text)
  expect_equal(suppressWarnings(lma_termcat(text, dict)[, 1]), rowSums(dtm) / 2)
  expect_equal(
    suppressWarnings(lma_termcat(text, paste0(dict, '*'), term.weights = rep(.1, length(dict)),
      bias = 10, glob = TRUE, partial = FALSE, dir = '')[, 1]),
    rowSums(dtm) / 2 * .1 + 10
  )
  expect_equal(
    suppressWarnings(lma_termcat(text, dict)[, 1]),
    lma_patcat(text, dict)[, 1]
  )
  dict = unlist(strsplit(dict, ' '))
  expect_equal(lma_termcat(as.factor(text), dict)[, 1], rowSums(dtm))
  expect_equal(lma_termcat(dtm, dict)[, 1], rowSums(dtm))
})

test_that('lma_termcat and lma_patcat are accurate', {
  terms = sample(words, 50)
  dict = do.call(rbind, lapply(c('a', 'b', 'c'), function(cat){
    data.frame(
      term = sample(terms, 30),
      category = cat,
      weight = rnorm(30)
    )
  }))
  text = vapply(1:10, function(i) paste(sample(terms, sample(50:150, 1), TRUE), collapse = ' '), '')
  dtm = lma_dtm(text)
  bias = c(a = .23, b = 1.2, c = 3)
  manual = vapply(names(bias), function(cat){
    cd = dict[dict$category == cat, -2]
    as.numeric(dtm[, cd$term] %*% cd$weight) + bias[[cat]]
  }, numeric(nrow(dtm)))
  dict = rbind(dict, data.frame(term = '_intercept', category = names(bias), weight = bias))
  expect_equal(as.numeric(as.matrix(lma_process(text, dict = dict)[, names(bias)])), as.numeric(manual))
  expect_equal(as.numeric(lma_termcat(text, dict)), as.numeric(manual))
  expect_identical(as.numeric(lma_patcat(text, dict)), as.numeric(manual))
  dict[dict$category == 'a', 'category'] = ''
  expect_equal(as.numeric(lma_termcat(text, dict)), as.numeric(manual))
  expect_identical(as.numeric(lma_patcat(text, dict)), as.numeric(manual))
  weights = split(dict$weight, dict$category)
  dict = split(dict$term, dict$category)
  expect_equal(as.numeric(lma_termcat(text, dict, weights)), as.numeric(manual))
  expect_identical(as.numeric(lma_patcat(text, dict, weights)), as.numeric(manual))
})

textdir = '../'
if(!file.exists(paste0(textdir, 'texts.txt'))) textdir = paste0('../../', textdir)
skip_if(!file.exists(paste0(textdir, 'texts.txt')), paste('texts.txt not found in', normalizePath(textdir)))

dicts = list.files('~/Dictionaries', '(?:csv|dic)$', full.names = TRUE)
skip_if(!length(dicts), paste('no .csv or .dic files in', path.expand('~/Dictionaries')))

test_that('applied dictionaries work', {
  text = sample(readLines(paste0(textdir, 'texts.txt')), 10)
  dtm = lma_dtm(text)
  expect_identical(as.numeric(lma_termcat(dtm, read.dic(dicts[1]))), as.numeric(lma_termcat(dtm, dicts[1])))
  expect_identical(as.numeric(lma_patcat(text, read.dic(dicts[1]))), as.numeric(lma_patcat(text, dicts[1])))
  for(d in dicts){
    dict = read.dic(d)
    opt = lma_termcat(dtm, dict)
    opp = lma_patcat(text, dict)
    expect_equal(dim(opt), dim(opp))
    expect_true(all(colnames(opt) %in% colnames(opp)))
    if(any(grepl(' ', if(is.null(dim(dict))) dict$term else unique(unlist(dict)), fixed = TRUE))){
      expect_equal(as.numeric(suppressWarnings(lma_termcat(text, dict))), as.numeric(opp))
    }
  }
})
