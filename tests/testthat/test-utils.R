context('utils')

texts = c(
  "And there with it isn't I think anyone would.",
  "Command lands of a few I two of it is."
)
file = paste0(tempfile(), '.txt')

test_that('lma_process works', {
  dtm = as.data.frame(lma_dtm(texts, sparse = FALSE))
  meta = lma_meta(texts)
  colnames(meta) = paste0('meta_', colnames(meta))
  manual = cbind(text = texts, dtm, meta)
  expect_equal(lma_process(texts), manual)
  write(texts, file)
  expect_equal(lma_process(file), manual)
  file.remove(file)
  manual[, colnames(dtm)] = lma_weight(dtm, 'tfidf', normalize = FALSE)
  expect_equal(lma_process(texts, weight = 'tfidf', normalize = FALSE), manual)
  termcat = as.data.frame(lma_termcat(lma_weight(dtm)))
  expect_equal(lma_process(texts, weight = 'count', dict = lma_dict(1:9), meta = FALSE)[, -1], termcat)
  expect_equal(lma_process(dtm, weight = 'count', dict = lma_dict(1:9), meta = FALSE), termcat)
})

test_that('read/write.dic works', {
  dict = list(
    full = letters,
    sub = letters[1:10],
    partial = paste0(letters[11:21], '*'),
    faces = c(': )', ':(', ':]', ': [', ': }', ':{')
  )
  write.dic(dict, file)
  expect_equal(read.dic(file), dict)
  expect_true(all(vapply(read.dic(file, to.regex = TRUE), function(cat){
    sum(grepl(paste(cat, collapse = '|'), unlist(dict))) >= length(cat)
  }, TRUE)))
  file.remove(file)
})

test_that('lma_patcat variants works', {
  opts = expand.grid(exclusive = c(TRUE, FALSE), boundary = c(TRUE, FALSE))
  opts_head = list(text = texts, dict = c('and', 'it', 'i'))
  expected = list(c(3, 2), c(3, 2), c(6, 5), c(8, 6))
  exp_mats = list(c(1, 0, rep(1, 4)), c(1, 0, rep(1, 4)), c(1, 2, 2, 1, 3, 2), c(1, 2, 2, 1, 5, 3))

  for(i in seq_len(nrow(opts))){
    expect_equal(do.call(lma_patcat, c(opts_head, as.list(opts[i,])))[, 1], expected[[i]])
    expect_equal(do.call(lma_patcat, c(opts_head, as.list(opts[i,]), fixed = FALSE))[, 1], expected[[i]])
    expect_equal(as.numeric(do.call(lma_patcat, c(opts_head, as.list(opts[i,]), return.dtm = TRUE))), exp_mats[[i]])
    expect_equal(as.numeric(do.call(lma_patcat, c(opts_head, as.list(opts[i,]), fixed = FALSE, return.dtm = TRUE))), exp_mats[[i]])
  }
})

test_that('lma_patcat globtoregex works', {
  text = 'fishes befish the unbefished'
  dict = list(prefish = '*fish', postfish = 'fish*', barefish = 'fish')
  expect_equal(as.numeric(lma_patcat(text, dict)), c(0, 0, 3))
  expect_equal(as.numeric(lma_patcat(text, dict, globtoregex = TRUE)), c(3, 0, 0))
  expect_equal(as.numeric(lma_patcat(text, dict, globtoregex = TRUE, exclusive = FALSE)), c(3, 3, 3))
})

test_that('lma_patcat parts work', {
  dict = list(
    a = c('an' = .1, 'i t' = .2, "'t" = .4),
    b = c('and' = .4, 'a few' = .2, 'two' = .1)
  )
  bias = c(a = -10, b = 10)
  lex = data.frame(
    term = c('_intercept', names(dict$a), '_intercept', names(dict$b)),
    category = rep(c('a', 'b'), each = 4),
    weight = c(-10, 10, unlist(dict))[c(1, 3:5, 2, 6:8)]
  )

  manual_dtm = Matrix(c(0, 1, 1, 1, 1, 2, 0, 0, 1, 0, 1, 0), 2, sparse = TRUE)
  base_dtm = lma_patcat(texts, lapply(dict, names), return.dtm = TRUE)
  expect_true(all(base_dtm == manual_dtm))
  expect_true(all(base_dtm == lma_patcat(texts, unlist(lapply(dict, names)), return.dtm = TRUE)))

  weights = unlist(dict, use.names = FALSE)[c(5, 2, 4, 6, 1, 3)]
  weighted_dtm = lma_patcat(texts, dict, return.dtm = TRUE)
  expect_true(all(weighted_dtm == manual_dtm * rep(weights, each = 2)))
  expect_true(all(weighted_dtm == lma_patcat(texts, unlist(lapply(dict, names)), unlist(dict), return.dtm = TRUE)))

  manual_cat = manual_dtm %*% Matrix(c(0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0), 6)
  base_cat = lma_patcat(texts, lapply(dict, names))[, names(dict)]
  expect_true(all(base_cat == manual_cat))
  expect_true(all(base_cat == lma_patcat(texts, unlist(lapply(dict, names)),
    pattern.categories = rep(names(dict), vapply(dict, length, 0)))[, names(dict)]))

  weighted_cat = lma_patcat(texts, dict)[, names(dict)]
  expect_true(all(weighted_cat == manual_dtm %*% Matrix(weights * c(0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0), 6)))
  expect_true(all(weighted_cat == lma_patcat(texts, unlist(lapply(dict, names)),
    unlist(dict), rep(names(dict), vapply(dict, length, 0)))[, names(dict)]))

  full = lma_patcat(texts, lex)[, names(dict)]
  expect_true(all(full == weighted_cat + rep(bias, each = 2)))
  expect_true(all(full == lma_patcat(texts, dict, bias = bias)[, names(dict)]))
})

test_that('lma_patcat wide dict format works', {
  dict = data.frame(term = c('a', 'b', 'c'), w1 = c(1, 2, 3), w2 = c(.1, .2, .3))
  text = vapply(1:5, function(i) paste(rep('a b c', i), collapse = ' '), '')
  dtm = lma_dtm(text)
  manual = cbind(dtm %*% dict$w1, dtm %*% dict$w2)
  expect_true(all(lma_patcat(text, dict) == manual))
  expect_true(all(lma_patcat(text, dict, c('w1', 'w2')) == manual))
  expect_true(all(lma_patcat(text, dict, pattern.categories = c('w1', 'w2')) == manual))
  expect_true(all(lma_patcat(text, dict, dict) == manual))
  expect_true(all(lma_patcat(text, dict, pattern.categories = dict) == manual))
})

test_that('read.segments works', {
  dir = path.package('lingmatch')
  files = grep('[DNRAE]{2}', list.files(dir, full.names = TRUE), value = TRUE)

  segs = read.segments(files, ext = '')
  wc = sum(segs$WC)
  expect_equal(unique(segs$input), files)

  all_segs = read.segments(dir, ext = '')
  expect_equivalent(segs, all_segs[all_segs$input %in% files,])

  manual = vapply(files, function(f)
    gsub('\\s{2,}', ' ', paste(readLines(f), collapse = ' ')), '')
  expect_true(all(tapply(segs$text, segs$input, paste, collapse = ' ') == manual))

  segs1 = read.segments(files, 1, ext = '')
  expect_equal(segs1[, -1], read.segments(manual, 1)[, -1])
  expect_true(all(table(segs1$input) == 1))
  expect_true(sum(segs1$WC) == wc)
  expect_true(all(segs1$text == manual))

  segs5 = read.segments(files, 5, ext = '')
  expect_equal(segs5[, -1], read.segments(manual, 5)[, -1])
  expect_true(all(table(segs5$input) == 5))
  expect_true(sum(segs5$WC) == wc)
  expect_true(all(tapply(segs5$text, segs5$input, paste, collapse = ' ') == manual))

  segs50w = read.segments(files, ext = '', segment.size = 50)
  expect_equal(segs50w[, -1], read.segments(manual, segment.size = 50)[, -1])
  expect_true(all(segs50w$WC <= 50))
  expect_true(sum(segs50w$WC) == wc)
  expect_true(all(tapply(segs50w$text, segs50w$input, paste, collapse = ' ') == manual))
})
