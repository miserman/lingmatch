context('utils')

texts = c(
  "And there with it isn't I think anyone would.",
  "Command lands of a few I two of it is."
)

test_that('lma_process works', {
  files = c(tempfile(fileext = '.txt'), tempfile(fileext = '.txt'))
  dtm = as.data.frame(lma_dtm(texts, sparse = FALSE))
  meta = lma_meta(texts)
  colnames(meta) = paste0('meta_', colnames(meta))
  manual = cbind(text = texts, dtm, meta)
  expect_equal(lma_process(texts), manual)
  pr = lma_process(lma_patcat(
    texts, dict = lma_dict(as.regex = FALSE), return.dtm = TRUE, fixed = FALSE, globtoregex = TRUE
  ))
  expect_equal(pr, lma_process(texts, dict = lma_dict(as.regex = FALSE), return.dtm = TRUE, fixed = FALSE,
    globtoregex = TRUE)[, names(pr)])
  write(texts[1], files[1])
  write(texts[2], files[2])
  expect_equal(lma_process(files)[, -(1:3)], manual)
  expect_equal(lma_process(read.segments(files))[, -(1:3)], manual)
  file.remove(files)
  manual[, colnames(dtm)] = lma_weight(dtm, 'tfidf', normalize = FALSE)
  expect_equal(lma_process(texts, weight = 'tfidf', normalize = FALSE), manual)
  wmeta = meta
  wmeta[, c(9, 14:23)] = wmeta[, c(9, 14:23)] / wmeta$meta_words
  expect_equal(lma_process(texts, weight = 'count')[, colnames(meta)], wmeta)
  termcat = as.data.frame(lma_termcat(lma_weight(dtm)))
  expect_equal(lma_process(texts, weight = 'count', dict = lma_dict(1:9), meta = FALSE)[, -1], termcat)
  expect_equal(lma_process(dtm, weight = 'count', dict = lma_dict(1:9), meta = FALSE), termcat)
  expect_equal(as.numeric(lma_process(dtm, dim.cutoff = .1)[, 1]), as.numeric(lma_lspace(dtm, dim.cutoff = .1)))
})

test_that('lma_dict works', {
  expect_equal(names(lma_dict(c('ppron', 'adv'))), c('ppron', 'adverb'))
  expect_equal(lma_dict(as.function = TRUE)(c('fefe', 'and', 'sksk')), c(FALSE, TRUE, FALSE))
  expect_equal('wdk kdls loe (cc)', lma_dict(special, as.function = gsub)('wdk \u0137dls lo\u00cb \u00A9'))
})

test_that('read/write.dic works', {
  file = tempfile(fileext = '.txt')
  dict = list(
    full = letters,
    sub = letters[1:10],
    partial = paste0(letters[11:21], '*'),
    faces = c(': )', ':(', ':]', ': [', ': }', ':{')
  )
  write.dic(dict, file)
  expect_equal(read.dic(file), dict)
  expect_true(all(vapply(read.dic(file, type = 'term'), function(cat){
    sum(grepl(paste(cat, collapse = '|'), unlist(dict))) >= length(cat)
  }, TRUE)))
  dict_weighted = read.dic(dict, as.weighted = TRUE)
  expect_equal(dim(dict_weighted), c(length(unique(unlist(dict))), length(dict) + 1))
  expect_equal(write.dic(dict, file, as.weighted = TRUE), dict_weighted)
  expect_equal(read.dic(file, as.weighted = FALSE), dict)
  expect_equal(read.dic(file), dict_weighted)
  file.remove(file)
  dict = list(
    positive = c('good', 'great'),
    neutral = c('what', 'hey'),
    negative = c('bad', 'horrible')
  )
  expect_equal(dict, read.dic(data.frame(unlist(dict, use.names = FALSE),
    c(1, 1.5, 0, 0, -1, -1.5))))
  expect_equal(dict[c(1, 3)], read.dic(data.frame(unlist(dict[c(1, 3)], use.names = FALSE),
    c(1, 1.5, -1, -1.5))))
  expect_equal(read.dic(data.frame(
    term = unlist(dict, use.names = FALSE), sentiment = rep(c(1, 0, -1), each = 2)
  )), dict)
  expect_equal(read.dic(data.frame(
    term = unlist(dict, use.names = FALSE), category = rep(names(dict), each = 2)
  ))[names(dict)], dict)
  dicts = select.dict()$info
  if(!is.na(d <- which(dicts$downloaded != '')[1])) expect_equal(
    read.dic(rownames(dicts)[d]), read.dic(dicts[d, 'downloaded'])
  )
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
  expect_true(all(lma_patcat(text, unname(dict)) == manual))
  expect_true(all(lma_patcat(text, dict, c('w1', 'w2')) == manual))
  expect_true(all(lma_patcat(text, dict, pattern.categories = c('w1', 'w2')) == manual))
  expect_true(all(lma_patcat(text, dict, dict) == manual))
  expect_true(all(lma_patcat(text, dict, pattern.categories = dict) == manual))
  expect_true(all(lma_patcat(text, dict$term, dict) == manual))
  expect_true(all(lma_patcat(text, dict$term, pattern.categories = dict) == manual))
})

test_that('read.segments works', {
  dir = path.package('lingmatch')
  files = grep('[DNRAE]{2}', list.files(dir, full.names = TRUE), value = TRUE)

  expect_equal(read.segments(texts, segment.size = 5, bysentence = TRUE)$WC, c(9, 10))

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

test_that('select dict and lsspace work', {
  expect_equal(nrow(select.dict(c('inq', 'sent'))$selected), 5)
  expect_equal(nrow(select.lsspace(c('goo'))$selected), 1)
  expect_equal(nrow(select.lsspace(c('goo', 'glove'), dir = '', get.map = FALSE)$selected), 4)
  skip_if_not(file.exists(paste0(getOption('lingmatch.lspace.dir'), '/lma_term_map.rda')), 'term map not present')
  expect_equal(select.lsspace(c('cenepa', "didn't", 'pansear', 'xenops'))$selected$coverage, c(1, 1, .75, .5, .5))
})

test_that('standardize.lsspace works', {
  dir = getOption('lingmatch.lspace.dir')
  f = paste0(dir, '/stdtest.', c('txt', 'rda'))
  skip_if_not(all(file.exists(f)), 'raw embeddings test files not present')
  standardize.lsspace('stdtest.txt', 'stdtest')
  o = read.table(f[1], sep = ' ', quote = '', row.names = 1)
  o = as.matrix(o[!grepl('[^a-z]', rownames(o)),])
  expect_equal(o, matrix(scan(paste0(dir, '/stdtest.dat'), quiet = TRUE),
    nrow(o), 300, TRUE, dimnames = list(readLines(paste0(dir, '/stdtest_terms.txt')), colnames(o))))
  standardize.lsspace('stdtest.rda', 'stdtest')
  expect_equal(o, matrix(scan(paste0(dir, '/stdtest.dat'), quiet = TRUE),
    nrow(o), 300, TRUE, dimnames = list(readLines(paste0(dir, '/stdtest_terms.txt')), colnames(o))))
})

dir = '~/../Downloads/'
skip_if(TRUE || !dir.exists(dir), 'not downloading dictionary or embeddings files')

test_that('select.lsspace can download term_map', {
  skip_if(file.exists(paste0(dir, 'lma_term_map.rda')), 'term map already downloaded')
  expect_true('term_map' %in% names(select.lsspace(get.map = TRUE, dir = dir)))
})

test_that('download.dict works', {
  skip_if(file.exists(paste0(dir, 'lusi.dic')), 'dictionary already downloaded')
  download.dict('lusi', dir = dir)
  expect_true(file.exists(paste0(dir, 'lusi.dic')))
})

test_that('download.lsspace works', {
  skip_if(file.exists(paste0(dir, 'senna.dat')), 'embeddings set already downloaded')
  download.lsspace('senna', dir = dir)
  expect_true(all(file.exists(paste0(dir, 'senna', c('.dat', '_terms.txt')))))
})
