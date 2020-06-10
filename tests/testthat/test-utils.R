context('utils')

texts = c(
  "And there with it isn't I think anyone would.",
  "Command lands of a few I two of it is."
)

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
  base_cat = lma_patcat(texts, lapply(dict, names))
  expect_true(all(base_cat == manual_cat))
  expect_true(all(base_cat == lma_patcat(texts, unlist(lapply(dict, names)),
    pattern.categories = rep(names(dict), vapply(dict, length, 0)))))

  weighted_cat = lma_patcat(texts, dict)
  expect_true(all(weighted_cat == manual_dtm %*% Matrix(weights * c(0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0), 6)))
  expect_true(all(weighted_cat == lma_patcat(texts, unlist(lapply(dict, names)),
    unlist(dict), rep(names(dict), vapply(dict, length, 0)))))

  full = lma_patcat(texts, lex)
  expect_true(all(full == weighted_cat + rep(bias, each = 2)))
  expect_true(all(full == lma_patcat(texts, dict, bias = bias)))
})

test_that('read/write.dic works', {
  dict = list(
    full = letters,
    sub = letters[1:10],
    partial = paste0(letters[11:21], '*'),
    faces = c(': )', ':(', ':]', ': [', ': }', ':{')
  )
  dict_lines = strsplit(write.dic(dict, save = FALSE), '\n', fixed = TRUE)[[1]]
  expect_equal(read.dic(dict_lines), dict)
  expect_true(all(vapply(read.dic(dict_lines, to.regex = TRUE), function(cat){
    sum(grepl(paste(cat, collapse = '|'), unlist(dict))) >= length(cat)
  }, TRUE)))
})

test_that('read.folder works', {
  dir = path.package('lingmatch')
  files = grep('[DNRAE]{2}', list.files(dir, full.names = TRUE), value = TRUE)

  segs = read.folder(files, ext = '')
  wc = sum(segs$WC)
  expect_equal(unique(segs$file), files)

  all_segs = read.folder(dir, ext = '')
  expect_equivalent(segs, all_segs[all_segs$file %in% files,])

  manual = vapply(files, function(f)
    gsub('\\s{2,}', ' ', paste(readLines(f), collapse = ' ')), '')
  expect_true(all(tapply(segs$text, segs$file, paste, collapse = ' ') == manual))

  segs1 = read.folder(files, 1, ext = '')
  expect_true(all(table(segs1$file) == 1))
  expect_true(sum(segs1$WC) == wc)
  expect_true(all(segs1$text == manual))

  mtokens = strsplit(manual, ' ')
  expect_true(all(vapply(mtokens, length, 0) == segs1$WC))
  segs_tokens = read.folder(files, 1, ext = '', return.tokens = TRUE)
  expect_true(all(vapply(segs_tokens$indices, function(inds)
    paste(segs_tokens$tokens[inds], collapse = ' '), '') == manual))

  segs5 = read.folder(files, 5, ext = '')
  expect_true(all(table(segs5$file) == 5))
  expect_true(sum(segs5$WC) == wc)
  expect_true(all(tapply(segs5$text, segs5$file, paste, collapse = ' ') == manual))

  segs50w = read.folder(files, ext = '', segment.size = 50)
  expect_true(all(segs50w$WC <= 50))
  expect_true(sum(segs50w$WC) == wc)
  expect_true(all(tapply(segs50w$text, segs50w$file, paste, collapse = ' ') == manual))
})
