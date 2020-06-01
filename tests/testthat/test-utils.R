context('utils')

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
