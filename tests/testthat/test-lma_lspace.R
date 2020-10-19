context('lma_lspace')

test_that('svd works', {
  dtm = matrix(rpois(100, 1), 10)
  colnames(dtm) = letters[1:10]
  lss = lma_lspace(dtm)
  expect_equal(
    lma_simets(lma_lspace(dtm, lma_lspace(dtm)), 'cos'),
    lma_simets(lma_lspace(dtm, keep.dim = TRUE), 'cos')
  )
  expect_equal(
    lma_simets(lma_lspace(dtm, lma_lspace(dtm, dim.cutoff = .1)), 'can'),
    lma_simets(lma_lspace(dtm, dim.cutoff = .1, keep.dim = TRUE), 'can')
  )
})

dir = getOption('lingmatch.lspace.dir')
map = paste0(dir, '/lma_term_map.rda')
files = list.files(dir, '\\.dat$', full.names = TRUE)
skip_if(
  is.null(dir) || !dir.exists(dir) || !file.exists(map) ||
  !length(files), 'embeddings files not downloaded'
)

spaces = select.lsspace()
names = gsub('^.+/|\\.dat$', '', files)
name = names[which(names %in% rownames(spaces$info))[1]]

skip_if(is.na(name), 'no recognized .dat file')

terms = names(which(spaces$term_map[, name] != 0))[1:5000]
space = matrix(scan(
  paste0(dir, '/', name, '.dat'), nlines = 5000,
  quiet = TRUE, quote = '', comment.char = '', na.strings = ''
), 5000, spaces$info[name, 'dimensions'], byrow = TRUE, dimnames = list(terms))

test_that('random reads align', {
  sel = sample(terms, 1000)
  read_c = as.numeric(lma_lspace(sel, name))
  read_r = lma_lspace(sel, name, use.scan = TRUE)
  expect_equal(read_c, as.numeric(read_r))
  expect_equal(read_c, as.numeric(space[sel,]))
})

texts = vapply(seq_len(50), function(d) paste0(c(sample(c('aaaa', 'bbbb'), 1), sample(
  terms, sample(100, 1), TRUE, prob = sort(rbeta(length(terms), .01, 1), TRUE)
)), collapse = ' '), '')
dtm = lma_dtm(texts)

test_that('mapping works', {
  overlap = colnames(dtm)[colnames(dtm) %in% terms]
  mapped = lma_lspace(dtm, name)
  expect_equal(as.numeric(mapped), as.numeric(dtm[, overlap] %*% space[overlap,]))
  mapped_sim = as.numeric(lma_simets(mapped, 'cosine'))
  expect_equal(mapped_sim, as.numeric(lingmatch(dtm, space = name)$sim))
  expect_equal(mapped_sim, as.numeric(lingmatch(texts, space = space)$sim))
  expect_equal(mapped_sim, as.numeric(lingmatch(dtm, space = space)$sim))
  expect_equal(mapped_sim, as.numeric(lingmatch(mapped, space = space)$sim))
})

test_that('fill.missing works', {
  terms = sample(c(rownames(spaces$term_map)[1:100], rep('xxxx', 100)), 100)
  compact = lma_lspace(terms, name)
  filled = lma_lspace(terms, name, fill.missing = TRUE)
  filled_map = lma_lspace(terms, name, term.map = spaces$term_map, fill.missing = TRUE)
  filled_scan = lma_lspace(terms, name, fill.missing = TRUE, use.scan = TRUE)
  expect_equal(as.numeric(compact), as.numeric(filled[rownames(compact),]))
  expect_equal(filled, filled_map)
  expect_equal(filled, filled_scan)
})
