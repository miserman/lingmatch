context("lma_dtm")

test_that("exclude works", {
  text <- "That would be what of the and word of the place name."
  dtm <- lma_dtm(text, "function")
  expect_equal(dtm[1, ], c(name = 1, place = 1, word = 1))
})

test_that("context works", {
  text <- c("i like", "you like", "it like", "like that")
  expect_equal(lma_dtm(text, context = c("(i) like", "(you) like"))[, "like"], c(0, 0, 1, 1))
  expect_equal(lma_dtm(text, context = lma_dict("special")$special$LIKE)[, "like"], c(0, 0, 1, 1))
})

test_that("numbers works", {
  text <- "word 21 word .231 word 23.302%, word word 1e54 word 1e-5."
  expect_equal(colnames(lma_dtm(text)), "word")
  expect_equal(ncol(lma_dtm(text, numbers = TRUE)), 6)
})

test_that("punct works", {
  text <- "word, word word. Word's word-word 'word' word? Word word: word; word!"
  expect_equal(colnames(lma_dtm(text)), sort(c("word", "word's", "word-word")))
  expect_equal(
    colnames(lma_dtm(text, punct = TRUE)),
    sort(c("!", '"', ",", ".", ":", ";", "?", "word", "word's", "word-word"))
  )
})

test_that("urls works", {
  text <- "click here: https://www.site.com/page, or try www.that.net?q=term or even dhji.gov#tag"
  expect_true(all(c("https://www.site.com/page", "www.that.net?q=term", "dhji.gov#tag") %in%
    colnames(lma_dtm(text, punct = TRUE))))
  expect_true(all(c("www.site.com", "www.that.net", "dhji.gov") %in% colnames(lma_dtm(text))))
  expect_true(lma_dtm(text, urls = FALSE)[, "repurl"] == 3)
})

test_that("emojis works", {
  text <- ":) word! ( : word: :( ... :...( > D: + :>( word :p ^_^ word :[ word [-:"
  expect_equal(colnames(lma_dtm(text)), c("d", "p", "word"))
  expect_equal(colnames(lma_dtm(text, emojis = TRUE)), c("repfrown", "repsmile", "word"))
  expect_true(all(lma_dtm(text, emojis = TRUE) == 5))
})

test_that("tokens.only lines up", {
  words <- vapply(seq_len(200), function(w) {
    paste0(sample(letters, sample(9, 1)), collapse = "")
  }, "")
  texts <- c("a.a", vapply(seq_len(13), function(d) {
    paste0(sample(words, sample(100, 1), TRUE), collapse = " ")
  }, ""), "c.c")
  dtm <- lma_dtm(texts)
  tokens <- lma_dtm(texts, tokens.only = TRUE)
  expect_equal(texts, vapply(tokens$indices, function(inds) {
    paste(names(tokens$tokens)[inds], collapse = " ")
  }, ""))
  expect_equal(as.numeric(lma_dtm(tokens)), as.numeric(dtm))
  ex <- c(lma_dict(), unique = c("a.a", "c.c"))
  dtm <- lma_dtm(texts, ex, dc.min = 2, dc.max = 5)
  tokens <- lma_dtm(texts, ex, dc.min = 2, dc.max = 5, tokens.only = TRUE)
  expect_equal(as.numeric(lma_dtm(tokens)), as.numeric(dtm))
})

test_that("alternate token list input works", {
  dtm <- matrix(
    c(1, 0, 1, 1, 0, 0, 0, 0, 1), 3,
    dimnames = list(c("a", "c", "f"), c("w1", "w2", "w3"))
  )
  expect_identical(lma_dtm(list(
    a = c(w1 = 1, w2 = 1), c = numeric(), f = c(w1 = 1, w3 = 1)
  ), sparse = FALSE), dtm)
  expect_equal(as.matrix(lma_dtm(list(
    a = "w1 w2", c = "", f = "w1 w3"
  ), numbers = TRUE)), dtm)
})
