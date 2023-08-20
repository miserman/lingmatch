options(encoding = "latin1", stringsAsFactors = FALSE)
texts <- c(
  "And there with it isn't I think anyone would.",
  "Command lands of a few I two of it is."
)

test_that("lma_patcat variants works", {
  opts <- expand.grid(exclusive = c(TRUE, FALSE), boundary = c(TRUE, FALSE))
  opts_head <- list(text = texts, dict = list(c("and", "it", "i")))
  expected <- list(c(3, 2), c(3, 2), c(6, 5), c(8, 6))
  exp_mats <- list(c(1, 0, rep(1, 4)), c(1, 0, rep(1, 4)), c(1, 2, 2, 1, 3, 2), c(1, 2, 2, 1, 5, 3))

  for (i in seq_len(nrow(opts))) {
    expect_equal(do.call(lma_patcat, c(opts_head, as.list(opts[i, ])))[, 1], expected[[i]])
    expect_equal(do.call(lma_patcat, c(opts_head, as.list(opts[i, ]), fixed = FALSE))[, 1], expected[[i]])
    expect_equal(as.numeric(do.call(lma_patcat, c(opts_head, as.list(opts[i, ]), return.dtm = TRUE))), exp_mats[[i]])
    expect_equal(as.numeric(do.call(lma_patcat, c(opts_head, as.list(opts[i, ]), fixed = FALSE, return.dtm = TRUE))), exp_mats[[i]])
  }
})

test_that("lma_patcat globtoregex works", {
  text <- "fishes befish the unbefished"
  dict <- list(prefish = "*fish", postfish = "fish*", barefish = "fish")
  expect_equal(as.numeric(lma_patcat(text, dict, globtoregex = FALSE)), c(0, 0, 3))
  expect_equal(as.numeric(lma_patcat(text, dict, globtoregex = TRUE)), c(3, 0, 0))
  expect_equal(as.numeric(lma_patcat(text, dict, globtoregex = TRUE, exclusive = FALSE)), c(3, 3, 3))
})

test_that("invalid regex are escaped", {
  text <- ":\\ :>\\ :] :-] :->( :("
  dict <- list(d = ":*\\", i = ":*]", f = ":*(")
  expect_true(all(lma_patcat(text, dict) == 0))
  expect_true(all(lma_patcat(text, dict, globtoregex = TRUE) == 2))
  expect_error(lma_patcat(text, "?", fixed = FALSE), "terms contain invalid", fixed = TRUE)
})

test_that("lma_patcat parts work", {
  dict <- list(
    a = c("an" = .1, "i t" = .2, "'t" = .4),
    b = c("and" = .4, "a few" = .2, "two" = .1)
  )
  bias <- c(a = -10, b = 10)
  lex <- data.frame(
    term = c("_intercept", names(dict$a), "_intercept", names(dict$b)),
    category = rep(c("a", "b"), each = 4),
    weight = c(-10, 10, unlist(dict))[c(1, 3:5, 2, 6:8)]
  )

  manual_dtm <- Matrix(c(0, 1, 1, 1, 1, 2, 0, 0, 1, 0, 1, 0), 2, sparse = TRUE)
  base_dtm <- lma_patcat(texts, lapply(dict, names), return.dtm = TRUE)
  expect_true(all(base_dtm == manual_dtm))
  expect_true(all(base_dtm == lma_patcat(texts, unlist(lapply(dict, names)), return.dtm = TRUE)))

  manual_cat <- manual_dtm %*% Matrix(c(0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0), 6)
  base_cat <- lma_patcat(texts, lapply(dict, names))[, names(dict)]
  expect_true(all(base_cat == manual_cat))
  expect_true(all(base_cat == lma_patcat(texts, unlist(lapply(dict, names)),
    pattern.categories = rep(names(dict), vapply(dict, length, 0))
  )[, names(dict)]))

  weights <- unlist(dict, use.names = FALSE)[c(5, 2, 4, 6, 1, 3)]
  weighted_cat <- lma_patcat(texts, dict)[, names(dict)]
  expect_true(all(weighted_cat == manual_dtm %*% Matrix(weights * c(0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0), 6)))
  expect_true(all(weighted_cat == lma_patcat(
    texts, unlist(lapply(dict, names)),
    unlist(dict), rep(names(dict), vapply(dict, length, 0))
  )[, names(dict)]))
  expect_true(all(weighted_cat == lma_patcat(
    texts,
    data.frame(do.call(c, unname(dict)), rep(c("a", "b"), each = 3))
  )[, names(dict)]))

  full <- lma_patcat(texts, lex)[, names(dict)]
  expect_true(all(full == weighted_cat + rep(bias, each = 2)))
  expect_true(all(full == lma_patcat(texts, dict, bias = bias)[, names(dict)]))
  expect_true(all(full == lma_patcat(texts, dict, bias = c(10, -10))[, names(dict)]))
  expect_true(all(full == lma_patcat(texts, dict, bias = c(bias, g = 100))[, names(dict)]))

  expect_true(all(lma_patcat(texts, data.frame(
    term = lex$term[-5],
    a = c(lex$weight[1:4], numeric(3)),
    b = c(10, numeric(3), lex$weight[6:8])
  )) == full))

  colnames(lex)[1] <- "terms"
  expect_identical(as.numeric(full), as.numeric(lma_patcat(texts, lex)[, names(dict)]))
})

test_that("lma_patcat wide dict format works", {
  dict <- data.frame(term = c("a", "b", "c"), w1 = c(1, 2, 3), w2 = c(.1, .2, .3))
  text <- vapply(1:5, function(i) paste(rep("a b c", i), collapse = " "), "")
  dtm <- lma_dtm(text)
  manual <- cbind(dtm %*% dict$w1, dtm %*% dict$w2)
  expect_true(all(lma_patcat(text, dict) == manual))
  expect_true(all(lma_patcat(text, unname(dict)) == manual))
  expect_true(all(lma_patcat(text, dict, c("w1", "w2")) == manual))
  expect_true(all(lma_patcat(text, dict, pattern.categories = c("w1", "w2")) == manual))
  expect_true(all(lma_patcat(text, dict, dict) == manual))
  expect_true(all(lma_patcat(text, dict, pattern.categories = dict) == manual))
  expect_true(all(lma_patcat(text, list(dict$term, dict$term), dict) == manual))
  expect_true(all(lma_patcat(text, pattern.weights = structure(dict, row.names = dict$term)) == manual))
  expect_true(all(lma_patcat(text, dict$term, dict) == manual))
  expect_true(all(lma_patcat(text, dict$term, pattern.categories = dict) == manual))
  expect_identical(colnames(lma_patcat("", dict$term, pattern.categories = dict)), colnames(dict)[-1])
  expect_true(ncol(lma_patcat("", dict$term, pattern.categories = dict, drop.zeros = TRUE)) == 0)
  expect_identical(as.numeric(lma_patcat(text, dict, return.dtm = TRUE)), as.numeric(rep(1:5, 3)))
})

skip_if_not(file.exists("~/Dictionaries/adicat_function.dic"), "test dictionary is not present")

test_that("lma_patcat named dictionary works", {
  expect_identical(
    as.numeric(lma_patcat(texts, "adicat_function", dir = "")),
    as.numeric(lma_patcat(texts, lma_dict(as.regex = FALSE)))
  )
})
