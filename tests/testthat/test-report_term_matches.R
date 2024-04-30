text <- c(
  "I am sadly homeless, and suffering from depression :(",
  "This wholesome happiness brings joy to my heart! :D:D:D",
  "They are joyous in these fearsome happenings D:",
  "I feel weightless now that my sadness has been depressed! :()"
)
dict <- list(
  sad = c("*less", "sad*", "depres*", ":("),
  happy = c("*some", "happ*", "joy*", "d:"),
  i = c("i *", "my *"),
  nomatch = "fefef"
)
report <- NULL

test_that("base example works", {
  report <<- report_term_matches(dict, text, as_string = FALSE)
  expect_identical(report$max_count, rep(c(1L, 2L, 0L), c(8, 1, 2)))
})

test_that("regex terms work", {
  expect_identical(report_term_matches("ha[^i]+in.*?", text)$matches, "happiness (1), happenings (1)")
})

test_that("mixed terms work", {
  cols <- c("count", "max_count", "variants")
  expect_identical(
    report_term_matches(c("ha[^i]+in.*?", "depress*"), text)[, cols],
    as.data.frame(matrix(c(2L, 2L, 1L, 1L, 2L, 2L), 2, dimnames = list(NULL, cols)))
  )
})

test_that("text as terms works", {
  expect_identical(as.numeric(report_term_matches(
    dict, c("hopeless", "helpless", "fearless"),
    as_terms = TRUE
  )[1, c("count", "max_count", "variants")]), c(3, 1, 3))
})

test_that("writing results works", {
  file <- tempfile(fileext = ".csv")
  report_term_matches(dict, text, outFile = file)
  expect_identical(report[, 1:6], read.csv(file)[, 1:6])
})

dir <- path.expand("~/Latent Semantic Spaces")
map <- paste0(dir, "/lma_term_map.rda")
files <- list.files(dir, "\\.dat$", full.names = TRUE)
skip_if(
  !dir.exists(dir) || !file.exists(map) || !length(files),
  "embeddings files not downloaded"
)

found <- unlist(lapply(report$matches, colnames))
spaces <- select.lspace(terms = found)
space <- rownames(spaces$selected)[1]

test_that("space works", {
  report <- report_term_matches(dict, text, space = TRUE)
  expect_true(all(c("space", "mean_sim", "min_sim") %in% colnames(report)))
  expect_identical(report$space[[1]], space)
  expect_true(grepl("\\w \\(1, 1\\)", report$matches[[1]]))
})

test_that("space preload works", {
  report <- report_term_matches(dict, space = space)
  expect_true(all(c("space", "mean_sim", "min_sim") %in% colnames(report)))
  expect_identical(report$space[[1]], space)
  expect_true(grepl("\\w \\(1\\)", report$matches[[1]]))
})
