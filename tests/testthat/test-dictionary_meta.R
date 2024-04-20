dir <- path.expand("~/Latent Semantic Spaces")
map <- paste0(dir, "/lma_term_map.rda")
files <- list.files(dir, "\\.dat$", full.names = TRUE)
skip_if(
  is.null(dir) || !dir.exists(dir) || !file.exists(map) ||
    !length(files), "embeddings files not downloaded"
)

text <- c(
  "I am sadly homeless, and suffering from depression :(",
  "This wholesome happiness brings joy to my heart! :D:D:D",
  "They are joyous in these fearsome happenings D:",
  "I feel weightless now that my sadness has been depressed! :()"
)
dict <- list(
  sad = c("*less", "sad*", "depres*", ":("),
  happy = c("*some", "happ*", "joy*", "d:"),
  self = c("i *", "my *")
)

test_that("term match report works", {
  report <- report_term_matches(dict)
  expect_true(any(report$count > 100))
  expect_identical(report$count, report$variants)
  expect_true(all(report$max_count < 2))

  report <- report_term_matches(dict, text, space = TRUE)
  expect_identical(report$space, rep("100k", nrow(report)))
})

test_that("dicitonary meta works", {
  meta <- dictionary_meta(dict)
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
})
