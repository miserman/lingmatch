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

test_that("space works", {
  report <- report_term_matches(dict, text, space = TRUE)
  expect_identical(report$space, rep("100k", nrow(report)))
})
