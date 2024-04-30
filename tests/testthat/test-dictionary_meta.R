dir <- path.expand("~/Latent Semantic Spaces")
map <- paste0(dir, "/lma_term_map.rda")
files <- list.files(dir, "\\.dat$", full.names = TRUE)
skip_if(
  is.null(dir) || !dir.exists(dir) || !file.exists(map) ||
    !length(files), "embeddings files not downloaded"
)

dict <- list(
  furniture = c("table", "chair", "desk*", "couch*", "sofa*"),
  well_adjusted = c("happy", "bright*", "friend*", "she", "he", "they")
)

test_that("dicitonary meta works", {
  RcppParallel::setThreadOptions(1)
  meta <- dictionary_meta(dict, suggest = TRUE)
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
  expect_false(is.null(meta$suggested))
})
