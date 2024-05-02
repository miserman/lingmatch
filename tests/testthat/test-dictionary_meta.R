dir <- path.expand("~/Latent Semantic Spaces")
map <- paste0(dir, "/lma_term_map.rda")
files <- list.files(dir, "\\.dat$", full.names = TRUE)
skip_if(
  is.null(dir) || !dir.exists(dir) || !file.exists(map) ||
    !length(files), "embeddings files not downloaded"
)

dict <- list(
  animal = c("ant", "frog", "aardvark*"),
  machine = c("car", "turbine", "housing*")
)

test_that("dictionary meta works", {
  meta <- dictionary_meta(dict, suggest = TRUE)
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
  expect_false(is.null(meta$suggested))

  space <- lma_lspace("100k")
  meta <- dictionary_meta(dict, space, suggest = TRUE, dimension_prop = .5)
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
  expect_false(is.null(meta$suggested))
})

test_that("multiple spaces work", {
  meta <- dictionary_meta(
    dict, "multi",
    n_spaces = 2, suggest = TRUE,
    dimension_prop = .5, pairwise = FALSE
  )
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
  expect_false(is.null(meta$suggested))

  meta <- dictionary_meta(dict, "multi", n_spaces = 2)
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
})
