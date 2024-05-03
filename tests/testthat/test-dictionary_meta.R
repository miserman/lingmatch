dir <- path.expand("~/Latent Semantic Spaces")
map <- paste0(dir, "/lma_term_map.rda")
files <- list.files(dir, "\\.dat$", full.names = TRUE)
skip_if(
  is.null(dir) || !dir.exists(dir) || !file.exists(map) ||
    !length(files), "embeddings files not downloaded"
)

dict <- list(
  rare = "not in space",
  one_match = c("teacher", "not in space*", "another not in space"),
  animal = c("ant", "frog", "aardvark*"),
  machine = c("car", "turbine", "housing*")
)

test_that("dictionary meta works", {
  meta <- dictionary_meta(dict, suggest = TRUE)
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
  expect_false(is.null(meta$suggested))

  space <- lma_lspace("blogs")
  meta <- dictionary_meta(dict, space, suggest = TRUE, dimension_prop = .5)
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
  expect_false(is.null(meta$suggested))

  meta <- dictionary_meta("student", space, suggest = TRUE)
  expect_identical(meta$summary$category, "cat1")
  expect_identical(meta$summary$n_terms, 1)
  expect_false(is.null(meta$suggested$cat1))

  expect_error(dictionary_meta("not in space", space, suggest = TRUE))
})

test_that("multiple spaces work", {
  meta <- dictionary_meta(
    dict, c("blogs", "eigenwords_tscca"),
    n_spaces = 2, suggest = TRUE
  )
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
  expect_false(is.null(meta$suggested))

  meta <- dictionary_meta(
    dict, "multi",
    dimension_prop = .2, expand_cutoff_freq = .5, pairwise = FALSE
  )
  expect_identical(meta$summary$category, names(dict))
  expect_identical(meta$summary$n_terms, vapply(unname(dict), length, 0))
})
