#' Assess Dictionary Categories Within a Latent Semantic Space
#'
#'
#' @param dict A vector of terms, list of such vectors, or a matrix-like object to be
#' categorized by \code{\link{read.dic}}.
#' @param space A vector space used to calculate similarities between terms.
#' Name of a the space (see \code{\link{select.lspace}}), a matrix with terms as row names, or
#' \code{"auto"} to auto-select a space based on matched terms.
#' @param suggest Logical; if \code{TRUE}, will search for other terms for possible inclusion
#' in \code{space}.
#' @param suggestion_terms Number of terms to use when selecting suggested additions.
#' @param suggest_stopwords Logical; if \code{TRUE}, will potentially suggest function words.
#' @param expand_cutoff_freq Proportion of mapped terms to include when expanding dictionary terms.
#' Applies when \code{space} is a character (referring to a space to be loaded).
#' @param expand_cutoff_spaces Number of spaces in which a term has to appear to be considered
#' for expansion. Applies when \code{space} is a character (referring to a space to be loaded).
#' @param dimension_prop Proportion of dimensions to use when searching for suggested additions,
#' where less than 1 will calculate similarities to the category core using fewer dimensions
#' of the space.
#' @param glob Logical; if \code{TRUE}, converts globs (asterisk wildcards) to regular expressions.
#' @param space_dir Directory from which \code{space} should be loaded.
#' @param verbose Logical; if \code{FALSE}, will not show status messages.
#' @returns A list
#' @export

dictionary_meta <- function(
    dict, space = "auto", suggest = FALSE, suggestion_terms = 10, suggest_stopwords = FALSE,
    expand_cutoff_freq = .98, expand_cutoff_spaces = 10, dimension_prop = 1, glob = TRUE,
    space_dir = getOption("lingmatch.lspace.dir"), verbose = TRUE) {
  if (missing(dict)) stop("dict must be specified", call. = FALSE)
  if (!is.list(dict)) dict <- list(dict)
  if (is.null(names(dict))) names(dict) <- paste0("cat", seq_along(dict))
  st <- proc.time()[[3]]
  if (verbose) cat("preparing terms (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  terms <- data.frame(category = rep(names(dict), vapply(dict, length, 0)), term = unlist(dict))
  terms$regex <- paste0("^", to_regex(list(terms$term), TRUE, glob)[[1]], "$")
  if (is.character(space)) {
    term_map <- select.lspace(dir = space_dir, get.map = TRUE)$term_map
    if (is.null(term_map)) {
      stop(
        "term map not found; specify `space_dir` or provide text",
        call. = FALSE
      )
    }
    if (expand_cutoff_freq > 0 && expand_cutoff_freq < 1) {
      term_map <- term_map[seq(1, ceiling(nrow(term_map) * expand_cutoff_freq)), ]
    }
    if (expand_cutoff_spaces > 0 && expand_cutoff_spaces < ncol(term_map)) {
      term_map <- term_map[rowSums(term_map != 0) >= expand_cutoff_spaces, ]
    }
    if (space[[1]] %in% colnames(term_map)) term_map <- term_map[term_map[, space[[1]]] != 0, ]
    space_terms <- rownames(term_map)
  } else {
    space_terms <- rownames(space)
    if (is.null(space_terms)) {
      stop(
        "for space, enter a name or matrix-like object with terms as rownames",
        call. = FALSE
      )
    }
  }
  if (verbose) cat("expanding terms (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  matches <- extract_matches(terms$regex, space_terms, FALSE)
  matched_terms <- unique(unlist(lapply(matches, names), use.names = FALSE))
  if (is.character(space)) {
    if (verbose) cat("loading space (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
    if (space[[1]] == "auto") {
      space <- rownames(select.lspace(
        terms = matched_terms, dir = space_dir
      )$selected)[[1]]
    }
    space_name <- space
    space <- lma_lspace(if (suggest) "" else matched_terms, space, dir = space_dir)
    matches <- lapply(matches, function(l) l[names(l) %in% space_terms])
    matched_terms <- unique(unlist(lapply(matches, names), use.names = FALSE))
  } else {
    space_name <- "custom"
  }
  cat_names <- structure(names(dict), names = names(dict))
  dict_exp <- lapply(cat_names, function(cat) {
    unique(names(unlist(matches[terms$category == cat])))
  })
  space <- as(space, "CsparseMatrix")
  if (!suggest_stopwords) {
    if (verbose) cat("removing stopwords (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
    space <- space[!lma_dict(as.function = TRUE)(rownames(space)), ]
  }
  space_terms <- rownames(space)
  if (verbose) cat("calculating term similarities (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  sims <- lapply(cat_names, function(cat) {
    su <- space_terms %in% dict_exp[[cat]]
    if (dimension_prop < 1) {
      loadings <- colSums(space[su, ])
      dsu <- which(loadings >= sort(colSums(space[su, ]))[ceiling(ncol(space) * dimension_prop)])
      if (!length(dsu)) dsu <- which.max(loadings)
      space <- space[, dsu, drop = FALSE]
    }
    lma_simets(space, space[su, ], metric = "cosine", pairwise = TRUE, symmetrical = TRUE)
  })
  suggested <- if (suggest) {
    if (verbose) cat("identifying potential additions (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
    lapply(sims, function(s) {
      if (length(s)) {
        su <- rownames(s) %in% colnames(s)
        core <- names(sort(rowSums(s[su, ]), TRUE)[seq(1, min(ncol(s), max(5, min(20, ceiling(ncol(s) * .9)))))])
        loadings <- sort(rowMeans(s * colnames(s) %in% core)[!su], TRUE)
        co <- min(length(loadings), max(which(loadings > 0)), suggestion_terms)
        loadings[loadings > loadings[co] + Reduce("-", range(loadings[seq(1, co)])) / 2]
      }
    })
  } else {
    NULL
  }
  if (verbose) cat("preparing results (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  summary <- cbind(data.frame(
    category = cat_names,
    n_terms = vapply(dict, length, 0),
    n_expanded = tapply(vapply(matches, length, 0), terms$category, sum)[cat_names],
    sim.space = space_name
  ), sim = do.call(rbind, lapply(sims, function(s) {
    if (length(s)) {
      s <- s[colnames(s), ]
      m <- (colSums(s) - 1) / (ncol(s) - 1)
    } else {
      m <- 0
    }
    structure(summary(m), names = c("min", "q1", "median", "mean", "q3", "max"))
  })))
  if (verbose) cat("done (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  list(expanded = dict_exp, summary = summary, suggested = suggested, sims = sims)
}
