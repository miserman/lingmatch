#' Assess Dictionary Categories Within a Latent Semantic Space
#'
#'
#' @param dict A vector of terms, list of such vectors, or a matrix-like object to be
#' categorized by \code{\link{read.dic}}.
#' @param space A vector space used to calculate similarities between terms.
#' Names of spaces (see \code{\link{select.lspace}}), a matrix with terms as row names, or
#' \code{"auto"} to auto-select a space based on matched terms. This can also be \code{multi}
#' to use multiple spaces, which are combined after similarities are calculated.
#' @param n_spaces Number of spaces to draw from if \code{space} is \code{multi}.
#' @param suggest Logical; if \code{TRUE}, will search for other terms for possible inclusion
#' in \code{space}.
#' @param suggestion_terms Number of terms to use when selecting suggested additions.
#' @param suggest_stopwords Logical; if \code{TRUE}, will suggest function words.
#' @param suggest_discriminate Logical; if \code{TRUE}, will adjust for similarity to other
#' categories when finding suggestions.
#' @param expand_cutoff_freq Proportion of mapped terms to include when expanding dictionary terms.
#' Applies when \code{space} is a character (referring to a space to be loaded).
#' @param expand_cutoff_spaces Number of spaces in which a term has to appear to be considered
#' for expansion. Applies when \code{space} is a character (referring to a space to be loaded).
#' @param dimension_prop Proportion of dimensions to use when searching for suggested additions,
#' where less than 1 will calculate similarities to the category core using fewer dimensions
#' of the space.
#' @param pairwise Logical; if \code{FALSE}, will compare candidate suggestion terms with a single,
#' averaged category vector rather than all category terms separately.
#' @param glob Logical; if \code{TRUE}, converts globs (asterisk wildcards) to regular expressions.
#' @param space_dir Directory from which \code{space} should be loaded.
#' @param verbose Logical; if \code{FALSE}, will not show status messages.
#' @family Dictionary functions
#' @seealso
#' To just expand fuzzy terms, see \code{\link{report_term_matches}()}.
#'
#' Similar information is provided in the \href{https://miserman.github.io/dictionary_builder/}{dictionary builder} web tool.
#' @returns A list:
#' \itemize{
#'   \item \strong{\code{expanded}}: A version of \code{dict} with fuzzy terms expanded.
#'   \item \strong{\code{summary}}: A summary of each dictionary category.
#'   \item \strong{\code{terms}}: Match (expanded term) similarities within terms and categories.
#'   \item \strong{\code{suggested}}: If \code{suggest} is \code{TRUE}, a list with suggested
#'   additions for each dictionary category. Each entry is a named numeric vector with
#'   similarities for each suggested term.
#' }
#' @examples
#' if (dir.exists("~/Latent Semantic Spaces")) {
#'   dict <- list(
#'     furniture = c("table", "chair", "desk*", "couch*", "sofa*"),
#'     well_adjusted = c("happy", "bright*", "friend*", "she", "he", "they")
#'   )
#'   dictionary_meta(dict, space_dir = "~/Latent Semantic Spaces")
#' }
#' @export

dictionary_meta <- function(
    dict, space = "auto", n_spaces = 5, suggest = FALSE, suggestion_terms = 10, suggest_stopwords = FALSE,
    suggest_discriminate = TRUE, expand_cutoff_freq = .98, expand_cutoff_spaces = 10,
    dimension_prop = 1, pairwise = TRUE, glob = TRUE, space_dir = getOption("lingmatch.lspace.dir"), verbose = TRUE) {
  if (missing(dict)) stop("dict must be specified", call. = FALSE)
  if (!is.list(dict)) dict <- list(dict)
  if (is.null(names(dict))) names(dict) <- paste0("cat", seq_along(dict))
  st <- proc.time()[[3]]
  if (verbose) cat("preparing terms (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  terms <- data.frame(category = rep(names(dict), vapply(dict, length, 0)), term = unlist(dict))
  rownames(terms) <- NULL
  terms$regex <- paste0("\\b", to_regex(list(terms$term), TRUE, glob)[[1]], "\\b")
  if (is.character(space)) {
    term_map <- select.lspace(dir = space_dir, get.map = TRUE)$term_map
    if (is.null(term_map)) {
      stop(
        "term map not found; specify `space_dir` or provide text",
        call. = FALSE
      )
    }
    if (expand_cutoff_freq > 0 && expand_cutoff_freq < 1) {
      term_map <- term_map[seq(1, ceiling(nrow(term_map) * expand_cutoff_freq)), , drop = FALSE]
    }
    if (expand_cutoff_spaces > 0 && expand_cutoff_spaces < ncol(term_map)) {
      term_map <- term_map[rowSums(term_map != 0) >= expand_cutoff_spaces, , drop = FALSE]
    }
    if (space[[1]] %in% colnames(term_map)) term_map <- term_map[term_map[, space[[1]]] != 0, drop = FALSE]
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
  matches <- extract_matches(terms$regex, paste(space_terms, collapse = "  "), TRUE)
  matched_terms <- unique(unlist(lapply(matches, names), use.names = FALSE))
  multi <- FALSE
  if (is.character(space)) {
    if (length(space) == 1 && !missing(n_spaces) && n_spaces > 1) space <- "multi"
    multi <- grepl("^multi", space[[1]], TRUE)
    if (length(space) > 1 || multi) {
      if (length(space) == 1 && multi) {
        term_map_matched <- term_map[rownames(term_map) %in% matched_terms, , drop = FALSE]
        commonness <- sort(-rowSums(term_map_matched != 0))
        common_terms <- names(commonness[commonness >= min(max(commonness), n_spaces)])
        space <- names(sort(-colSums(term_map_matched[common_terms, , drop = FALSE])))[
          seq_len(max(1, min(nrow(term_map), n_spaces)))
        ]
      }
      term_map <- term_map[, space, drop = FALSE]
      space_terms <- matched_terms <- rownames(term_map)[rowSums(term_map != 0) == length(space)]
      space_name <- paste(space, collapse = ", ")
      if (verbose) cat("loading spaces (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
      space <- lapply(space, function(s) {
        lma_lspace(
          if (suggest) space_terms else matched_terms[matched_terms %in% space_terms], s,
          dir = space_dir
        )
      })
      matches <- lapply(matches, function(l) l[names(l) %in% space_terms])
      matched_terms <- unique(unlist(lapply(matches, names), use.names = FALSE))
    } else {
      if (space == "auto") {
        space <- colnames(term_map)[
          which.max(colSums(term_map[rownames(term_map) %in% matched_terms, ] != 0))
        ]
      }
      space_name <- space
      space_terms <- rownames(term_map)[term_map[, space] != 0]
      if (verbose) cat("loading space (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
      space <- lma_lspace(
        if (suggest) space_terms else matched_terms[matched_terms %in% space_terms], space,
        dir = space_dir
      )
      matches <- lapply(matches, function(l) l[names(l) %in% space_terms])
      matched_terms <- unique(unlist(lapply(matches, names), use.names = FALSE))
    }
  } else {
    space_name <- "custom"
  }
  cat_names <- structure(names(dict), names = names(dict))
  dict_exp <- lapply(cat_names, function(cat) {
    unique(names(unlist(matches[terms$category == cat])))
  })
  if (multi) {
    space <- lapply(space, function(s) as(s, "CsparseMatrix"))
    if (!suggest) space_terms <- rownames(space[[1]])
    if (verbose) cat("calculating term similarities (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
    sims <- lapply(cat_names, function(cat) {
      su <- space_terms %in% dict_exp[[cat]]
      if (any(su)) {
        Reduce("+", lapply(space, function(s) {
          if (dimension_prop < 1) {
            loadings <- colMeans(s[su, , drop = FALSE])
            dsu <- order(-loadings)[seq(1, max(1, ceiling(ncol(s) * dimension_prop)))]
            s <- s[, dsu, drop = FALSE]
          }
          if (pairwise) {
            sim <- lma_simets(s, s[su, ], metric = "cosine", pairwise = TRUE, symmetrical = TRUE)
            if (is.null(dim(sim))) sim <- t(t(sim))
            diag(sim[su, ]) <- 0
            ms <- min(sim)
            sim <- (sim - ms) / (max(sim) - ms) * sign(sim)
            diag(sim[su, ]) <- 1
          } else {
            sim <- lma_simets(s, colMeans(s[su, , drop = FALSE]), metric = "cosine", pairwise = TRUE, symmetrical = TRUE)
            if (is.null(dim(sim))) sim <- t(t(sim))
          }
          sim
        })) / length(space)
      }
    })
  } else {
    space <- as(space, "CsparseMatrix")
    if (!suggest) space_terms <- rownames(space)
    if (verbose) cat("calculating term similarities (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
    sims <- lapply(cat_names, function(cat) {
      su <- space_terms %in% dict_exp[[cat]]
      if (dimension_prop < 1) {
        loadings <- colSums(space[su, , drop = FALSE])
        dsu <- order(loadings, decreasing = TRUE)[seq(1, max(1, ceiling(ncol(space) * dimension_prop)))]
        space <- space[, dsu, drop = FALSE]
      }
      sim <- lma_simets(space, if (pairwise) {
        space[su, , drop = FALSE]
      } else {
        colMeans(space[su, , drop = FALSE])
      }, metric = "cosine", pairwise = TRUE, symmetrical = TRUE)
      if (is.null(dim(sim))) sim <- t(t(sim))
      sim
    })
  }
  if (suggest) {
    if (verbose) cat("identifying potential additions (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
    if (!suggest_stopwords) is_stop <- lma_dict(as.function = TRUE)
    full_loadings <- do.call(cbind, lapply(sims, function(x) {
      if (length(x)) {
        rowMeans(x)
      } else {
        structure(numeric(length(space_terms)), names = space_terms)
      }
    }))
    loading_cat <- names(cat_names)[max.col(full_loadings)]
    suggested <- lapply(cat_names, function(cat) {
      s <- sims[[cat]]
      if (length(s)) {
        su <- !rownames(s) %in% dict_exp[[cat]] & loading_cat == cat
        loadings <- sort(if (suggest_discriminate) {
          nl <- full_loadings[su, colnames(full_loadings) != cat, drop = FALSE]
          (rowMeans(s[su, , drop = FALSE]) - nl[
            rep(seq_len(ncol(nl)), each = nrow(nl)) == max.col(nl)
          ]) / 2
        } else {
          rowMeans(s[su, , drop = FALSE])
        }, TRUE)
        if (!suggest_stopwords) loadings <- loadings[!is_stop(names(loadings))]
        co <- min(length(loadings), max(which(loadings > 0)), suggestion_terms)
        loadings[loadings > loadings[co] + Reduce("-", range(loadings[seq(1, co)])) / 2]
      }
    })
  }
  if (verbose) cat("preparing results (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  match_counts <- vapply(matches, length, 0)
  term_summary <- data.frame(
    terms[rep(seq_len(nrow(terms)), match_counts), c("category", "term")],
    match = unlist(lapply(matches, names))
  )
  term_summary <- cbind(term_summary, do.call(rbind, lapply(
    split(
      term_summary[, c("category", "match", "term")],
      term_summary$category
    )[unique(term_summary$category)],
    function(cl) {
      cat <- cl$category[[1]]
      if (pairwise) {
        s <- sims[[cat]]
      } else {
        su <- space_terms %in% dict_exp[[cat]]
        if (multi) {
          s <- Reduce("+", lapply(space, function(s) {
            sim <- lma_simets(s[su, , drop = FALSE], metric = "cosine", pairwise = TRUE, symmetrical = TRUE)
            if (is.null(dim(sim))) sim <- t(t(sim))
            diag(sim) <- 0
            ms <- min(sim)
            sim <- (sim - ms) / (max(sim) - ms) * sign(sim)
            diag(sim) <- 1
            sim
          })) / length(space)
        } else {
          s <- lma_simets(space[su, , drop = FALSE], metric = "cosine", pairwise = TRUE, symmetrical = TRUE)
        }
        if (is.null(dim(s))) s <- t(t(s))
      }
      if (is.null(s)) {
        cbind(sim.term = cl$match, sim.category = 0)
      } else {
        su <- !(cl$match %in% rownames(s))
        if (any(su)) s <- rbind(s, Matrix(0, sum(su), ncol(s), dimnames = list(cl$match[su], colnames(s))))
        term_sims <- unlist(lapply(unname(split(cl$match, cl$term)[unique(cl$term)]), function(l) {
          if (length(l) == 1) {
            structure(1, names = l)
          } else {
            cols <- l[l %in% colnames(s)]
            s[l, cols[which.min(nchar(cols))]]
          }
        }))
        cat_sims <- s[cl$match, which.max(if (is.null(dim(s))) s else colMeans(s[colnames(s), , drop = FALSE]))]
        cbind(sim.term = term_sims, sim.category = if (is.null(cat_sims)) 0 else cat_sims)
      }
    }
  )))
  summary <- cbind(data.frame(
    category = cat_names,
    n_terms = vapply(dict, length, 0),
    n_expanded = tapply(match_counts, terms$category, sum)[cat_names],
    sim.space = space_name
  ), sim = do.call(rbind, lapply(sims, function(s) {
    if (length(s) && !is.null(ncol(s)) && ncol(s) == 1) {
      m <- s[matched_terms, 1]
    } else if (length(s)) {
      s <- s[colnames(s), , drop = FALSE]
      m <- (colSums(s) - 1) / (ncol(s) - 1)
    } else {
      m <- 0
    }
    structure(summary(m), names = c("min", "q1", "median", "mean", "q3", "max"))
  })))
  if (verbose) cat("done (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  list(expanded = dict_exp, summary = summary, terms = term_summary, suggested = if (suggest) suggested)
}
