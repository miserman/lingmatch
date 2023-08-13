#' Generate a Report of Term Matches
#'
#' Extract matches to fuzzy terms (globs/wildcards or regular expressions) from provided text, in order
#' to assess their appropriateness for inclusion in a dictionary.
#' @param dict A vector of terms, list of such vectors, or a matrix-like object to be
#' categorized by \code{\link{read.dic}}.
#' @param text A vector of text to extract matches from. If not specified, will use the terms
#' in the \code{term_map} retrieved from \code{\link{select.lspace}}.
#' @param space A vector space used to calculate similarities between term matches.
#' Name of a the space (see \code{\link{select.lspace}}), a matrix with terms as row names, or
#' \code{TRUE} to auto-select a space based on matched terms.
#' @param glob Logical; if \code{TRUE}, converts globs (asterisk wildcards) to regular expressions.
#' If not specified, this will be set automatically.
#' @param parse_phrases Logical; if \code{TRUE} (default) and \code{space} is specified, will
#' break unmatched phrases into single terms, and average across and matched vectors.
#' @param tolower Logical; if \code{FALSE}, will retain \code{text}'s case.
#' @param punct Logical; if \code{FALSE}, will remove punctuation markings in \code{text}.
#' @param special Logical; if \code{FALSE}, will attempt to replace special characters in \code{text}.
#' @param as_terms Logical; if \code{TRUE}, will treat \code{text} as terms, meaning \code{dict}
#' terms will only count as matches when matching the complete text.
#' @param bysentence Logical; if \code{TRUE}, will split \code{text} into sentences, and only
#' consider unique sentences.
#' @param as_string Logical; if \code{FALSE}, returns matches as tables rather than a string.
#' @param outFile File path to write results to, always ending in \code{.csv}.
#' @param space_dir Directory from which \code{space} should be loaded.
#' @param verbose Logical; if \code{FALSE}, will not display status messages.
#' @note
#' Matches are extracted for each term independently, so they may not align with some implementations
#' of dictionaries. For instance, by default \code{\link{lma_patcat}} matches destructively, and sorts
#' terms by length such that shorter terms will not match the same text and longer terms that overlap.
#' Here, the match would show up for both terms.
#' @returns A \code{data.frame} of results, with a row for each unique term, and the following columns:
#' \itemize{
#'   \item \strong{\code{term}}: The originally entered term.
#'   \item \strong{\code{regex}}: The converted and applied regular expression form of the term.
#'   \item \strong{\code{categories}}: Comma-separated category names,
#'   if \code{dict} is a list with named entries.
#'   \item \strong{\code{count}}: Total number of matches to the term.
#'   \item \strong{\code{max_count}}: Number of matches to the most representative
#'   (that with the highest average similarity) variant of the term.
#'   \item \strong{\code{variants}}: Number of variants of the term.
#'   \item \strong{\code{space}}: Name of the latent semantic space, if one was used.
#'   \item \strong{\code{mean_sim}}: Average similarity to the most representative variant among terms
#'   found in the space, if one was used.
#'   \item \strong{\code{min_sim}}: Minimal similarity to the most representative variant.
#'   \item \strong{\code{matches}}: Variants, with counts and similarity (Pearson's r) to the
#'   most representative term (if a space was specified). Either in the form of a comma-separated
#'   string or a \code{data.frame} (if \code{as_string} is \code{FALSE}).
#' }
#' @examples
#' text <- c(
#'   "I am sadly homeless, and suffering from depression :(",
#'   "This wholesome happiness brings joy to my heart! :D:D:D",
#'   "They are joyous in these fearsome happenings D:",
#'   "I feel weightless now that my sadness has been depressed! :()"
#' )
#' dict <- list(
#'   sad = c("*less", "sad*", "depres*", ":("),
#'   happy = c("*some", "happ*", "joy*", "d:"),
#'   self = c("i *", "my *")
#' )
#'
#' report_term_matches(dict, text)
#' @export

report_term_matches <- function(dict, text = NULL, space = NULL, glob = TRUE,
                                parse_phrases = TRUE, tolower = TRUE, punct = TRUE, special = TRUE,
                                as_terms = FALSE, bysentence = FALSE, as_string = TRUE, outFile = NULL,
                                space_dir = getOption("lingmatch.lspace.dir"), verbose = TRUE) {
  if (missing(dict)) stop("dict must be specified", .call = FALSE)
  if (is.null(text)) {
    text <- rownames(select.lspace(dir = space_dir, get.map = TRUE)$term_map)
    as_terms <- TRUE
  }
  if (is.null(text) && is.null(space)) stop("either text or space must be specified", .call = FALSE)
  st <- proc.time()[[3]]
  if (!is.null(text) && !as_terms) {
    if (verbose) cat("preparing text (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
    if (bysentence) text <- read.segments(text, segment.size = 1, bysentence = TRUE)$text
    if (tolower) text <- tolower(text)
    if (!punct) text <- gsub("[,_:;/\\\\.?!\"()\\{}[]|\\]", " ", text)
    if (!special) text <- lma_dict("special", as.function = gsub)(text)
    text <- unique(text)
  }
  if (verbose) cat("preparing dict (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  if (!is.null(dim(dict)) || (is.character(dict) && length(dict) == 1 && file.exists(dict))) dict <- read.dic(dict)
  terms <- data.frame(term = unique(unlist(dict, use.names = FALSE)))
  if (missing(glob)) {
    glob <- any(grepl("^\\*", terms$term))
    if (!glob) {
      glob <- any(grepl("\\*$", terms$term))
      if (glob) glob <- !any(grepl("(?:\\\\\\w|[].})])\\*$", terms$term))
    }
  }
  space_name <- NULL
  if (is.null(text)) {
    if (is.character(space)) {
      space_name <- space
      if (verbose) cat("preloading space (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
      space <- lma_lspace(space, dir = space_dir)
    }
    text <- rownames(space)
  }
  rawtext <- is.null(space_name) && !as_terms
  terms$regex <- to_regex(list(terms$term), TRUE, glob)[[1]]
  terms <- terms[!is.na(terms$regex) & terms$regex != "", ]
  terms$regex <- if (rawtext) paste0("\\b", terms$regex, "\\b") else paste0("^", terms$regex, "$")
  if (is.list(dict)) {
    if (is.null(names(dict))) names(dict) <- paste0("cat_", seq_along(dict))
    categories <- character(nrow(terms))
    for (cat in names(dict)) {
      su <- terms$term %in% dict[[cat]]
      if (any(su)) {
        ssu <- su & categories != ""
        categories[ssu] <- paste0(categories[ssu], ", ", cat)
        categories[su & categories == ""] <- cat
      }
    }
    terms$categories <- categories
  }
  if (verbose) cat("extracting matches (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  matches <- extract_matches(terms$regex, text, rawtext)
  has_space <- FALSE
  if (!is.null(space)) {
    obs <- unique(unlist(lapply(matches, names), use.names = FALSE))
    if (is.null(space_name)) {
      if (is.logical(space) && space) {
        space <- select.lspace(terms = obs)$selected
        space <- if (nrow(space)) rownames(space)[1] else NULL
      }
      if (is.character(space)) {
        if (verbose) cat("loading space (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
        space_name <- space
        space <- lma_lspace(obs, space, dir = space_dir)
      }
    }
    if (!nrow(space) || !any(obs %in% rownames(space))) space <- NULL
    if (is.null(space)) {
      warning("failed to recognize space")
    } else {
      su <- obs %in% rownames(space)
      if (parse_phrases && any(!su)) {
        phrase <- grepl("[ _/-]", obs)
        if (any(phrase)) {
          if (verbose) cat("parsing phrases (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
          split_parts <- strsplit(obs[phrase], "[ _/-]")
          parts <- unique(unlist(split_parts, use.names = FALSE))
          part_vectors <- if (!is.null(space_name)) {
            lma_lspace(parts, space_name)
          } else {
            if (any(parts %in% rownames(space))) space[parts[parts %in% rownames(space)]] else space[0, ]
          }
          if (nrow(part_vectors)) {
            space_terms <- rownames(part_vectors)
            space_dim <- ncol(space)
            names(split_parts) <- obs[phrase]
            agg_vectors <- t(vapply(split_parts, function(p) {
              su <- p %in% space_terms
              if (any(su)) {
                colMeans(part_vectors[p[su], , drop = FALSE])
              } else {
                numeric(space_dim)
              }
            }, numeric(space_dim)))
            space <- rbind(space, agg_vectors[rowSums(agg_vectors) != 0, ])
          }
        }
      }
      if (is.null(space_name)) space_name <- "custom"
      has_space <- TRUE
    }
  }
  if (verbose) cat("preparing results (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  terms <- cbind(terms, do.call(rbind, lapply(matches, function(m) {
    hits <- if (length(m)) {
      if (has_space) {
        msim <- m
        if (length(m) == 1) {
          msim[] <- 1L
        } else {
          msim[] <- NA
          su <- names(m) %in% rownames(space)
          if (sum(su) == 1) {
            msim[su] <- 1L
          } else if (any(su)) {
            ns <- names(m)[su]
            sims <- lma_simets(space[ns, ], metric = "pearson", symmetrical = TRUE)
            msim[su] <- as.numeric(sims[, which.max(colMeans(sims))])
          }
        }
        o <- order(msim, decreasing = TRUE)
        m <- m[o]
        if (as_string) {
          paste(paste0(names(m), " (", if (!as_terms) paste0(m, ", "), round(msim[o], 2), ")"), collapse = ", ")
        } else {
          list(as.data.frame(rbind(m, msim[o])))
        }
      } else {
        m <- sort(m, TRUE)
        if (as_string) {
          paste(paste0(names(m), if (!as_terms) paste0(" (", m, ")")), collapse = ", ")
        } else {
          list(t(as.data.frame(m)))
        }
      }
    } else {
      if (has_space) msim <- NA
      if (as_string) "" else list(data.frame())
    }
    if (!as_string) rownames(hits[[1]]) <- NULL
    res <- data.frame(
      count = sum(m),
      max_count = if (all(is.na(m))) 0L else max(m, na.rm = TRUE),
      variants = length(m)
    )
    if (has_space) {
      res$space <- space_name
      if (all(is.na(msim))) {
        res$mean_sim <- res$min_sim <- NA
      } else {
        res$mean_sim <- mean(msim, na.rm = TRUE)
        res$min_sim <- min(msim, na.rm = TRUE)
      }
    }
    res[["matches"]] <- hits
    res
  })))
  terms <- terms[if (has_space) order(terms$mean_sim) else order(terms$variants, decreasing = TRUE), ]
  rownames(terms) <- NULL
  if (!is.null(outFile)) {
    outFile <- paste0(sub(".csv", "", outFile, fixed = TRUE), ".csv")
    if (verbose) cat("writing results: ", outFile, " (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
    write.table(terms, outFile, sep = ",", row.names = FALSE, qmethod = "double")
  }
  if (verbose) cat("done (", round(proc.time()[[3]] - st, 4), ")\n", sep = "")
  terms
}
