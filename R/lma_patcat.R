#' Categorize Texts
#'
#' Categorize raw texts using a pattern-based dictionary.
#'
#' @param text A vector of text to be categorized. Texts are padded by 2 spaces, and potentially lowercased.
#' @param dict At least a vector of terms (patterns), usually a matrix-like object with columns for terms,
#'   categories, and weights.
#' @param pattern.weights A vector of weights corresponding to terms in \code{dict}, or the column name of
#'   weights found in \code{dict}.
#' @param pattern.categories A vector of category names corresponding to terms in \code{dict}, or the column name of
#'   category names found in \code{dict}.
#' @param bias A constant to add to each category after weighting and summing. Can be a vector with names
#'   corresponding to the unique values in \code{dict[, category]}, but is usually extracted from dict based
#'   on the intercept included in each category (defined by \code{name.map['intname']}).
#' @param to.lower Logical indicating whether \code{text} should be converted to lowercase before processing.
#' @param return.dtm Logical; if \code{TRUE}, only a document-term matrix will be returned, rather than the
#'   weighted, summed, and biased category values.
#' @param exclusive Logical; if \code{FALSE}, each dictionary term is searched for in the original text.
#'   Otherwise (by default), terms are sorted by length (with longer terms being searched for first), and
#'   matches are removed from the text (avoiding subsequent matches to matched patterns).
#' @param drop.zeros logical; if \code{TRUE}, categories or terms with no matches will be removed.
#' @param boundary A string to add to the beginning and end of each dictionary term. If \code{TRUE},
#'   \code{boundary} will be set to \code{' '}, avoiding pattern matches within words. By default, dictionary
#'   terms are left as entered.
#' @param fixed Logical; if \code{FALSE}, patterns are treated as regular expressions.
#' @param globtoregex Logical; if \code{TRUE}, initial and terminal asterisks are replaced with \code{\\\\b\\\\w*}
#'   and \code{\\\\w*\\\\b} respectively. This will also set \code{fixed} to \code{FALSE} unless fixed is specified.
#' @param name.map A named character vector:
#'   \itemize{
#'     \item \strong{\code{intname}}: term identifying category biases within the term list;
#'       defaults to \code{'_intercept'}
#'     \item \strong{\code{term}}: name of the column containing terms in \code{dict}; defaults to \code{'term'}
#'   }
#'   Missing names are added, so names can be specified positional (e.g., \code{c('_int',} \code{'terms')}),
#'   or only some can be specified by name (e.g., \code{c(term =} \code{'patterns')}), leaving the rest default.
#' @param dir Path to a folder in which to look for \code{dict} if it is the name of a file to be passed to
#'   \code{\link{read.dic}}.
#' @seealso For applying term-based dictionaries (to a document-term matrix) see \code{\link{lma_termcat}}.
#' @family Dictionary functions
#' @return A matrix with a row per \code{text} and columns per dictionary category, or (when \code{return.dtm = TRUE})
#' a sparse matrix with a row per \code{text} and column per term. Includes a \code{WC} attribute with original
#' word counts, and a \code{categories} attribute with row indices associated with each category if
#' \code{return.dtm = TRUE}.
#' @examples
#' # example text
#' text <- c(
#'   paste(
#'     "Oh, what youth was! What I had and gave away.",
#'     "What I took and spent and saw. What I lost. And now? Ruin."
#'   ),
#'   paste(
#'     "God, are you so bored?! You just want what's gone from us all?",
#'     "I miss the you that was too. I love that you."
#'   ),
#'   paste(
#'     "Tomorrow! Tomorrow--nay, even tonight--you wait, as I am about to change.",
#'     "Soon I will off to revert. Please wait."
#'   )
#' )
#'
#' # make a document-term matrix with pre-specified terms only
#' lma_patcat(text, c("bored?!", "i lo", ". "), return.dtm = TRUE)
#'
#' # get counts of sets of letter
#' lma_patcat(text, list(c("a", "b", "c"), c("d", "e", "f")))
#'
#' # same thing with regular expressions
#' lma_patcat(text, list("[abc]", "[def]"), fixed = FALSE)
#'
#' # match only words
#' lma_patcat(text, list("i"), boundary = TRUE)
#'
#' # match only words, ignoring punctuation
#' lma_patcat(
#'   text, c("you", "tomorrow", "was"),
#'   fixed = FALSE,
#'   boundary = "\\b", return.dtm = TRUE
#' )
#'
#' \dontrun{
#'
#' # read in the temporal orientation lexicon from the World Well-Being Project
#' tempori <- read.csv(
#'   "https://wwbp.org/downloads/public_data/temporalOrientationLexicon.csv"
#' )
#'
#' lma_patcat(text, tempori)
#'
#' # or use the standardized version
#' tempori_std <- read.dic("wwbp_prospection", dir = "~/Dictionaries")
#'
#' lma_patcat(text, tempori_std)
#'
#' ## get scores on the same scale by adjusting the standardized values
#' tempori_std[, -1] <- tempori_std[, -1] / 100 *
#'   select.dict("wwbp_prospection")$selected[, "original_max"]
#'
#' lma_patcat(text, tempori_std)[, unique(tempori$category)]
#' }
#' @export

lma_patcat <- function(text, dict = NULL, pattern.weights = "weight", pattern.categories = "category", bias = NULL,
                       to.lower = TRUE, return.dtm = FALSE, drop.zeros = FALSE, exclusive = TRUE, boundary = NULL, fixed = TRUE,
                       globtoregex = FALSE, name.map = c(intname = "_intercept", term = "term"), dir = getOption("lingmatch.dict.dir")) {
  if (is.factor(text)) text <- as.character(text)
  if (!is.character(text)) stop("enter a character vector as the first argument")
  text <- paste(" ", text, " ")
  if (is.null(names(name.map)) && length(name.map) < 3) names(name.map) <- c("intname", "term")[seq_along(name.map)]
  wide <- FALSE
  if (missing(dict) && missing(pattern.weights) && missing(pattern.categories)) dict <- lma_dict()
  if (is.character(dict) && length(dict) == 1 && (file.exists(dict) || grepl("^[A-Za-z_]{3}", dict)) &&
    missing(pattern.weights) && missing(pattern.categories)) {
    if (dir == "") dir <- "~/Dictionaries"
    if (!any(file.exists(dict)) && any(file.exists(normalizePath(paste0(dir, "/", dict), "/", FALSE)))) {
      dict <- normalizePath(paste0(dir, "/", dict), "/", FALSE)
    }
    td <- tryCatch(read.dic(dict), error = function(e) NULL)
    dict <- if (is.null(td)) list(cat1 = dict) else td
  }
  if (!is.null(dim(dict))) {
    if (is.null(colnames(dict))) {
      colnames(dict) <- paste0("X", seq_len(ncol(dict)))
    } else {
      if (!is.data.frame(dict)) dict <- as.data.frame(as.matrix(dict), stringsAsFactors = FALSE)
      terms <- if (name.map[["term"]] %in% colnames(dict)) colnames(dict) != name.map[["term"]] else !logical(ncol(dict))
      if (missing(pattern.weights) && !any(pattern.weights %in% colnames(dict))) {
        if (any(su <- terms & vapply(dict, is.numeric, TRUE))) {
          terms <- terms & !su
          pattern.weights <- dict[, su]
        }
      }
      if (missing(pattern.categories) && !pattern.categories %in% colnames(dict)) {
        if (any(su <- terms & vapply(dict, function(v) !is.numeric(v) && anyDuplicated(v), TRUE))) {
          terms <- terms & !su
          pattern.categories <- dict[, su]
          if (sum(su) > 1) pattern.categories <- do.call(paste, pattern.categories)
        }
      }
      if (name.map[["term"]] %in% colnames(dict)) {
        dict[, name.map[["term"]]]
      } else if (!all(terms)) {
        dict <- if (any(terms)) dict[, which(terms)[1]] else rownames(dict)
      }
    }
  }
  # independently entered wide weights
  if ((is.null(dict) || is.null(dim(dict))) && (!is.null(ncol(pattern.weights)) || !is.null(ncol(pattern.categories)))) {
    weights <- if (!is.null(ncol(pattern.weights))) pattern.weights else pattern.categories
    if (!is.null(rownames(weights)) && any(grepl("[^0-9]", rownames(weights)))) {
      dict <- rownames(weights)
    } else if (is.list(dict) && (length(dict) == 1 ||
      (length(dict[[1]]) == nrow(weights) && all(vapply(dict, length, 0) == nrow(weights))))) {
      dict <- dict[[1]]
    }
    if (length(dict) != nrow(weights)) stop("dict and wide weights do not align")
    wide <- TRUE
    if (!missing(pattern.categories) && is.character(pattern.categories) && any(su <- pattern.categories %in% weights)) {
      weights <- weights[, pattern.categories[su], drop = FALSE]
    }
    weights <- weights[, vapply(seq_len(ncol(weights)), function(col) is.numeric(weights[, col]), TRUE), drop = FALSE]
    if (!ncol(weights)) stop("could not identify numeric weights in wide weights")
    lex <- list(terms = dict, weights = weights, category = colnames(weights))
    # wide weights in dict
  } else if (!is.null(dim(dict)) && (
    (length(pattern.weights) > 1 && is.character(pattern.weights)) ||
      (length(pattern.categories) > 1 &&
        (length(pattern.categories) != nrow(dict) || all(pattern.categories %in% colnames(dict)))) ||
      (!any(pattern.weights %in% colnames(dict)) && !any(pattern.categories %in% colnames(dict)))
  )) {
    if (any(su <- pattern.weights %in% colnames(dict))) {
      categories <- pattern.weights[su]
    } else if (any(su <- pattern.categories %in% colnames(dict))) {
      categories <- pattern.categories
    } else if (any(su <- vapply(colnames(dict), function(v) is.numeric(dict[, v]), TRUE))) {
      categories <- colnames(dict)[su]
    } else {
      stop("could not find weights in dict column names")
    }
    wide <- TRUE
    if (!name.map[["term"]] %in% colnames(dict)) {
      terms <- colnames(dict)[vapply(colnames(dict), function(v) !is.numeric(dict[, v]), TRUE)]
      if (!length(terms)) stop("could not find terms in dict")
      name.map[["term"]] <- if (length(terms) > 1) {
        su <- vapply(terms, function(v) !anyDuplicated(dict[, v]), TRUE)
        if (any(su)) terms[which(su)[1]] else terms[1]
      } else {
        terms
      }
    }
    lex <- list(term = dict[, name.map[["term"]]], weights = dict[, categories, drop = FALSE], category = categories)
    # independently entered weights and categories
  } else if (is.null(dim(dict))) {
    if (is.null(dict) || (is.numeric(dict) && is.null(names(dict))) || (is.list(dict) && is.numeric(dict[[1]]) &&
      is.null(names(dict[[1]])))) {
      stop("could not recognize terms in dict")
    }
    n <- length(dict)
    lex <- data.frame(
      term = if (is.character(dict)) {
        dict
      } else if (is.numeric(dict)) {
        names(dict)
      } else if (is.list(dict) &&
        is.numeric(dict[[1]])) {
        unlist(lapply(dict, names), use.names = FALSE)
      } else {
        unlist(dict, use.names = FALSE)
      },
      category = if (length(pattern.categories) == n) {
        if (is.list(dict) && !is.null(names(dict))) {
          names(dict)
        } else {
          pattern.categories
        }
      } else if (is.list(dict)) {
        rep(if (!is.null(names(dict))) {
          names(dict)
        } else {
          paste0("cat", seq_along(dict))
        }, vapply(dict, length, 0))
      } else {
        "cat1"
      },
      weights = if (is.numeric(dict)) {
        unname(dict)
      } else if (is.numeric(pattern.weights)) {
        if (!is.null(names(pattern.weights)) && is.character(dict) && all(dict %in% names(pattern.weights))) {
          pattern.weights[dict]
        } else {
          pattern.weights
        }
      } else if (is.list(dict)) {
        if (is.numeric(dict[[1]])) {
          unlist(dict, use.names = FALSE)
        } else if (is.list(pattern.weights) && is.numeric(pattern.weights[[1]])) {
          unlist(pattern.weights, use.names = FALSE)
        } else {
          1
        }
      } else {
        1
      }, stringsAsFactors = FALSE
    )
  } else {
    term <- if ("term" %in% names(name.map)) name.map[["term"]] else "term"
    en <- colnames(dict)
    if (!term %in% en) {
      su <- vapply(en, function(v) !is.numeric(dict[, v]), TRUE)
      if (any(su)) {
        term <- en[which(su)[1]]
        if (sum(su) > 1) {
          su <- su & vapply(en, function(v) !anyDuplicated(dict[, v]), TRUE)
          if (any(su)) term <- en[which(su)[1]]
        }
      } else {
        stop("could not recognize terms in dict")
      }
    }
    lex <- data.frame(
      term = dict[[term]],
      category = if (length(pattern.categories) == nrow(dict)) {
        pattern.categories
      } else
      if (pattern.categories %in% en) dict[[pattern.categories]] else "cat1",
      weights = if (length(pattern.weights) == nrow(dict)) {
        pattern.weights
      } else
      if (all(pattern.weights %in% en)) dict[[pattern.weights]] else 1, stringsAsFactors = FALSE
    )
  }
  if (any(lex$category == "")) lex[lex$category == "", "category"] <- "cat_unnamed"
  if (is.factor(lex$term)) lex$term <- as.character(lex$term)
  if (globtoregex || !fixed) {
    lex$term <- to_regex(list(lex$term), TRUE, globtoregex)[[1]]
    if (missing(fixed)) fixed <- FALSE
  }
  if (wide && return.dtm) {
    wide <- FALSE
    lex <- data.frame(term = lex$term, category = if (length(lex$category) == 1) lex$category else "all")
  }
  if (!return.dtm && is.null(bias)) {
    if (!"intname" %in% names(name.map)) name.map[["intname"]] <- "_intercept"
    if (any(su <- lex$term == name.map[["intname"]])) {
      if (wide) {
        bias <- structure(lex$weights[su, ], names = lex$categories[su])
        lex$term <- lex$term[!su]
        lex$weights <- lex$weights[!su, , drop = FALSE]
      } else {
        bias <- structure(lex[su, "weights"], names = lex[su, "category"])
        lex <- lex[!su, ]
      }
    }
  }
  if (exclusive) {
    cls <- tryCatch(-nchar(lex$term), error = function(e) NULL)
    if (is.null(cls)) {
      warning(
        "dict appears to be misencoded, so results may not be as expected;\n",
        'might try reading the dictionary in with encoding = "latin1"'
      )
      lex$term <- iconv(lex$term, sub = "#")
      cls <- -nchar(lex$term)
    }
    if (wide) {
      o <- order(cls)
      lex$term <- lex$term[o]
      lex$weights <- lex$weights[o, ]
    } else {
      lex <- lex[order(cls), ]
    }
  }
  lex$category <- factor(lex$category, unique(lex$category))
  categories <- levels(lex$category)
  if (length(bias)) {
    if (is.null(names(bias)) && length(bias) == length(categories)) names(bias) <- categories
    if (any(su <- !categories %in% names(bias))) bias[categories[su]] <- 0
  } else {
    bias <- structure(integer(length(categories)), names = categories)
  }
  bias <- bias[categories]
  if (is.logical(boundary) && boundary) boundary <- " "
  if (missing(to.lower)) {
    if (any(grepl("[A-Z]", lex$term))) {
      to.lower <- FALSE
      if (!any(grepl("[a-z]", lex$term))) text <- toupper(text)
    }
  }
  if (to.lower) text <- tolower(text)
  st <- proc.time()[[3]]
  terms <- unique(lex$term)
  if (!fixed) {
    ck <- tryCatch(
      suppressWarnings(grepl(paste0("(?:", paste(terms, collapse = "|"), ")"), "", perl = TRUE)),
      error = function(e) NULL
    )
    if (is.null(ck)) stop("terms contain invalid regular expressions", call. = FALSE)
  }
  op <- pattern_search(
    text, if (is.character(boundary)) paste0(boundary, terms, boundary) else terms,
    seq_along(terms) - 1L, fixed, exclusive
  )
  colnames(op[[1]]) <- terms
  if (return.dtm) {
    attr(op[[1]], "categories") <- lapply(categories, function(cat) {
      which(colnames(op[[1]]) %in% lex[lex$category == cat, "term"])
    })
    names(attr(op[[1]], "categories")) <- categories
  } else {
    op[[1]] <- vapply(categories, function(cat) {
      l <- if (wide) {
        data.frame(term = lex$term, weights = if (cat %in% colnames(lex$weights)) {
          lex$weights[, cat]
        } else {
          lex$weights
        }, stringsAsFactors = FALSE)
      } else {
        lex[lex$category == cat, ]
      }
      as.numeric(op[[1]][, l$term, drop = FALSE] %*% l$weights + bias[[cat]])
    }, numeric(length(text)))
    if (length(text) == 1) {
      op[[1]] <- t(op[[1]])
      rownames(op[[1]]) <- 1
    }
  }
  attr(op[[1]], "WC") <- op[[2]]
  attr(op[[1]], "time") <- c(patcat = proc.time()[[3]] - st)
  if (drop.zeros) op[[1]] <- op[[1]][, colSums(op[[1]]) != 0, drop = FALSE]
  op[[1]]
}
