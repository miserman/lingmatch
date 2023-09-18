#' Document-Term Matrix Categorization
#'
#' Reduces the dimensions of a document-term matrix by dictionary-based categorization.
#' @param dtm A matrix with terms as column names.
#' @param dict The name of a provided dictionary
#'   (\href{https://osf.io/y6g5b/wiki/home}{osf.io/y6g5b/wiki}) or of a file found in
#'   \code{dir}, or a \code{list} object with named character vectors as word lists,
#'   or the path to a file to be read in by \code{\link{read.dic}}.
#' @param term.weights A \code{list} object with named numeric vectors lining up with the character
#'   vectors in \code{dict}, used to weight the terms in each \code{dict} vector. If a category in
#'   \code{dict} is not specified in \code{term.weights}, or the \code{dict} and \code{term.weights}
#'   vectors aren't the same length, the weights for that category will be 1.
#' @param bias A list or named vector specifying a constant to add to the named category. If a term
#'   matching \code{bias.name} is included in a category, it's associated \code{weight} will be used
#'   as the \code{bias} for that category.
#' @param bias.name A character specifying a term to be used as a category bias; default is
#'   \code{'_intercept'}.
#' @param escape Logical indicating whether the terms in \code{dict} should not be treated as plain
#'   text (including asterisk wild cards). If \code{TRUE}, regular expression related characters are
#'   escaped. Set to \code{TRUE} if you get PCRE compilation errors.
#' @param partial Logical; if \code{TRUE} terms are partially matched (not padded by ^ and $).
#' @param glob Logical; if \code{TRUE} (default), will convert initial and terminal asterisks to
#'   partial matches.
#' @param to.lower Logical; if \code{TRUE} will lowercase dictionary terms. Otherwise, dictionary
#'   terms will be converted to match the terms if they are single-cased. Set to \code{FALSE} to
#'   always keep dictionary terms as entered.
#' @param term.filter A regular expression string used to format the text of each term (passed to
#'   \code{gsub}). For example, if terms are part-of-speech tagged (e.g.,
#'   \code{'a_DT'}), \code{'_.*'} would remove the tag.
#' @param term.break If a category has more than \code{term.break} characters, it will be processed
#'   in chunks. Reduce from 20000 if you get a PCRE compilation error.
#' @param dir Path to a folder in which to look for \code{dict}; \cr
#'   will look in \code{'~/Dictionaries'} by default. \cr
#'   Set a session default with \code{options(lingmatch.dict.dir = 'desired/path')}.
#' @param coverage Logical; if \code{TRUE}, will calculate coverage
#'   (number of unique term matches) for each category.
#' @seealso For applying pattern-based dictionaries (to raw text) see \code{\link{lma_patcat}}.
#' @family Dictionary functions
#' @return A matrix with a row per \code{dtm} row and columns per dictionary category
#' (with added \code{coverage_} versions if \code{coverage} is \code{TRUE}),
#' and a \code{WC} attribute with original word counts.
#' @examples
#' \dontrun{
#'
#' # Score texts with the NRC Affect Intensity Lexicon
#'
#' dict <- readLines("https://saifmohammad.com/WebDocs/NRC-AffectIntensity-Lexicon.txt")
#' dict <- read.table(
#'   text = dict[-seq_len(grep("term\tscore", dict, fixed = TRUE)[[1]])],
#'   col.names = c("term", "weight", "category")
#' )
#'
#' text <- c(
#'   angry = paste(
#'     "We are outraged by their hateful brutality,",
#'     "and by the way they terrorize us with their hatred."
#'   ),
#'   fearful = paste(
#'     "The horrific torture of that terrorist was tantamount",
#'     "to the terrorism of terrorists."
#'   ),
#'   joyous = "I am jubilant to be celebrating the bliss of this happiest happiness.",
#'   sad = paste(
#'     "They are nearly suicidal in their mourning after",
#'     "the tragic and heartbreaking holocaust."
#'   )
#' )
#'
#' emotion_scores <- lma_termcat(text, dict)
#' if (require("splot")) splot(emotion_scores ~ names(text), leg = "out")
#'
#' ## or use the standardized version (which includes more categories)
#'
#' emotion_scores <- lma_termcat(text, "nrc_eil", dir = "~/Dictionaries")
#' emotion_scores <- emotion_scores[, c("anger", "fear", "joy", "sadness")]
#' if (require("splot")) splot(emotion_scores ~ names(text), leg = "out")
#' }
#' @export

lma_termcat <- function(dtm, dict, term.weights = NULL, bias = NULL, bias.name = "_intercept",
                        escape = TRUE, partial = FALSE, glob = TRUE, term.filter = NULL, term.break = 2e4,
                        to.lower = FALSE, dir = getOption("lingmatch.dict.dir"), coverage = FALSE) {
  st <- proc.time()[[3]]
  if (ckd <- dir == "") dir <- "~/Dictionaries"
  if (missing(dict)) dict <- lma_dict(1:9)
  if (is.factor(dict)) dict <- as.character(dict)
  if (is.character(dict) && length(dict) == 1 && missing(term.weights) && (file.exists(dict) || !grepl("\\s", dict))) {
    if (!file.exists(dict) && any(file.exists(normalizePath(paste0(dir, "/", dict), "/", FALSE)))) {
      dict <- normalizePath(paste0(dir, "/", dict))
    }
    td <- tryCatch(read.dic(dict, dir = if (ckd) "" else dir), error = function(e) NULL)
    dict <- if (is.null(td)) list(cat1 = dict) else td
  }
  if (!is.null(dim(dict))) {
    if (!is.null(term.weights)) {
      if (is.character(term.weights) && any(su <- term.weights %in% colnames(dict))) {
        term.weights <- dict[, term.weights[su], drop = FALSE]
      }
      if (!is.null(dim(term.weights))) {
        term.weights <- term.weights[, vapply(
          seq_len(ncol(term.weights)),
          function(col) is.numeric(term.weights[, col]), TRUE
        )]
      }
    } else if (any(su <- vapply(seq_len(ncol(dict)), function(col) is.numeric(dict[, col]), TRUE))) {
      term.weights <- dict[, su, drop = FALSE]
      dict <- if (all(su)) {
        if (!is.null(rownames(dict))) {
          data.frame(term = rownames(dict), stringsAsFactors = FALSE)
        } else {
          term.weights <- if (ncol(term.weights) == 1) NULL else term.weights[, -1, drop = FALSE]
          dict[, 1, drop = FALSE]
        }
      } else {
        dict[, !su, drop = FALSE]
      }
    }
    if (!is.null(rownames(dict)) && ncol(dict) == 1 && any(grepl("^[a-z]", rownames(dict), TRUE))) {
      dict <- rownames(dict)
    } else {
      su <- vapply(seq_len(ncol(dict)), function(col) !is.numeric(dict[, col]), TRUE)
      if (!any(su)) stop("no terms found in dictionary")
      dict <- if (sum(su) > 1) {
        su <- which(su)
        if (!is.null(term.weights) && (!is.list(term.weights) || ncol(term.weights) == 1)) {
          if (is.list(term.weights)) term.weights <- term.weights[, 1]
          ssu <- vapply(su, function(col) length(unique(dict[, col])), 0) + seq(length(su), 1)
          term.weights <- split(term.weights, dict[, which.min(ssu)])
          dict <- split(dict[, which.max(ssu)], dict[, which.min(ssu)])
        } else {
          ssu <- vapply(su, function(col) anyDuplicated(dict[, col]) == 0, TRUE)
          if (any(ssu)) dict[, su[ssu][1]] else dict[, su[1]]
        }
      } else {
        dict[, su]
      }
    }
  }
  if (is.numeric(dict) && is.null(term.weights)) {
    term.weights <- dict
    dict <- names(dict)
  }
  if (is.factor(dict)) dict <- as.character(dict)
  if (!is.null(dim(term.weights))) {
    if (is.null(colnames(term.weights))) colnames(term.weights) <- if (length(dict) == length(term.weights)) names(dict) else paste0("cat", seq_len(ncol(term.weights)))
    if (!is.data.frame(term.weights)) term.weights <- as.data.frame(term.weights, stringsAsFactors = FALSE)
    su <- vapply(term.weights, is.numeric, TRUE)
    if (any(!su)) {
      if (any(ssu <- !su & vapply(term.weights, anyDuplicated, 0) == 0)) {
        rownames(term.weights) <- term.weights[, which(ssu)[1]]
      }
      term.weights <- term.weights[, su]
    }
    if (!length(term.weights)) stop("no numeric columns in term.weights")
  }
  if (!is.list(dict)) {
    dict <- if (is.matrix(dict)) {
      as.data.frame(dict, stringsAsFactors = FALSE)
    } else if (is.character(dict) && length(dict) == 1 && (file.exists(dict) || dict %in% rownames(select.dict()$info))) {
      read.dic(dict, dir = if (ckd) "" else dir)
    } else {
      list(dict)
    }
  }
  if (is.list(dict)) {
    if (is.null(names(dict))) {
      tn <- if (!is.null(colnames(term.weights))) colnames(term.weights) else names(term.weights)
      names(dict) <- if (!is.null(tn) && length(tn) == length(dict)) tn else paste0("cat", seq_along(dict))
    } else if (any(su <- names(dict) == "")) {
      names(dict)[su] <- if (sum(su) == 1) "cat_unnamed" else paste0("cat_unnamed", seq_len(sum(su)))
      if (!is.null(term.weights) && any(su <- names(term.weights) == "")) {
        names(term.weights)[su] <- if (sum(su) == 1) "cat_unnamed" else paste0("cat_unnamed", seq_len(sum(su)))
      }
    }
  } else {
    dict <- list(dict)
  }
  if (!is.null(term.weights)) {
    if (is.null(dim(term.weights))) {
      if (is.list(term.weights)) {
        if (length(dict) != length(term.weights) && !is.null(names(term.weights[[1]]))) dict <- term.weights
        if (length(dict) == length(term.weights) && !all(names(dict) %in% names(term.weights))) {
          if (is.null(names(term.weights)) || !any(names(term.weights) %in% names(dict))) {
            names(term.weights) <- names(dict)
          } else {
            for (cat in names(dict)[!names(dict) %in% names(term.weights)]) {
              term.weights[cat] <- structure(rep(1, length(dict[[cat]])), names = dict[[cat]])
            }
          }
        }
        for (cat in names(dict)) {
          if (is.null(names(term.weights[[cat]]))) {
            if (length(term.weights[[cat]]) == length(dict[[cat]])) {
              names(term.weights[[cat]]) <- dict[[cat]]
            } else {
              term.weights[[cat]] <- structure(rep(1, length(dict[[cat]])), names = dict[[cat]])
            }
          }
        }
      } else {
        if (is.null(names(term.weights))) {
          if (length(dict[[1]]) == length(term.weights)) {
            term.weights <- list(term.weights)
            names(term.weights) <- names(dict)
            names(term.weights[[1]]) <- dict[[1]]
          } else {
            term.weights <- NULL
            warning("term.weights were dropped as they could not be aligned with dict")
          }
        }
      }
    } else {
      if (length(dict) == 1 && length(dict[[1]]) == nrow(term.weights) &&
        !any(grepl("[a-z]", rownames(term.weights), TRUE))) {
        if (is.factor(dict[[1]])) dict[[1]] <- as.character(dict[[1]])
        if (anyDuplicated(dict[[1]])) {
          dt <- unique(dict[[1]][duplicated(dict[[1]])])
          su <- dict[[1]] %in% dt
          td <- term.weights[su, ]
          tw <- matrix(0, length(dt), ncol(term.weights), dimnames = list(dt, colnames(term.weights)))
          for (term in dt) tw[term, ] <- colMeans(term.weights[dict[[1]] == term, , drop = FALSE], na.rm = TRUE)
          term.weights <- rbind(term.weights[!su, ], tw)
          rownames(term.weights) <- c(dict[[1]][!su], dt)
          dict[[1]] <- rownames(term.weights)
        } else {
          rownames(term.weights) <- dict[[1]]
        }
      }
    }
    if (!is.null(term.weights)) {
      if (!is.list(term.weights)) term.weights <- list(term.weights)
      dlen <- length(dict)
      if (is.null(names(term.weights))) {
        names(term.weights) <- if (length(term.weights) == dlen) names(dict) else seq_along(term.weights)
      }
      if (length(term.weights) > dlen && dlen == 1 && all(vapply(term.weights, length, 0) == length(dict[[1]]))) {
        dict <- lapply(term.weights, function(ws) dict[[1]])
      }
    }
  }
  dict <- lapply(dict, function(cat) {
    if (!is.character(cat)) {
      if (is.null(names(cat))) as.character(cat) else names(cat)
    } else {
      cat
    }
  })
  if (!is.null(bias) && is.null(names(bias))) {
    names(bias) <- if (length(bias) == length(dict)) names(dict) else seq_along(bias)
  }
  if (!is.null(names(term.weights)) && length(names(term.weights)) == length(dict)) names(dict) <- names(term.weights)
  for (n in names(dict)) {
    if (!n %in% names(bias) && any(ii <- !is.na(dict[[n]]) & dict[[n]] == bias.name)) {
      bias[n] <- term.weights[[n]][ii]
      term.weights[[n]][ii] <- 0
    }
  }
  dict_chars <- list(
    all = paste(unique(strsplit(paste0(unique(unlist(dict, use.names = FALSE)), collapse = ""), "")[[1]]),
      collapse = ""
    )
  )
  dict_chars$alpha <- gsub("[^A-Za-z]", "", dict_chars$all)
  dict_chars$case <- if (grepl("[A-Z]", dict_chars$alpha)) {
    if (grepl("[a-z]", dict_chars$alpha)) "mixed" else "upper"
  } else {
    "lower"
  }
  edtm <- substitute(dtm)
  if (is.factor(dtm)) dtm <- as.character(dtm)
  if (is.character(dtm) || !any(grepl("\\s", colnames(dtm)))) {
    if (any(grepl("\\s", unlist(dict, use.names = FALSE)))) {
      if (is.character(dtm)) {
        warning(
          "dict has terms with spaces, so using lma_patcat instead;",
          "\n  enter a dtm (e.g., lma_dtm(", paste0(edtm, collapse = ""), ")) to force lma_termcat use"
        )
        args <- list(text = dtm, dict = dict)
        if (!is.null(term.weights)) args$pattern.weights <- term.weights
        if (!is.null(bias)) args$bias <- bias
        if (!missing(glob)) args$globtoregex <- glob
        if (!missing(partial) && !partial) args$boundary <- "\\b"
        if (!missing(dir)) args$dir <- if (ckd) "" else dir
        return(do.call(lma_patcat, args))
      }
    }
    if (is.character(dtm)) {
      if (dict_chars$case == "upper") dtm <- toupper(dtm)
      dtm <- lma_dtm(dtm,
        numbers = grepl("[0-9]", dict_chars$all), punct = grepl('[_/\\?!."-]', dict_chars$all),
        to.lower = dict_chars$case == "lower"
      )
    }
  }
  if (is.null(dim(dtm))) dtm <- t(dtm)
  ats <- attributes(dtm)[c("opts", "WC", "type")]
  ats <- ats[!vapply(ats, is.null, TRUE)]
  atsn <- names(ats)
  ws <- if (is.null(term.filter)) colnames(dtm) else gsub(term.filter, "", colnames(dtm), perl = TRUE)
  if ((missing(to.lower) || !is.logical(to.lower)) && dict_chars$case != "mixed") {
    text_case <- if (any(grepl("[A-Z]", ws))) if (any(grepl("[a-z]", ws))) "mixed" else "upper" else "lower"
    if (text_case == "upper") {
      dict <- lapply(dict, toupper)
      dict_chars$case <- "upper"
    }
    to.lower <- text_case == "lower"
  }
  if (to.lower && dict_chars$case != "lower") {
    dict <- lapply(dict, tolower)
    dict_chars$case <- "lower"
  }
  if (dict_chars$case != "mixed") ws <- (if (dict_chars$case == "lower") tolower else toupper)(ws)
  odict <- dict
  boundaries <- FALSE
  formatdict <- function(dict, collapse = "|") {
    lab <- if (!escape) {
      lab <- lapply(dict, function(l) {
        if (!any(grepl("[][)(}{]", l))) {
          return(FALSE)
        }
        sl <- strsplit(l, "")
        !any(grepl("\\[.+\\]|\\(.+\\)|\\{.+\\}", l)) || any(vapply(
          sl, function(cs) {
            sum(sl == "[") != sum(sl == "]") &
              sum(sl == "{") != sum(sl == "}") &
              sum(sl == "(") != sum(sl == ")")
          },
          TRUE
        ))
      })
      Filter(isTRUE, lab)
    } else {
      logical()
    }
    if (!partial) {
      s <- "^"
      e <- "$"
    } else {
      s <- e <- ""
    }
    rec <- "([][)(}{*.^$+?\\|\\\\])"
    if (length(lab)) {
      for (l in names(lab)) dict[[l]][lab[[l]]] <- gsub("([][)(}{])", "\\\\\\1", dict[[l]][lab[[l]]])
      rec <- "([*.^$+?\\|])"
    }
    res <- if (escape) {
      lapply(dict, function(l) {
        paste0(s, gsub(rec, "\\\\\\1", l, perl = TRUE), e, collapse = collapse)
      })
    } else {
      lapply(dict, function(l) paste(paste0(s, gsub("([+*])[+*]+", "\\\\\\1+", l), e), collapse = collapse))
    }
    if (glob) {
      lapply(res, function(l) {
        gsub(paste0(
          if (s == "^") "\\" else "", s, if (escape) "\\\\" else "", "\\*|", if (escape) "\\\\" else "", "\\*", if (e == "$") {
            "\\"
          } else {
            ""
          }, e
        ), "", l)
      })
    } else {
      res
    }
  }
  for (l in dict) {
    if (!boundaries) boundaries <- !any(grepl("^\\*|\\*$", l)) && any(grepl("^\\^|\\$$", l))
    if (missing(partial) && boundaries) partial <- TRUE
    if (missing(glob) && (any(grepl("([][}{.^$+?\\|\\\\])", l)) || any(grepl("\\w\\*\\w", l)))) glob <- FALSE
    if (missing(escape) && (boundaries || any(grepl("[.])][+*]|[.+*]\\?|\\[\\^", l))) &&
      !any(grepl("[({[][^])}]*$|^[^({[]*[])}]", l))) {
      escape <- FALSE
    }
  }
  cls <- 0
  if (is.null(term.weights)) {
    cls <- structure(numeric(length(dict)), names = names(dict))
    for (cat in seq_along(dict)) {
      ccls <- tryCatch(nchar(dict[[cat]]), error = function(e) NULL)
      if (is.null(ccls)) {
        warning(
          "dict appears to be misencoded, so results may not be as expected;\n",
          'might try reading the dictionary in with encoding = "latin1"'
        )
        dict[[cat]] <- iconv(dict[[cat]], sub = "#")
        ccls <- nchar(dict[[cat]])
      }
      cls[cat] <- sum(ccls)
    }
  }
  if (any(cls > term.break)) {
    br <- function(l, e = term.break) {
      f <- ceiling(cls[[l]] / e)
      l <- length(dict[[l]])
      e <- ceiling(l / f)
      o <- lapply(seq_len(f), function(i) seq_len(e) + e * (i - 1))
      o[[f]] <- o[[f]][o[[f]] <= l]
      o
    }
    op <- matrix(0, nrow(dtm), length(dict), dimnames = list(rownames(dtm), names(dict)))
    if (coverage) cop <- op
    for (cat in names(dict)) {
      matches <- if (cls[[cat]] > term.break) {
        unique(unlist(lapply(br(cat), function(s) {
          grep(formatdict(list(dict[[cat]][s]))[[1]], ws, perl = TRUE)
        })))
      } else {
        grep(formatdict(list(dict[[cat]])), ws, perl = TRUE)
      }
      if (length(matches)) {
        su <- dtm[, matches, drop = FALSE]
        op[, cat] <- rowSums(su, na.rm = TRUE)
        if (coverage) cop[, cat] <- rowSums(su != 0, na.rm = TRUE)
      }
    }
    if (coverage) {
      colnames(cop) <- paste0("coverage_", colnames(op))
      op <- cbind(op, cop)
    }
  } else {
    if (!is.null(term.weights)) {
      dict <- formatdict(dict, NULL)
      terms <- unique(unlist(dict))
      termmap <- lapply(terms, grep, ws, perl = TRUE, value = TRUE)
      names(termmap) <- unique(unlist(odict))
      termmap <- Filter(length, termmap)
      if (is.null(dim(term.weights))) {
        op <- matrix(0, nrow(dtm), length(dict), dimnames = list(rownames(dtm), names(dict)))
        if (length(termmap)) {
          weights <- lapply(names(term.weights), function(n) {
            l <- term.weights[[n]]
            if (is.null(names(l)) && n %in% names(dict) && length(dict[[n]]) == length(l)) {
              names(term.weights[[n]]) <- dict[[n]]
              l <- term.weights[[n]]
            }
            if (any(su <- !names(termmap) %in% names(l))) l[names(termmap)[su]] <- 0
            do.call(c, lapply(names(termmap), function(p) {
              structure(rep(l[[p]], length(termmap[[p]])), names = termmap[[p]])
            }))
          })
          names(weights) <- names(term.weights)
          for (cat in names(dict)) {
            if (length(weights[[cat]])) {
              op[, cat] <- as.numeric(dtm[, names(weights[[cat]]), drop = FALSE] %*% weights[[cat]])
            }
          }
        }
      } else {
        if (length(termmap)) {
          weights <- do.call(rbind, lapply(names(termmap), function(p) {
            matrix(
              rep(as.numeric(term.weights[p, ]), length(termmap[[p]])),
              ncol = ncol(term.weights), dimnames = list(termmap[[p]], colnames(term.weights))
            )
          }))
          op <- matrix(0, nrow(dtm), ncol(weights), dimnames = list(rownames(dtm), colnames(weights)))
          for (cat in colnames(op)) {
            op[, cat] <- as.numeric(dtm[, rownames(weights), drop = FALSE] %*% weights[, cat])
          }
        } else {
          op <- matrix(0, nrow(dtm), length(dict), dimnames = list(rownames(dtm), colnames(weights)))
        }
      }
    } else {
      dict <- formatdict(dict)
      if (coverage) {
        op <- vapply(names(dict), function(cat) {
          su <- dtm[, grep(dict[[cat]], ws, perl = TRUE), drop = FALSE]
          c(rowSums(su != 0, na.rm = TRUE), rowSums(su, na.rm = TRUE))
        }, numeric(nrow(dtm) * 2))
        cop <- op[seq_len(nrow(dtm)), , drop = FALSE]
        colnames(cop) <- paste0("coverage_", names(dict))
        op <- cbind(op[-seq_len(nrow(dtm)), , drop = FALSE], cop)
      } else {
        op <- vapply(names(dict), function(cat) {
          rowSums(dtm[, grep(dict[[cat]], ws, perl = TRUE),
            drop = FALSE
          ], na.rm = TRUE)
        }, numeric(nrow(dtm)))
      }
      if (nrow(dtm) == 1) {
        op <- t(op)
        rownames(op) <- 1
      }
    }
  }
  if (!is.null(bias)) for (n in names(bias)) if (n %in% colnames(op)) op[, n] <- op[, n] + bias[[n]]
  attr(op, "WC") <- if ("WC" %in% atsn) {
    ats$WC
  } else if (all(vapply(seq_len(ncol(dtm)), function(i) {
    is.numeric(dtm[, i]) || is.integer(dtm[, i])
  }, TRUE))) {
    rowSums(dtm, na.rm = TRUE)
  } else {
    NULL
  }
  attr(op, "time") <- c(attr(dtm, "time"), termcat = proc.time()[[3]] - st)
  if ("type" %in% atsn) attr(op, "type") <- ats$type
  op
}
