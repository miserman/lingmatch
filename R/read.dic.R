to_regex <- function(dict, intext = FALSE, isGlob = TRUE) {
  lapply(dict, function(l) {
    l <- gsub("([+*])[+*]+", "\\\\\\1+", sub("(?<=[^\\\\])\\\\$", "\\\\\\\\", l, perl = TRUE))
    if (any(ck <- grepl("[[({]", l) + grepl("[})]|\\]", l) == 1)) {
      l[ck] <- gsub("([([{}\\])])", "\\\\\\1", l[ck], perl = TRUE)
    }
    if (isGlob) {
      if (!intext) l <- gsub("\\^\\*|\\*\\$", "", paste0("^", l, "$"))
      l <- gsub("\\*", "[^\\\\s]*", l)
    }
    l
  })
}

#' Read/Write Dictionary Files
#'
#' Read in or write dictionary files in Comma-Separated Values (.csv; weighted) or
#' Linguistic Inquiry and Word Count (.dic; non-weighted) format.
#' @param path Path to a file, a name corresponding to a file in \code{getOption('lingmatch.dict.dir')}
#'   (or \code{'~/Dictionaries'}) or one of the dictionaries available at \href{https://osf.io/y6g5b}{osf.io/y6g5b},
#'   a matrix-like object to be categorized, or a list to be formatted.
#' @param cats A character vector of category names to be returned. All categories are returned by default.
#' @param type A character indicating whether and how terms should be altered. Unspecified or matching 'asis'
#'   leaves terms as they are. Other options change wildcards to regular expressions:
#'   \code{'pattern'} (\code{'^[poi]'}) replaces initial asterisks with \code{'\\\\b\\\\w*'},
#'   and terminal asterisks with \code{'\\\\w*\\\\b'}, to match terms within raw text;
#'   for anything else, terms are padded with \code{^} and \code{$}, then those bounding marks are removed
#'   when an asterisk is present, to match tokenized terms.
#' @param as.weighted Logical; if \code{TRUE}, prevents weighted dictionaries from being converted to
#'   unweighted versions, or converts unweighted dictionaries to a binary weighted version
#'   -- a data.frame with a "term" column of unique terms, and a column for each category.
#' @param dir Path to a folder containing dictionaries, or where you would like dictionaries to be downloaded;
#'   passed to \code{\link{select.dict}} and/or \code{\link{download.dict}}.
#' @param ... Passes arguments to \code{\link{readLines}}.
#' @param term.name,category.name Strings identifying column names in \code{path} containing terms and categories
#'   respectively.
#' @param raw Logical or a character. As logical, indicates if \code{path} should be treated
#'   as a raw dictionary (as might be read in from a .dic file). As a character, replaces \code{path}
#'   as if it were read in from a file.
#' @return \code{read.dic}: A \code{list} (unweighted) with an entry for each category containing
#'   character vectors of terms, or a \code{data.frame} (weighted) with columns for terms (first, "term") and
#'   weights (all subsequent, with category labels as names).
#' @family Dictionary functions
#' @importFrom utils read.table write.table
#' @export

read.dic <- function(path, cats, type = "asis", as.weighted = FALSE, dir = getOption("lingmatch.dict.dir"), ...,
                     term.name = "term", category.name = "category", raw = FALSE) {
  if (ckd <- dir == "") dir <- "~/Dictionaries"
  if (!is.logical(raw)) {
    path <- raw
    raw <- TRUE
  }
  if (missing(path)) path <- file.choose()
  if (!raw) {
    if (is.character(path) && !any(file.exists(path)) &&
      any(file.exists(normalizePath(paste0(dir, "/", path), "/", FALSE)))) {
      path <- normalizePath(paste0(dir, "/", path), "/", FALSE)
    }
    if (is.character(path) && !any(file.exists(path))) {
      tp <- select.dict(path, dir = if (ckd) "" else dir)
      if (nrow(tp$selected) && length(path) <= nrow(tp$info)) {
        if (any(tp$selected$downloaded == "")) {
          td <- rownames(tp$selected)[tp$selected$downloaded == ""]
          if (!ckd && grepl("^$|^[yt1]|^ent", readline(paste0(
            "would you like to download ", if (length(td) == 1) "this dictionary" else "these dictionaries", "?:\n",
            sub(",(?=[^,]+$)", if (length(td) == 2) " and" else ", and", paste0(td, collapse = ", "), perl = TRUE),
            "\n(press Enter for yes): "
          )))) {
            tp$selected[td, "downloaded"] <- unlist(download.dict(td, dir = dir), use.names = FALSE)
          }
        }
        path <- tp$selected[tp$selected[, "downloaded"] != "", "downloaded"]
        if (!length(path)) {
          stop(
            if (nrow(tp$selected) == 1) "dictionary" else "dictionaries", " (",
            paste(rownames(tp$selected), collapse = ", "), ") not found in dir (", dir, ")",
            if (ckd) '\nspecify a directory (e.g., dir = "~") to locate or download; see ?download.dict',
            call. = FALSE
          )
        }
      }
    }
    if (is.character(path) && length(path) > 1 && any(file.exists(path))) {
      dicts <- list()
      for (p in path) {
        if (file.exists(p)) {
          name <- gsub("^.*[/\\]+|\\.[^.]+$", "", p)
          dicts[[name]] <- read.dic(p)
        }
      }
      path <- if (length(dicts) == 1) dicts[[1]] else dicts
    }
  }
  if (!is.null(dim(path))) {
    if (anyNA(path)) path[is.na(path)] <- 0
    cats <- colnames(path)
    if (term.name %in% cats) {
      terms <- path[, term.name]
      cats <- cats[cats != term.name]
    } else if (!is.null(rownames(path)) && any(grepl("[a-z]", rownames(path), TRUE))) {
      terms <- rownames(path)
    } else {
      su <- which(vapply(cats, function(cat) !is.numeric(path[, cat]), TRUE))
      if (!length(su)) {
        if (!is.null(colnames(path))) {
          path <- data.frame(term = colnames(path), t(path), stringsAsFactors = FALSE)
          terms <- path$term
          cats <- colnames(path)[-1]
          if (missing(as.weighted)) as.weighted <- TRUE
        } else {
          stop("no non-numeric columns found in path")
        }
      } else {
        if (length(su) > 1) {
          ssu <- vapply(su, function(col) {
            if (!anyDuplicated(path[, col])) {
              1
            } else
            if (all(path[, col] == path[1, col])) 0 else 2
          }, 0)
          if (length(su) == ncol(path) && !any(ssu == 0)) {
            path <- data.frame(
              term = unlist(path[, su], use.names = FALSE),
              category = rep(colnames(path), each = nrow(path))
            )
            category.name <- cats <- "category"
            su <- 1
          } else {
            if (any(ssu == 2)) {
              cats <- colnames(path)[su[which(ssu == 2)]]
              if (length(cats) > 1 && length(su) != ncol(path)) cats <- cats[1]
            }
            su <- if (any(ssu == 1)) su[which(ssu == 1)[[1]]] else su[[1]]
          }
        }
        terms <- path[, su]
      }
    }
    if (category.name %in% colnames(path)) {
      su <- which(vapply(cats, function(cat) is.numeric(path[, cat]), TRUE))
      cats <- path[, category.name]
      if (length(su) == 1) {
        weights <- path[, names(su)]
        wl <- data.frame(term = terms)
        v <- numeric(nrow(path))
        for (cat in unique(cats)) {
          su <- cats == cat
          v[su] <- weights[su]
          wl[, cat] <- v
          v[] <- 0
        }
      } else {
        wl <- split(terms, cats)
      }
    } else {
      su <- vapply(cats, function(col) {
        if (is.numeric(path[, col])) {
          1
        } else if (anyDuplicated(path[, col])) {
          if (!all(path[, col] == path[1, col])) 2 else 3
        } else {
          0
        }
      }, 0)
      wl <- if (!1 %in% su && any(su == 2) && any(su != 2)) {
        cats <- cats[su == 2]
        wl <- lapply(cats, function(cat) split(terms, path[, cat]))
        names(wl) <- cats
        wl
      } else {
        if (!any(su == 1)) {
          if (any(su > 1)) {
            cats <- cats[su > 1]
            if (length(cats) == 1) {
              split(terms, path[, cats])
            } else {
              wl <- lapply(cats, function(cat) split(terms, path[, cat]))
              names(wl) <- cats
              unlist(wl, FALSE)
            }
          } else {
            stop("no numeric columns found in path")
          }
        } else if (as.weighted) {
          cbind(term = terms, path[, cats[su == 1], drop = FALSE])
        } else {
          cats <- cats[su == 1]
          if (length(cats) == 1) {
            weights <- path[, cats]
            if (any(weights < 0) && any(weights > 0)) {
              Filter(length, list(
                positive = terms[weights > 0],
                neutral = terms[weights == 0],
                negative = terms[weights < 0]
              ))
            } else if (anyDuplicated(weights)) split(terms, weights) else list(category = terms)
          } else {
            weights <- as.data.frame(path[, cats], stringsAsFactors = FALSE)
            if (any(weights > 0) && any(weights < 0)) {
              for (cat in cats) {
                if (any(weights[, cat] > 0)) weights[, paste0(cat, ".positive")] <- as.integer(weights[, cat] > 0)
                if (any(weights[, cat] == 0)) weights[, paste0(cat, ".neutral")] <- as.integer(weights[, cat] == 0)
                if (any(weights[, cat] < 0)) weights[, paste0(cat, ".negative")] <- as.integer(weights[, cat] < 0)
                weights <- weights[, colnames(weights) != cat]
              }
              cats <- sort(colnames(weights))
            }
            lvs <- sort(unique(unlist(weights)))
            if (length(lvs) == 2 && all(lvs == c(0, 1))) {
              wl <- lapply(cats, function(cat) terms[weights[, cat] == 1])
              names(wl) <- cats
              wl <- Filter(length, wl)
            } else {
              wl <- split(terms, colnames(weights)[max.col(weights, "first")])
            }
            wl
          }
        }
      }
    }
  } else if (is.list(path)) {
    path <- Filter(length, path)
    if (all(vapply(path, function(d) is.character(d) || is.factor(d), TRUE))) {
      wl <- path
      if (is.null(names(wl))) names(wl) <- paste0("cat", seq_along(wl))
      if (!missing(cats)) {
        wl <- wl[names(wl) %in% cats]
        if (!length(wl)) stop("no cats were found in path")
      }
    } else {
      if (is.null(names(path))) names(path) <- paste0("dict", seq_along(path))
      wl <- if (any(vapply(path, function(d) !is.null(dim(d)), TRUE))) {
        terms <- NULL
        cats <- NULL
        for (d in names(path)) {
          path[[d]] <- read.dic(path[[d]], as.weighted = as.weighted)
          if (as.weighted) {
            terms <- unique(c(terms, path[[d]]$term))
            cats <- c(cats, paste0(d, ".", colnames(path[[d]])[-1]))
          }
        }
        if (as.weighted) {
          wl <- as.data.frame(
            matrix(0, length(terms), length(cats), dimnames = list(terms, cats)),
            stringsAsFactors = FALSE
          )
          for (d in names(path)) {
            cats <- colnames(path[[d]])[-1] <- paste0(d, ".", colnames(path[[d]])[-1])
            su <- duplicated(path[[d]]$term)
            if (any(su)) {
              su <- path[[d]]$term %in% path[[d]]$term[su]
              td <- path[[d]][su, , drop = FALSE]
              for (term in unique(td$term)) wl[term, cats] <- colMeans(td[td$term == term, cats])
            }
            if (any(!su)) path[[d]] <- path[[d]][!su, ]
            rownames(path[[d]]) <- path[[d]]$term
            wl[path[[d]]$term, cats] <- path[[d]][, cats]
          }
          data.frame(term = rownames(wl), wl, stringsAsFactors = FALSE)
        } else {
          unlist(path, FALSE)
        }
      } else if (any(vapply(path, is.list, TRUE))) {
        unlist(path, FALSE)
      } else {
        if (any(vapply(path, function(d) is.null(names(d)), TRUE))) {
          if (all(vapply(path, length, 0) == length(path[[1]]))) {
            data.frame(term = names(path), do.call(rbind, path), stringsAsFactors = FALSE)
          } else {
            stop("failed to resolve path; as a list, entries should contain character or named numeric vectors")
          }
        } else {
          terms <- unique(unlist(lapply(path, names), use.names = FALSE))
          v <- structure(numeric(length(terms)), names = terms)
          data.frame(term = terms, vapply(path, function(d) {
            v[names(d)] <- d
            v
          }, numeric(length(terms))), stringsAsFactors = FALSE)
        }
      }
    }
  } else {
    if (raw || length(path) != 1) {
      if (length(path) == 1) path <- strsplit(path, "\n")[[1]]
      di <- path
    } else {
      di <- tryCatch(readLines(path, warn = FALSE, ...), error = function(e) NULL)
      if (is.null(di)) stop("assumed path (", path, ") is to a file, but failed to read it in", call. = FALSE)
    }
    lst <- grep("%", di, fixed = TRUE)
    if (length(lst) > 1 && !grepl(",", di[lst[1]], fixed = TRUE)) {
      if (length(lst) < 2) {
        stop(
          "could not identify the end of the header -- ",
          "this should be the second percent sign (%) following the last category name."
        )
      }
      lst <- lst[2]
      h <- grep("^\\d", gsub("^\\s+|\\s*%+\\s*|\\s+$", "", di[seq_len(lst)]), value = TRUE)
      ci <- character()
      for (cat in h) ci[sub("\\s.*$", "", cat)] <- sub("^[^\\s]+\\s+", "", cat, perl = TRUE)
      if (missing(cats)) cats <- ci
      sep <- if (grepl("\t", di[lst + 1], fixed = TRUE)) "\t" else "\\s"
      cb <- paste0("(?:", sep, "+(?:", paste(names(ci), collapse = "|"), ")(?=", sep, "|$))*$")
      di <- di[-seq_len(lst - 1)]
      wl <- lapply(structure(names(ci), names = ci), function(cat) {
        unique(sub(cb, "", di[grep(paste0(sep, cat, cb), di, perl = TRUE)], perl = TRUE))
      })
      wl <- wl[cats[cats %in% names(wl)]]
    } else {
      if (missing(as.weighted) && length(path) == 1) as.weighted <- TRUE
      wl <- if (any(grepl("[\\s,]", di, perl = TRUE))) {
        di <- read.table(
          text = di, header = TRUE, sep = if (grepl("\t", di[[1]])) "\t" else ",",
          quote = '"', comment.char = "", stringsAsFactors = FALSE
        )
        if (!missing(as.weighted) || (!term.name %in% colnames(di) && !any(vapply(di, is.character, TRUE)) &&
          !any(grepl("[a-z]", rownames(di), TRUE)))) {
          di <- tryCatch(
            read.dic(di, cats = cats, type = type, as.weighted = as.weighted),
            error = function(e) e$message
          )
        }
        di
      } else {
        list(cat1 = di)
      }
      if (length(wl) == 1 && is.character(wl)) {
        stop("assuming path is to a comma separated values file, but failed to read it in:\n", wl)
      }
    }
  }
  if (!missing(type) && !grepl("^[Aa]", type)) wl <- to_regex(wl, grepl("^[poi]", type, TRUE))
  if (as.weighted && is.null(dim(wl))) {
    op <- data.frame(term = unique(unlist(wl)), stringsAsFactors = FALSE)
    for (cat in names(wl)) op[, cat] <- as.integer(op$term %in% wl[[cat]])
    op
  } else {
    wl
  }
}

#' @rdname read.dic
#' @param dict A \code{list} with a named entry of terms for each category, or a \code{data.frame}
#'   with terms in one column, and categories or weights in the rest.
#' @param filename The name of the file to be saved.
#' @param save Logical: if \code{FALSE}, does not write a file.
#' @return \code{write.dic}: A version of the written dictionary -- a raw character vector for
#'   unweighted dictionaries, or a \code{data.frame} for weighted dictionaries.
#' @examples
#' # make a small murder related dictionary
#' dict <- list(
#'   kill = c("kill*", "murd*", "wound*", "die*"),
#'   death = c("death*", "dying", "die*", "kill*")
#' )
#'
#' # convert it to a weighted format
#' (dict_weighted <- read.dic(dict, as.weighted = TRUE))
#'
#' # categorize it back
#' read.dic(dict_weighted)
#'
#' # convert it to a string without writing to a file
#' cat(raw_dict <- write.dic(dict, save = FALSE))
#'
#' # parse it back in
#' read.dic(raw = raw_dict)
#'
#' \dontrun{
#'
#' # save it as a .dic file
#' write.dic(dict, "murder")
#'
#' # read it back in as a list
#' read.dic("murder.dic")
#'
#' # read in the Moral Foundations or LUSI dictionaries from urls
#' moral_dict <- read.dic("https://osf.io/download/whjt2")
#' lusi_dict <- read.dic("https://www.depts.ttu.edu/psy/lusi/files/lusi_dict.txt")
#'
#' # save and read in a version of the General Inquirer dictionary
#' inquirer <- read.dic("inquirer", dir = "~/Dictionaries")
#' }
#' @export

write.dic <- function(dict, filename, type = "asis", as.weighted = FALSE, save = TRUE) {
  if (!is.list(dict) || is.data.frame(dict)) {
    if (save && (missing(as.weighted) || as.weighted)) {
      as.weighted <- TRUE
      o <- dict
    } else {
      dict <- read.dic(dict)
    }
  }
  if (is.null(dim(dict))) {
    terms <- unique(as.character(unlist(dict, use.names = FALSE)))
    terms <- terms[terms != ""]
    if (!missing(type) && !grepl("^[Aa]", type)) dict <- to_regex(dict, grepl("^[poi]", type, TRUE))
    if (as.weighted) {
      o <- data.frame(term = terms, stringsAsFactors = FALSE)
      for (cat in names(dict)) o[, cat] <- as.integer(o$term %in% dict[[cat]])
    } else {
      l <- length(dict)
      m <- as.data.frame(matrix("", length(terms) + l + 2, l + 1), stringsAsFactors = FALSE)
      m[, 1] <- c("%", seq_len(l), "%", terms)
      m[seq_len(l) + 1, 2] <- if (is.null(names(dict))) seq_len(l) else names(dict)
      for (i in seq_along(dict)) m[which(m[-seq_len(l + 2), 1] %in% dict[[i]]) + l + 2, i + 1] <- i
      o <- gsub("\t{2,}", "\t", paste(sub("\t+$", "", do.call(paste, c(m, sep = "\t"))), collapse = "\n"))
    }
  }
  if (save) {
    filename <- filename[[1]]
    if (!grepl("\\.[^.]+$", filename)) filename <- paste0(filename, if (as.weighted) ".csv" else ".dic")
    if (as.weighted) {
      write.table(o, filename, sep = ",", row.names = FALSE, qmethod = "double")
    } else {
      write(o, filename)
    }
    message("dictionary saved to ", filename)
  }
  invisible(o)
}
