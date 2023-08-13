#' Document-Term Matrix Creation
#'
#' Creates a document-term matrix (dtm) from a set of texts.
#' @param text Texts to be processed. This can be a vector (such as a column in a data frame)
#'   or list. When a list, these can be in the form returned with \code{tokens.only = TRUE},
#'   or a list with named vectors, where names are tokens and values are frequencies or the like.
#' @param exclude A character vector of words to be excluded. If \code{exclude} is a single string
#'   matching \code{'function'}, \code{lma_dict(1:9)} will be used.
#' @param context A character vector used to reformat text based on look- ahead/behind. For example,
#'   you might attempt to disambiguate \emph{like} by reformatting certain \emph{like}s
#'   (e.g., \code{context = c('(i) like*', '(you) like*', '(do) like')}, where words in parentheses are
#'   the context for the target word, and asterisks denote partial matching). This would be converted
#'   to regular expression (i.e., \code{'(? <= i) like\\\\b'}) which, if matched, would be
#'   replaced with a coded version of the word (e.g., \code{"Hey, i like that!"} would become
#'   \code{"Hey, i i-like that!"}). This would probably only be useful for categorization, where a
#'   dictionary would only include one or another version of a word (e.g., the LIWC 2015 dictionary
#'   does something like this with \emph{like}, and LIWC 2007 did something like this with
#'   \emph{kind (of)}, both to try and clean up the posemo category).
#' @param replace.special Logical: if \code{TRUE}, special characters are replaced with regular
#'   equivalents using the \code{\link{lma_dict}} special function.
#' @param numbers Logical: if \code{TRUE}, numbers are preserved.
#' @param punct Logical: if \code{TRUE}, punctuation is preserved.
#' @param urls Logical: if \code{FALSE}, attempts to replace all urls with "repurl".
#' @param emojis Logical: if \code{TRUE}, attempts to replace emojis (e.g., ":(" would be replaced
#'   with "repfrown").
#' @param to.lower Logical: if \code{FALSE}, words with different capitalization are treated as
#'   different terms.
#' @param word.break A regular expression string determining the way words are split. Default is
#'   \code{' +'} which breaks words at one or more blank spaces. You may also like to break by
#'   dashes or slashes (\code{'[ /-]+'}), depending on the text.
#' @param dc.min Numeric: excludes terms appearing in the set number or fewer documents.
#'   Default is 0 (no limit).
#' @param dc.max Numeric: excludes terms appearing in the set number or more. Default
#'   is Inf (no limit).
#' @param sparse Logical: if \code{FALSE}, a regular dense matrix is returned.
#' @param tokens.only Logical: if \code{TRUE}, returns a list rather than a matrix, with these entries:
#'   \tabular{ll}{
#'     \code{tokens} \tab A vector of indices with terms as names. \cr
#'     \code{frequencies} \tab A vector of counts with terms as names. \cr
#'     \code{WC} \tab A vector of term counts for each document. \cr
#'     \code{indices} \tab A list with a vector of token indices for each document. \cr
#'   }
#' @note
#' This is a relatively simple way to make a dtm. To calculate the (more or less) standard forms of
#' LSM and LSS, a somewhat raw dtm should be fine, because both processes essentially use
#' dictionaries (obviating stemming) and weighting or categorization (largely obviating 'stop word'
#' removal). The exact effect of additional processing will depend on the dictionary/semantic space
#' and weighting scheme used (particularly for LSA). This function also does some processing which
#' may matter if you plan on categorizing with categories that have terms with look- ahead/behind assertions
#' (like LIWC dictionaries). Otherwise, other methods may be faster, more memory efficient, and/or more featureful.
#' @return A sparse matrix (or regular matrix if \code{sparse = FALSE}), with a row per \code{text},
#' and column per term, or a list if \code{tokens.only = TRUE}. Includes an attribute with options (\code{opts}),
#' and attributes with word count (\code{WC}) and column sums (\code{colsums}) if \code{tokens.only = FALSE}.
#' @examples
#' text <- c(
#'   "Why, hello there! How are you this evening?",
#'   "I am well, thank you for your inquiry!",
#'   "You are a most good at social interactions person!",
#'   "Why, thank you! You're not all bad yourself!"
#' )
#'
#' lma_dtm(text)
#'
#' # return tokens only
#' (tokens <- lma_dtm(text, tokens.only = TRUE))
#'
#' ## convert those to a regular DTM
#' lma_dtm(tokens)
#'
#' # convert a list-representation to a sparse matrix
#' lma_dtm(list(
#'   doc1 = c(why = 1, hello = 1, there = 1),
#'   doc2 = c(i = 1, am = 1, well = 1)
#' ))
#' @export

lma_dtm <- function(text, exclude = NULL, context = NULL, replace.special = FALSE, numbers = FALSE,
                    punct = FALSE, urls = TRUE, emojis = FALSE, to.lower = TRUE, word.break = " +", dc.min = 0,
                    dc.max = Inf, sparse = TRUE, tokens.only = FALSE) {
  if (!is.null(dim(text))) {
    if (is.character(text[, 1]) || is.factor(text[, 1])) {
      text <- text[, 1]
    } else {
      stop("enter a vector of texts as the first argument")
    }
  }
  if (is.list(text)) {
    if (all(c("tokens", "indices") %in% names(text))) {
      m <- do.call(rbind, lapply(seq_along(text$indices), function(i) {
        if (length(text$indices[[i]])) {
          inds <- as.factor(text$indices[[i]])
          cbind(i, as.integer(levels(inds)), tabulate(inds))
        }
      }))
      dtm <- sparseMatrix(m[, 1], m[, 2],
        x = m[, 3], dims = c(length(text$indices), length(text$tokens)),
        dimnames = list(NULL, if (is.character(text$tokens)) text$tokens else names(text$tokens))
      )
      if (!sparse) dtm <- as.matrix(dtm)
      attr(dtm, "colsums") <- text$frequencies
      attr(dtm, "type") <- "count"
      attr(dtm, "WC") <- text$WC
      attr(dtm, "opts") <- attr(text, "opts")
      attr(dtm, "time") <- attr(text, "time")
      return(dtm)
    } else {
      tokens <- unlist(unname(text), recursive = FALSE)
      cinds <- unique(names(tokens))
      if (is.null(cinds)) {
        text <- unlist(text)
      } else {
        rinds <- rep(seq_along(text), vapply(text, length, 0))
        cinds <- structure(seq_along(cinds), names = cinds)
        dtm <- sparseMatrix(
          rinds, cinds[names(tokens)],
          x = tokens,
          dims = c(length(text), length(cinds)), dimnames = list(names(text), names(cinds))
        )
        if (!sparse) dtm <- as.matrix(dtm)
        return(dtm)
      }
    }
  }
  if (is.null(text)) stop(substitute(text), " not found")
  docnames <- names(text)
  if (is.character(text) && all(nchar(text) < 500) && all(file.exists(text))) {
    text <- if (length(text) != 1 || dir.exists(text)) read.segments(text) else readLines(text)
  }
  text <- paste(" ", text, " ")
  st <- proc.time()[[3]]
  if (replace.special) text <- lma_dict("special", as.function = gsub)(text)
  text <- gsub("(?<=[^a-z0-9])'|'(?=[^a-z0-9])", '"', text, TRUE, TRUE)
  if (!urls) {
    text <- gsub(paste0(
      "\\s[a-z]+://[^\\s]*|www\\.[^\\s]*|\\s[a-z_~-]+\\.[a-z_~-]{2,}[^\\s]*|\\s[a-z_~-]+\\.",
      "(?:io|com|net|org|gov|edu)\\s"
    ), " repurl ", text, TRUE, TRUE)
    text <- gsub("(?<=[A-Z])\\.\\s", " ", text, perl = TRUE)
  }
  text <- gsub("\\s+", " ", text)
  text <- gsub("\\s(etc|st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof)\\.", " \\1tempperiod", text, TRUE)
  text <- gsub("\\s\\.|\\.\\s", " . ", text)
  if (any(punct, emojis, !is.null(context))) {
    special <- lma_dict(special)[[1]]
    if (!missing(context) && length(context) == 1 && grepl("like", context, TRUE)) {
      context <- special[["LIKE"]]
    }
    if (punct) text <- gsub(special[["ELLIPSIS"]], " repellipsis ", text)
    if (emojis) {
      for (type in c("SMILE", "FROWN")) {
        text <- gsub(
          special[[type]], paste0(" rep", tolower(type), " "), text,
          perl = TRUE
        )
      }
    }
    if (!missing(context)) {
      if (!any(grepl("[?=]", context))) {
        context <- gsub("^\\(", "(?<=", context)
        context <- gsub("\\((?!\\?)", "(?=", context, perl = TRUE)
        context <- gsub("(?<![)*])$", "\\\\b", context, perl = TRUE)
        context <- gsub("\\*", "\\\\w*", context, perl = TRUE)
      }
      context <- structure(
        as.list(context),
        names = paste("", gsub("--+", "-", gsub("[\\s^[]|\\\\s]", "-",
          gsub("[^a-z0-9\\s\\\\']|\\\\[wbs]", "", context, TRUE, TRUE),
          perl = TRUE
        )), "")
      )
      for (rn in names(context)) text <- gsub(context[[rn]], rn, text, perl = TRUE)
    }
  }
  if (to.lower) text <- tolower(text)
  if (!is.null(exclude)) {
    if (length(exclude) == 1 && grepl(exclude, "function", TRUE)) {
      exclude <- unlist(lma_dict(), use.names = FALSE)
    } else if (is.list(exclude)) exclude <- unlist(exclude, use.names = FALSE)
  }
  if (!numbers) text <- gsub("[[:punct:]]*[0-9][0-9,.el-]*", " ", text, TRUE, TRUE)
  text <- gsub(paste0(
    "([^a-z0-9.,':/?=#\\s-]|[:/?=#](?=\\s)|(?:(?<=\\s)[:/=-]|,)(?=[a-z])|(?<=[^a-z0-9])",
    "(,(?=[a-z0-9])|[.-](?=[a-z]))|[.,'-](?=[^0-9a-z]|[.,'-]))"
  ), " \\1 ", text, TRUE, TRUE)
  text <- gsub("(\\s[a-z]+)/([a-z]+\\s)", " \\1 / \\2 ", text, TRUE, TRUE)
  text <- gsub("([a-z0-9.,'-].*[^a-z0-9])", " \\1 ", text, TRUE, TRUE)
  text <- gsub("(?<=[a-z])\\s['\u00E7\u00ED]\\s(?=[a-z])", "'", text, TRUE, TRUE)
  if (!punct) {
    text <- gsub("[^A-Za-z0-9'._-]+", " ", text)
    text <- gsub("(?=[a-z])\\.+|(?<=[^a-z0-9])['._-]+|'+\\s", " ", text, TRUE, TRUE)
  }
  text <- gsub("tempperiod", ".", text, fixed = TRUE)
  text <- gsub("^\\s+|\\s(?=\\s)|\\s+$", "", text, perl = TRUE)
  text <- strsplit(text, word.break)
  words <- sort(unique(unlist(text)))
  words <- words[!words == ""]
  if (!is.null(exclude)) {
    if (is.list(exclude)) exclude <- unlist(exclude, use.names = FALSE)
    if (!any(grepl("^", exclude, fixed = TRUE))) exclude <- gsub("\\^\\*|\\*\\$", "", paste0("^", exclude, "$"))
    if (any(ck <- grepl("[[({]", exclude) + grepl("[})]|\\]", exclude) == 1)) {
      exclude[ck] <- gsub("([([{}\\])])", "\\\\\\1", exclude[ck], perl = TRUE)
    }
    words <- grep(paste(exclude, collapse = "|"), words, value = TRUE, invert = TRUE)
  }
  if (tokens.only) {
    m <- match_terms(
      text, words, !grepl("^[[:punct:]]$|^repellipsis$", words),
      c(length(text), length(words)), is.null(exclude), TRUE
    )
    names(m) <- c("tokens", "frequencies", "WC", "indices")
    m$tokens <- m$tokens + 1L
    m$tokens <- sort(m$tokens)
    inds <- vector("list", length(text))
    l <- 0
    for (i in seq_along(inds)) {
      if (m$WC[i]) {
        inds[[i]] <- m$indices[seq_len(m$WC[i]) + l] + 1L
        l <- l + m$WC[i]
      } else {
        inds[[i]] <- integer()
      }
    }
    m$indices <- inds
    if (dc.min > 0 || dc.max < Inf) {
      su <- m$frequencies > dc.min & m$frequencies < dc.max
      if (any(!su)) {
        if (!any(su)) {
          warning(
            "document count bounds [", dc.min, ", ", dc.max, "] exlcuded all terms, so they were ignored",
            call. = FALSE
          )
        } else {
          m$frequencies <- m$frequencies[su]
          ex <- m$tokens[!su]
          m$tokens <- m$tokens[su]
          new_inds <- structure(seq_along(m$tokens), names = m$tokens)
          m$tokens[] <- new_inds
          for (i in seq_along(m$indices)) {
            inds <- m$indices[[i]]
            m$indices[[i]] <- unname(new_inds[as.character(inds[!inds %in% ex])])
            m$WC[i] <- length(m$indices[[i]])
          }
        }
      }
    }
    names(m$frequencies) <- names(m$tokens)
  } else {
    msu <- match_terms(
      text, words, !grepl("^[[:punct:]]$|^repellipsis$", words),
      c(length(text), length(words)), is.null(exclude), FALSE
    )
    m <- if (sparse) as(msu[[1]], "CsparseMatrix") else as.matrix(msu[[1]])
    if (length(docnames) == nrow(m)) rownames(m) <- docnames
    su <- msu[[3]] > dc.min & msu[[3]] < dc.max
    names(msu[[3]]) <- words
    if (any(!su)) {
      if (!any(su)) {
        warning(
          "document count bounds [", dc.min, ", ", dc.max, "] exlcuded all terms, so they were ignored",
          call. = FALSE
        )
      } else {
        m <- m[, su, drop = FALSE]
      }
    }
    attr(m, "WC") <- unlist(msu[[2]], use.names = FALSE)
    attr(m, "colsums") <- msu[[3]]
    attr(m, "type") <- "count"
    if (!missing(dc.min) || !missing(dc.max)) {
      attr(m, "info") <- paste(
        "a lim of", dc.min, "and", dc.max, "left", sum(su), "of", length(words), "unique terms"
      )
    }
  }
  attr(m, "opts") <- c(numbers = numbers, punct = punct, urls = urls, to.lower = to.lower)
  attr(m, "time") <- c(dtm = proc.time()[[3]] - st)
  m
}
