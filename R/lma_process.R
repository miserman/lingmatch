#' Process Text
#'
#' A wrapper to other pre-processing functions, potentially from \code{\link{read.segments}}, to \code{\link{lma_dtm}}
#' or \code{\link{lma_patcat}}, to \code{\link{lma_weight}}, then \code{\link{lma_termcat}} or \code{\link{lma_lspace}},
#' and optionally including \code{\link{lma_meta}} output.
#'
#' @param input A vector of text, or path to a text file or folder.
#' @param ... arguments to be passed to \code{\link{lma_dtm}}, \code{\link{lma_patcat}}, \code{\link{lma_weight}},
#'   \code{\link{lma_termcat}}, and/or \code{\link{lma_lspace}}. All arguments must be named.
#' @param meta Logical; if \code{FALSE}, metastatistics are not included. Only applies when raw text is available.
#'  If included, meta categories are added as the last columns, with names starting with "meta_".
#' @param coverage Logical; if \code{TRUE} and a dictionary is provided (\code{dict}),
#'  will calculate the coverage (number of unique term matches) of each dictionary category.
#' @return A matrix with texts represented by rows, and features in columns, unless there are multiple rows per output
#'  (e.g., when a latent semantic space is applied without terms being mapped) in which case only the special output
#'  is returned (e.g., a matrix with terms as rows and latent dimensions in columns).
#' @seealso If you just want to compare texts, see the \code{\link{lingmatch}} function.
#' @examples
#' # starting with some texts in a vector
#' texts <- c(
#'   "Firstly, I would like to say, and with all due respect...",
#'   "Please, proceed. I hope you feel you can speak freely...",
#'   "Oh, of course, I just hope to be clear, and not cause offense...",
#'   "Oh, no, don't monitor yourself on my account..."
#' )
#'
#' # by default, term counts and metastatistics are returned
#' lma_process(texts)
#'
#' # add dictionary and percent arguments for standard dictionary-based results
#' lma_process(texts, dict = lma_dict(), percent = TRUE)
#'
#' # add space and weight arguments for standard word-centroid vectors
#' lma_process(texts, space = lma_lspace(texts), weight = "tfidf")
#' @export

lma_process <- function(input = NULL, ..., meta = TRUE, coverage = FALSE) {
  inp <- as.list(substitute(...()))
  funs <- c("read.segments", "lma_dtm", "lma_weight", "lma_lspace", "lma_termcat", "lma_patcat")
  allargs <- NULL
  arg_matches <- list()
  for (f in funs) {
    a <- names(as.list(args(f)))
    a <- a[c(FALSE, a[-1] %in% names(inp))]
    allargs <- c(allargs, a)
    arg_matches[[f]] <- inp[a]
  }
  dupargs <- unique(allargs[duplicated(allargs)])
  arg_checks <- vapply(arg_matches, function(l) sum(!names(l) %in% dupargs), 0)
  # identify input
  op <- NULL
  if (is.function(input)) stop("enter a character vector or matrix-like object as input")
  if (is.null(dim(input)) && is.list(input) && is.character(input[[1]])) input <- unlist(input, use.names = FALSE)
  if (is.character(input) || is.factor(input)) {
    ck_paths <- length(input) != 1 && all(file.exists(input))
    op <- if (length(arg_matches$read.segments) || ck_paths) {
      an <- names(arg_matches$read.segments)
      if (!any(grepl("path|text", an))) arg_matches$read.segments$path <- input
      do.call(read.segments, lapply(arg_matches$read.segments, eval.parent, 2))
    } else {
      data.frame(
        text = if (length(input) == 1 && file.exists(input)) readLines(input) else input, stringsAsFactors = FALSE
      )
    }
  } else {
    if (is.null(dim(input))) {
      if (is.null(names(input)) || (is.list(input) && !all(vapply(input, length, 0) == length(input[[1]])))) {
        stop("input is not of a recognized format -- should be text or a dtm-like object")
      }
      input <- if (is.list(input)) as.data.frame(input, stringsAsFactors = FALSE) else t(input)
    }
    op <- input
  }
  # process
  ck_text <- "text" %in% colnames(op) && is.character(op[, "text"])
  ck_changed <- FALSE
  if (ck_text) {
    if (arg_checks[["lma_patcat"]]) {
      if (!"return.dtm" %in% names(arg_matches$lma_patcat) && (coverage || length(arg_matches$lma_weight))) {
        arg_matches$lma_patcat$return.dtm <- TRUE
      }
      arg_matches$lma_patcat$text <- op[, "text"]
      x <- do.call(lma_patcat, lapply(arg_matches$lma_patcat, eval.parent, 2))
      ck_changed <- TRUE
    } else {
      arg_matches$lma_dtm$text <- op[, "text"]
      x <- do.call(lma_dtm, lapply(arg_matches$lma_dtm, eval.parent, 2))
      ck_changed <- TRUE
    }
  } else {
    x <- op
  }
  if (arg_checks[["lma_weight"]]) {
    arg_matches$lma_weight$dtm <- x
    x <- do.call(lma_weight, lapply(arg_matches$lma_weight, eval.parent, 2))
    attr(x, "categories") <- attr(arg_matches$lma_weight$dtm, "categories")
    ck_changed <- TRUE
  }
  if (!is.null(attr(x, "categories"))) {
    categories <- attr(x, "categories")
    xc <- as.data.frame(
      matrix(0, nrow(op), length(categories), dimnames = list(NULL, names(categories))),
      stringsAsFactors = FALSE
    )
    if (coverage) {
      cxc <- xc
      for (cat in names(categories)) {
        su <- x[, categories[[cat]], drop = FALSE]
        xc[, cat] <- rowSums(su, na.rm = TRUE)
        cxc[, cat] <- rowSums(su != 0, na.rm = TRUE)
      }
      colnames(cxc) <- paste0("coverage_", colnames(xc))
      xc <- cbind(xc, cxc)
    } else {
      for (cat in names(categories)) {
        xc[, cat] <- rowSums(x[, categories[[cat]], drop = FALSE], na.rm = TRUE)
      }
    }
    x <- xc
    ck_changed <- TRUE
  } else if (arg_checks[["lma_termcat"]] || (length(arg_matches$lma_termcat) &&
    any(names(arg_matches$lma_termcat) != "dir"))) {
    arg_matches$lma_termcat$coverage <- coverage
    arg_matches$lma_termcat$dtm <- x
    x <- do.call(lma_termcat, lapply(arg_matches$lma_termcat, eval.parent, 2))
    ck_changed <- TRUE
  }
  if (arg_checks[["lma_lspace"]]) {
    nr <- NROW(x)
    arg_matches$lma_lspace$dtm <- x
    x <- do.call(lma_lspace, lapply(arg_matches$lma_lspace, eval.parent, 2))
    if (nrow(x) != nr) {
      colnames(x) <- paste0("dim", seq_len(ncol(x)))
      return(x)
    }
    ck_changed <- TRUE
  }
  if (any(grepl("Matrix", class(x), fixed = TRUE))) x <- as.matrix(x)
  if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  op <- if (ck_text && ck_changed) cbind(op, if (is.null(dim(x))) t(x) else x) else x
  if (ck_text && meta) {
    opm <- lma_meta(op[, "text"])
    if (arg_checks[["lma_weight"]] &&
      (!"normalize" %in% names(arg_matches$lma_weight) || arg_matches$lma_weight$normalize)) {
      cols <- c(9, 14:23)
      opm_counts <- opm[, cols]
      su <- opm_counts != 0
      adj <- if ("percent" %in% names(arg_matches$lma_weight) && arg_matches$lma_weight$percent) 100 else 1
      opm_counts[su] <- opm_counts[su] / rep(opm$words, length(cols))[which(su)] * adj
      opm[, cols] <- opm_counts
    }
    colnames(opm) <- paste0("meta_", colnames(opm))
    op <- cbind(op, opm)
  }
  op
}
