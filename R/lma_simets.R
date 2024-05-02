#' Similarity Calculations
#'
#' Enter a numerical matrix, set of vectors, or set of matrices to calculate similarity per vector.
#'
#' @param a A vector or matrix. If a vector, \code{b} must also be provided. If a matrix and \code{b}
#'   is missing, each row will be compared. If a matrix and \code{b} is not missing, each row will
#'   be compared with \code{b} or each row of \code{b}.
#' @param b A vector or matrix to be compared with \code{a} or rows of \code{a}.
#' @param metric A character or vector of characters at least partially matching one of the
#'   available metric names (or 'all' to explicitly include all metrics),
#'   or a number or vector of numbers indicating the metric by index:
#'   \itemize{
#'     \item \strong{\code{jaccard}}: \code{sum(a & b) / sum(a | b)}
#'     \item \strong{\code{euclidean}}: \code{1 / (1 + sqrt(sum((a - b) ^ 2)))}
#'     \item \strong{\code{canberra}}: \code{mean(1 - abs(a - b) / (a + b))}
#'     \item \strong{\code{cosine}}: \code{sum(a * b) / sqrt(sum(a ^ 2 * sum(b ^ 2)))}
#'     \item \strong{\code{pearson}}: \code{(mean(a * b) - (mean(a) * mean(b))) /} \cr
#'       \code{sqrt(mean(a ^ 2) - mean(a) ^ 2) / sqrt(mean(b ^ 2) - mean(b) ^ 2)}
#'   }
#' @param group If \code{b} is missing and \code{a} has multiple rows, this will be used to make
#'   comparisons between rows of \code{a}, as modified by \code{agg} and \code{agg.mean}.
#' @param lag Amount to adjust the \code{b} index; either rows if \code{b} has multiple rows (e.g.,
#'   for \code{lag = 1}, \code{a[1, ]} is compared with \code{b[2, ]}), or values otherwise (e.g.,
#'   for \code{lag = 1}, \code{a[1]} is compared with \code{b[2]}). If \code{b} is not supplied,
#'   \code{b} is a copy of \code{a}, resulting in lagged self-comparisons or autocorrelations.
#' @param agg Logical: if \code{FALSE}, only the boundary rows between groups will be compared, see
#'   example.
#' @param agg.mean Logical: if \code{FALSE} aggregated rows are summed instead of averaged.
#' @param pairwise Logical: if \code{FALSE} and \code{a} and \code{b} are matrices with the same number of
#'   rows, only paired rows are compared. Otherwise (and if only \code{a} is supplied), all pairwise
#'   comparisons are made.
#' @param symmetrical Logical: if \code{TRUE} and pairwise comparisons between \code{a} rows were made,
#'   the results in the lower triangle are copied to the upper triangle.
#' @param mean Logical: if \code{TRUE}, a single mean for each metric is returned per row of \code{a}.
#' @param return.list Logical: if \code{TRUE}, a list-like object will always be returned, with an entry
#'   for each metric, even when only one metric is requested.
#' @details
#' Use \code{\link[RcppParallel]{setThreadOptions}} to change parallelization options; e.g., run
#' RcppParallel::setThreadOptions(4) before a call to lma_simets to set the number of CPU
#' threads to 4.
#' @return Output varies based on the dimensions of \code{a} and \code{b}:
#'   \itemize{
#'     \item \strong{Out:} A vector with a value per metric. \cr
#'       \strong{In:} Only when \code{a} and \code{b} are both vectors.
#'     \item \strong{Out:} A vector with a value per row. \cr
#'       \strong{In:} Any time a single value is expected per row: \code{a} or \code{b} is a vector,
#'       \code{a} and \code{b} are matrices with the same number of rows and \code{pairwise = FALSE}, a group is
#'       specified, or \code{mean = TRUE}, and only one metric is requested.
#'     \item \strong{Out:} A data.frame with a column per metric. \cr
#'       \strong{In:} When multiple metrics are requested in the previous case.
#'     \item \strong{Out:} A sparse matrix with a \code{metric} attribute with the metric name. \cr
#'       \strong{In:} Pairwise comparisons within an \code{a} matrix or between
#'       an \code{a} and \code{b} matrix, when only 1 metric is requested.
#'     \item \strong{Out:} A list with a sparse matrix per metric. \cr
#'       \strong{In:} When multiple metrics are requested in the previous case.
#'   }
#' @examples
#' text <- c(
#'   "words of speaker A", "more words from speaker A",
#'   "words from speaker B", "more words from speaker B"
#' )
#' (dtm <- lma_dtm(text))
#'
#' # compare each entry
#' lma_simets(dtm)
#'
#' # compare each entry with the mean of all entries
#' lma_simets(dtm, colMeans(dtm))
#'
#' # compare by group (corresponding to speakers and turns in this case)
#' speaker <- c("A", "A", "B", "B")
#'
#' ## by default, consecutive rows from the same group are averaged:
#' lma_simets(dtm, group = speaker)
#'
#' ## with agg = FALSE, only the rows at the boundary between
#' ## groups (rows 2 and 3 in this case) are used:
#' lma_simets(dtm, group = speaker, agg = FALSE)
#' @export

lma_simets <- function(a, b = NULL, metric = NULL, group = NULL, lag = 0, agg = TRUE, agg.mean = TRUE,
                       pairwise = TRUE, symmetrical = FALSE, mean = FALSE, return.list = FALSE) {
  mets <- c("jaccard", "euclidean", "canberra", "cosine", "pearson")
  if (missing(metric) && length(b) == 1 && !grepl(" ", b) &&
    any(grepl(tolower(substr(b, 1, 3)), mets, fixed = TRUE))) {
    metric <- b
    b <- NULL
  }
  met <- match_metric(metric)
  if (!length(met$selected)) {
    stop(
      "no recognized metric; should match one of ",
      paste0(mets, collapse = ", "), ", or all"
    )
  }
  st <- proc.time()[[3]]
  slots <- c("i", "p", "x", "Dim")
  if ((is.character(a) || is.factor(a)) && any(grepl("[a-zA-Z]", a))) {
    a <- lma_dtm(a)
  } else if (is.data.frame(a)) a <- Matrix(as.matrix(a), sparse = TRUE)
  if (is.null(b) && !missing(lag) && is.null(dim(a))) b <- a
  if (is.null(b)) {
    n <- dim(a)[1]
    if (is.null(n) || n < 2) stop("a must have more than 1 row when b is not provided", call. = FALSE)
    if (is.null(group)) {
      if (!all(slots %in% slotNames(a))) a <- as(a, "CsparseMatrix")
      res <- calculate_similarities(a, NULL, 2, met$dummy)
      for (i in seq_along(res)) attr(res[[i]], "metric") <- met$selected[i]
    } else {
      if (length(group) != n) stop("group is not the same length as a or columns in a")
      ager <- if (agg.mean) colMeans else colSums
      l <- length(group)
      chunks <- NULL
      i <- 1
      while (i < l) {
        st <- i
        g <- group[i]
        while (i < l && g == group[i + 1]) i <- i + 1
        chunks <- c(chunks, list(seq(st, i)))
        i <- i + 1
      }
      if (!any(chunks[[length(chunks)]] == l)) chunks <- c(chunks, list(l))
      rows <- character(length(chunks) - 1)
      res <- as.data.frame(matrix(0, length(chunks) - 1, sum(met$dummy), dimnames = list(NULL, met$selected)))
      for (i in seq_len(length(chunks) - 1)) {
        s <- chunks[[i]]
        sa <- if (agg) s else s[length(s)]
        ta <- ager(a[sa, , drop = FALSE])
        s <- chunks[[i + 1]]
        sb <- if (agg) s else s[1]
        tb <- ager(a[sb, , drop = FALSE])
        res[i, ] <- vector_similarity(ta, tb, met$dummy)
        rows[i] <- paste(paste(sa, collapse = ", "), "<->", paste(sb, collapse = ", "))
      }
      rownames(res) <- rows
    }
  } else {
    if ((is.character(b) || is.factor(b)) && any(grepl("[a-zA-Z]", b))) {
      b <- lma_dtm(b)
    } else if (is.data.frame(b)) b <- Matrix(as.matrix(b), sparse = TRUE)
    bn <- if (is.null(dim(b))) length(b) else dim(b)[1]
    if (lag && abs(lag) >= bn) lag <- if (lag < 0) -bn + 1 else bn - 1
    res <- if (is.null(dim(b)) && length(a) == bn && (is.null(dim(a)) || any(dim(a) == 1))) {
      b <- as.numeric(b)
      if (lag) b <- if (lag < 0) c(b[-seq_len(-lag)], numeric(-lag)) else c(numeric(lag), b)[seq_len(bn)]
      vector_similarity(as.numeric(a), b, met$dummy)
    } else {
      if (is.null(dim(a))) a <- Matrix(a, 1, dimnames = list(NULL, names(a)), sparse = TRUE)
      if (!all(slots %in% slotNames(a))) a <- as(a, "CsparseMatrix")
      if (is.null(dim(b))) b <- Matrix(b, 1, dimnames = list(NULL, names(b)), sparse = TRUE)
      if (!all(slots %in% slotNames(b))) b <- as(b, "CsparseMatrix")
      d <- c(dim(a), dim(b))
      if (d[2] != d[4]) {
        ns <- colnames(a)
        if (!is.null(ns)) {
          ns <- ns[ns %in% colnames(b)]
          if (length(ns)) {
            a <- a[, ns, drop = FALSE]
            b <- b[, ns, drop = FALSE]
          }
        }
        d <- c(dim(a), dim(b))
        if (d[2] != d[4]) {
          stop("a and b have a different number of columns, which could not be aligned by name")
        }
      }
      if (lag) {
        b <- if (lag > 0) {
          rbind(Matrix(0, lag, d[4], sparse = TRUE), b[-(seq_len(lag) + d[3] - lag), ])
        } else {
          rbind(b[-seq_len(-lag), ], Matrix(0, -lag, d[4], sparse = TRUE))
        }
      }
      type <- if (((missing(pairwise) || !pairwise) && d[1] == d[3]) ||
        d[3] == 1) {
        1
      } else {
        3
      }
      calculate_similarities(a, b, type, met$dummy)
    }
  }
  if ("list" %in% class(res) && length(res)) {
    pairwise <- "dtCMatrix" %in% class(res[[1]])
    if ((pairwise && symmetrical) || mean) {
      for (i in seq_along(res)) {
        if (pairwise && (symmetrical || mean)) res[[i]] <- forceSymmetric(res[[i]], "L")
        if (mean) {
          res[[i]] <- if (is.null(dim(res[[i]]))) {
            mean(res[[i]], na.rm = TRUE)
          } else {
            (rowSums(res[[i]], TRUE) - 1) / (ncol(res[[i]]) - 1)
          }
        }
      }
    }
    if (is.null(dim(res[[1]]))) {
      rn <- if (!is.na(nd <- which(c(dim(a), dim(b)) == length(res[[1]]))[1]) && !is.null(rownames(if (nd == 1) a else b))) {
        rownames(if (nd == 1) a else b)
      } else {
        NULL
      }
      if (length(met$selected) == 1) {
        if (length(rn) == length(res[[1]])) names(res[[1]]) <- rn
      } else {
        attr(res, "row.names") <- if (length(rn) == length(res[[1]])) rn else seq_along(res[[1]])
        attr(res, "class") <- "data.frame"
      }
    }
    if (!return.list && length(met$selected) == 1) res <- res[[1]]
  }
  attr(res, "time") <- c(simets = proc.time()[[3]] - st)
  res
}
