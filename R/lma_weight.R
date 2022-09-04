#' Document-Term Matrix Weighting
#'
#' Weight a document-term matrix.
#' @param dtm A matrix with words as column names.
#' @param weight A string referring at least partially to one (or a combination; see note) of the
#'   available weighting methods:
#'
#'   \strong{Term weights} (applied uniquely to each cell)
#'   \itemize{
#'     \item \strong{\code{binary}} \cr
#'     \code{(dtm > 0) * 1} \cr
#'     Convert frequencies to 1s and 0s; remove differences in frequencies.
#'
#'     \item \strong{\code{log}} \cr
#'     \code{log(dtm + 1, log.base)} \cr
#'     Log of frequencies.
#'
#'     \item \strong{\code{sqrt}} \cr
#'     \code{sqrt(dtm)} \cr
#'     Square root of frequencies.
#'
#'     \item \strong{\code{count}} \cr
#'     \code{dtm} \cr
#'     Unaltered; sometimes called term frequencies (tf).
#'
#'     \item \strong{\code{amplify}} \cr
#'     \code{dtm ^ alpha} \cr
#'     Amplify difference in frequencies.
#'   }
#'
#'   \strong{Document weights} (applied by column)
#'   \itemize{
#'     \item \strong{\code{dflog}} \cr
#'     \code{log(colSums(dtm > 0), log.base)} \cr
#'     Log of binary term sum.
#'
#'     \item \strong{\code{entropy}} \cr
#'     \code{1 - rowSums(x *} \code{log(x + 1, log.base) /} \code{log(ncol(x), log.base),} \code{na.rm = TRUE)} \cr
#'     Where \code{x = t(dtm) / colSums(dtm > 0)}; entropy of term-conditional term distribution.
#'
#'     \item \strong{\code{ppois}} \cr
#'     \code{1 - ppois(pois.x,} \code{colSums(dtm) / nrow(dtm))} \cr
#'     Poisson-predicted term distribution.
#'
#'     \item \strong{\code{dpois}} \cr
#'     \code{1 - dpois(pois.x, colSums(dtm) / nrow(dtm))} \cr
#'     Poisson-predicted term density.
#'
#'     \item \strong{\code{dfmlog}} \cr
#'     \code{log(diag(dtm[max.col(t(dtm)), ]), log.base)} \cr
#'     Log of maximum term frequency.
#'
#'     \item \strong{\code{dfmax}} \cr
#'     \code{diag(dtm[max.col(t(dtm)), ])} \cr
#'     Maximum term frequency.
#'
#'     \item \strong{\code{df}} \cr
#'     \code{colSums(dtm > 0)} \cr
#'     Sum of binary term occurrence across documents.
#'
#'     \item \strong{\code{idf}} \cr
#'     \code{log(nrow(dtm) / colSums(dtm > 0), log.base)} \cr
#'     Inverse document frequency.
#'
#'     \item \strong{\code{ridf}} \cr
#'     \code{idf - log(dpois, log.base)} \cr
#'     Residual inverse document frequency.
#'
#'     \item \strong{\code{normal}} \cr
#'     \code{sqrt(1 / colSums(dtm ^ 2))} \cr
#'     Normalized document frequency.
#'   }
#'
#' Alternatively, \code{'pmi'} or \code{'ppmi'} will apply a pointwise mutual information weighting
#' scheme (with \code{'ppmi'} setting negative values to 0).
#' @param normalize Logical: if \code{FALSE}, the dtm is not divided by document word-count before
#'   being weighted.
#' @param wc.complete If the dtm was made with \code{\link{lma_dtm}} (has a \code{'WC'}
#'   attribute), word counts for
#'   frequencies can be based on the raw count (default; \code{wc.complete = TRUE}). If
#'   \code{wc.complete = FALSE}, or the dtm does not have a \code{'WC'} attribute,
#'   \code{rowSums(dtm)} is used as word count.
#' @param log.base The base of logs, applied to any weight using \code{\link[base]{log}}.
#'   Default is 10.
#' @param alpha A scaling factor applied to document frequency as part of pointwise mutual
#'   information weighting, or amplify's power (\code{dtm ^ alpha}, which defaults to 1.1).
#' @param pois.x integer; quantile or probability of the poisson distribution (\code{dpois(pois.x,
#'   colSums(x,} \code{na.rm = TRUE) / nrow(x))}).
#' @param doc.only Logical: if \code{TRUE}, only document weights are returned (a single value for
#'   each term).
#' @param percent Logical; if \code{TRUE}, frequencies are multiplied by 100.
#' @note
#' Term weights works to adjust differences in counts within documents, with differences meaning
#' increasingly more from \code{binary} to \code{log} to \code{sqrt} to \code{count} to \code{amplify}.
#'
#' Document weights work to treat words differently based on their between-document or overall frequency.
#' When term frequencies are constant, \code{dpois}, \code{idf}, \code{ridf}, and \code{normal} give
#' less common words increasingly more weight, and \code{dfmax}, \code{dfmlog}, \code{ppois}, \code{df},
#' \code{dflog}, and \code{entropy} give less common words increasingly less weight.
#'
#' \code{weight} can either be a vector with two characters, corresponding to term weight and
#' document weight (e.g., \code{c('count', 'idf')}), or it can be a string with term and
#' document weights separated by any of \code{:\\*_/; ,-} (e.g., \code{'count-idf'}).
#' \code{'tf'} is also acceptable for \code{'count'}, and \code{'tfidf'} will be parsed as
#' \code{c('count', 'idf')}, though this is a special case.
#'
#' For \code{weight}, term or document weights can be entered individually; term weights alone will
#' not apply any document weight, and document weights alone will apply a \code{'count'} term weight
#' (unless \code{doc.only = TRUE}, in which case a term-named vector of document weights is returned
#' instead of a weighted dtm).
#' @return A weighted version of \code{dtm}, with a \code{type} attribute added (\code{attr(dtm, 'type')}).
#' @examples
#' # visualize term and document weights
#'
#' ## term weights
#' term_weights <- c("binary", "log", "sqrt", "count", "amplify")
#' Weighted <- sapply(term_weights, function(w) lma_weight(1:20, w, FALSE))
#' if (require(splot)) splot(Weighted ~ 1:20, labx = "Raw Count", lines = "co")
#'
#' ## document weights
#' doc_weights <- c(
#'   "df", "dflog", "dfmax", "dfmlog", "idf", "ridf",
#'   "normal", "dpois", "ppois", "entropy"
#' )
#' weight_range <- function(w, value = 1) {
#'   m <- diag(20)
#'   m[upper.tri(m, TRUE)] <- if (is.numeric(value)) {
#'     value
#'   } else {
#'     unlist(lapply(
#'       1:20, function(v) rep(if (value == "inverted") 21 - v else v, v)
#'     ))
#'   }
#'   lma_weight(m, w, FALSE, doc.only = TRUE)
#' }
#'
#' if (require(splot)) {
#'   category <- rep(c("df", "idf", "normal", "poisson", "entropy"), c(4, 2, 1, 2, 1))
#'   op <- list(
#'     laby = "Relative (Scaled) Weight", labx = "Document Frequency",
#'     leg = "outside", lines = "connected", mv.scale = TRUE, note = FALSE
#'   )
#'   splot(
#'     sapply(doc_weights, weight_range) ~ 1:20,
#'     options = op, title = "Same Term, Varying Document Frequencies",
#'     sud = "All term frequencies are 1.",
#'     colorby = list(category, grade = TRUE)
#'   )
#'   splot(
#'     sapply(doc_weights, weight_range, value = "sequence") ~ 1:20,
#'     options = op, title = "Term as Document Frequencies",
#'     sud = "Non-zero terms are the number of non-zero terms.",
#'     colorby = list(category, grade = TRUE)
#'   )
#'   splot(
#'     sapply(doc_weights, weight_range, value = "inverted") ~ 1:20,
#'     options = op, title = "Term Opposite of Document Frequencies",
#'     sud = "Non-zero terms are the number of zero terms + 1.",
#'     colorby = list(category, grade = TRUE)
#'   )
#' }
#'
#' @export

lma_weight <- function(dtm, weight = "count", normalize = TRUE, wc.complete = TRUE,
                       log.base = 10, alpha = 1, pois.x = 1L, doc.only = FALSE, percent = FALSE) {
  if (is.null(dim(dtm))) dtm <- if (is.character(dtm) || is.factor(dtm)) lma_dtm(dtm) else matrix(dtm, 1)
  ck <- attr(dtm, "type")
  if (!is.null(ck) && length(ck) == 3 && (ck[1] == "TRUE" || ck[2] != "count" || ck[3] != "NA")) {
    message(
      "the entered dtm appears to already be weighted (", paste(ck[2:3], collapse = "*"),
      "), so it will not be altered"
    )
    return(dtm)
  }
  weight <- tolower(weight)
  if (missing(normalize) && any(grepl("pmi", weight))) normalize <- FALSE
  if (normalize) {
    wc <- attr(dtm, "WC")
    if (is.null(wc) || !wc.complete || nrow(dtm) != length(wc)) wc <- rowSums(dtm, na.rm = TRUE)
    adj <- if (percent) 100 else 1
    if (.hasSlot(dtm, "x") && .hasSlot(dtm, "i")) {
      wc <- wc[dtm@i + 1]
      su <- wc != 0
      dtm@x[su] <- dtm@x[su] / wc[su] * adj
    } else {
      su <- wc != 0
      dtm[su, ] <- dtm[su, ] / wc[su] * adj
    }
  }
  nr <- nrow(dtm)
  if (any(grepl("pmi", weight))) {
    tw <- dw <- "pmi"
    if (missing(log.base)) log.base <- 2
    twc <- sum(dtm, na.rm = TRUE)
    pc <- matrix(colSums(dtm^alpha, na.rm = TRUE) / twc^alpha, 1)
    dtm <- dtm / twc
    dtm <- dtm / rowSums(dtm, na.rm = TRUE) %*% pc
    if (.hasSlot(dtm, "x")) {
      dtm@x <- log(dtm@x, base = log.base)
      dtm@x[!is.finite(dtm@x)] <- 0
    } else {
      dtm <- log(dtm, base = log.base)
      dtm[!is.finite(dtm)] <- 0
    }
    if (any(grepl("pp", weight))) {
      tw <- dw <- "ppmi"
      dtm[dtm < 0] <- 0
    }
  } else {
    term <- function(x, type) {
      switch(type,
        binary = (x > 0) * 1,
        log = log(x + 1, base = log.base),
        sqrt = sqrt(x),
        count = x,
        amplify = x^alpha
      )
    }
    doc <- function(x, type) {
      d <- switch(type,
        df = colSums(x > 0, na.rm = TRUE),
        dflog = log(colSums(x > 0, na.rm = TRUE), base = log.base),
        dfmax = diag(x[max.col(t(x)), ]),
        dfmlog = log(diag(x[max.col(t(x)), ]), base = log.base),
        idf = log(nrow(x) / colSums(x > 0, na.rm = TRUE), base = log.base),
        normal = sqrt(1 / colSums(x^2, na.rm = TRUE)),
        dpois = 1 - dpois(pois.x, colSums(x, na.rm = TRUE) / nrow(x)),
        ppois = 1 - ppois(pois.x, colSums(x, na.rm = TRUE) / nrow(x)),
        ridf = doc(x, "idf") - log(doc(x, "dpois"), base = log.base),
        entropy = {
          x <- t(x) / colSums(x > 0, na.rm = TRUE)
          1 - rowSums(x * log(x + 1, base = log.base) /
            log(ncol(x), base = log.base), na.rm = TRUE)
        }
      )
      d[!is.finite(d)] <- 0
      d
    }
    if (length(weight) == 1) {
      weight <- strsplit(weight, " *[:\\*_/; ,-] *")[[1]]
      if (length(weight) == 1 && weight == "tfidf") weight <- c("count", "idf")
    }
    if (grepl("^(?:t|na|non|f)", weight[1])) weight[1] <- "count"
    tws <- c("binary", "log", "sqrt", "count", "amplify")
    tw <- if (weight[1] == "") "count" else grep(substr(weight[1], 0, 4), tws, value = TRUE)[1]
    pdw <- TRUE
    dws <- c("df", "dflog", "dfmax", "dfmlog", "idf", "normal", "dpois", "ppois", "ridf", "entropy")
    if (is.na(tw)) {
      tw <- grep(substr(weight[1], 0, 4), dws, value = TRUE)[1]
      if (!is.na(tw)) {
        pdw <- FALSE
        if (!doc.only) {
          dw <- tw
          tw <- "count"
        } else {
          return(doc(dtm, tw))
        }
      } else {
        stop(paste(weight, collapse = " * "), " is not a recognized weight", call. = FALSE)
      }
    }
    if (pdw) dw <- if (length(weight) > 1) grep(substr(weight[2], 0, 4), dws, value = TRUE)[1] else "none"
    if (is.na(dw)) dw <- "none"
    if (missing(alpha) && tw == "amplify") alpha <- 1.1
    dtm <- if (dw == "none") term(dtm, tw) else term(dtm, tw) * rep(doc(dtm, dw), each = nr)
  }
  attr(dtm, "type") <- c(normalized = normalize, term = tw, document = dw)
  dtm
}
