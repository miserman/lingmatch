#' Latent Semantic Space (Embeddings) Operations
#'
#' Map a document-term matrix onto a latent semantic space, extract terms from a
#' latent semantic space (if \code{dtm} is a character vector, or \code{map.space =} \code{FALSE}),
#' or perform a singular value decomposition of a document-term matrix (if \code{dtm} is a matrix
#' and \code{space} is missing).
#' @param dtm A matrix with terms as column names, or a character vector of terms to be extracted
#'   from a specified space. If this is of length 1 and \code{space} is missing, it will be treated
#'   as \code{space}.
#' @param space A matrix with terms as rownames. If missing, this will be the right singular vectors
#'   of a singular value decomposition of \code{dtm}. If a character, a file matching the character
#'   will be searched for in \code{dir} (e.g., \code{space = 'google'}). If a file is not found and
#'   the character matches one of the \href{https://osf.io/489he/wiki/home}{available spaces}, you
#'   will be given the option to download it, as handled by \code{\link{download.lspace}}.
#'   If \code{dtm} is missing, the entire space will be loaded and returned.
#' @param map.space Logical: if \code{FALSE}, the original vectors of \code{space} for terms
#'   found in \code{dtm} are returned. Otherwise \code{dtm} \code{\%*\%} \code{space} is returned,
#'   excluding uncommon columns of \code{dtm} and rows of \code{space}.
#' @param fill.missing Logical: if \code{TRUE} and terms are being extracted from a space, includes
#'   terms not found in the space as rows of 0s, such that the returned matrix will have a row
#'   for every requested term.
#' @param term.map A matrix with \code{space} as a column name, terms as row names, and indices of
#'   the terms in the given space as values, or a numeric vector of indices with terms as names, or
#'   a character vector or terms corresponding to rows of the space. This is used instead of reading
#'   in an "_terms.txt" file corresponding to a \code{space} entered as a character (the name of a
#'   space file).
#' @param dim.cutoff If a \code{space} is calculated, this will be used to decide on the number of
#'   dimensions to be retained: \code{cumsum(d) / sum(d) < dim.cutoff}, where \code{d} is a vector
#'   of singular values of \code{dtm} (i.e., \code{svd(dtm)$d}). The default is \code{.5}; lower
#'   cutoffs result in fewer dimensions.
#' @param keep.dim Logical: if \code{TRUE}, and a space is being calculated from the input, a matrix
#'   in the same dimensions as \code{dtm} is returned. Otherwise, a matrix with terms as rows and
#'   dimensions as columns is returned.
#' @param use.scan Logical: if \code{TRUE}, reads in the rows of \code{space} with \code{\link{scan}}.
#' @param dir Path to a folder containing spaces. \cr
#'   Set a session default with \code{options(lingmatch.lspace.dir = 'desired/path')}.
#' @note
#' A traditional latent semantic space is a selection of right singular vectors from the singular
#' value decomposition of a dtm (\code{svd(dtm)$v[, 1:k]}, where \code{k} is the selected number of
#' dimensions, decided here by \code{dim.cutoff}).
#'
#' Mapping a new dtm into a latent semantic space consists of multiplying common terms:
#' \code{dtm[, ct]} \code{\%*\% space[ct, ]}, where \code{ct} \code{=} \code{colnames(dtm)[colnames(dtm)}
#' \code{\%in\%} \code{rownames(space)]} -- the terms common between the dtm and the space. This
#' results in a matrix with documents as rows, and dimensions as columns, replacing terms.
#' @family Latent Semantic Space functions
#' @return A matrix or sparse matrix with either (a) a row per term and column per latent dimension (a latent
#' space, either calculated from the input, or retrieved when \code{map.space = FALSE}), (b) a row per document
#' and column per latent dimension (when a dtm is mapped to a space), or (c) a row per document and
#' column per term (when a space is calculated and \code{keep.dim = TRUE}).
#' @examples
#' text <- c(
#'   paste(
#'     "Hey, I like kittens. I think all kinds of cats really are just the",
#'     "best pet ever."
#'   ),
#'   paste(
#'     "Oh year? Well I really like cars. All the wheels and the turbos...",
#'     "I think that's the best ever."
#'   ),
#'   paste(
#'     "You know what? Poo on you. Cats, dogs, rabbits -- you know, living",
#'     "creatures... to think you'd care about anything else!"
#'   ),
#'   paste(
#'     "You can stick to your opinion. You can be wrong if you want. You know",
#'     "what life's about? Supercharging, diesel guzzling, exhaust spewing,",
#'     "piston moving ignitions."
#'   )
#' )
#'
#' dtm <- lma_dtm(text)
#'
#' # calculate a latent semantic space from the example text
#' lss <- lma_lspace(dtm)
#'
#' # show that document similarities between the truncated and full space are the same
#' spaces <- list(
#'   full = lma_lspace(dtm, keep.dim = TRUE),
#'   truncated = lma_lspace(dtm, lss)
#' )
#' sapply(spaces, lma_simets, metric = "cosine")
#'
#' \dontrun{
#'
#' # specify a directory containing spaces,
#' # or where you would like to download spaces
#' space_dir <- "~/Latent Semantic Spaces"
#'
#' # map to a pretrained space
#' ddm <- lma_lspace(dtm, "100k", dir = space_dir)
#'
#' # load the matching subset of the space
#' # without mapping
#' lss_100k_part <- lma_lspace(colnames(dtm), "100k", dir = space_dir)
#'
#' ## or
#' lss_100k_part <- lma_lspace(dtm, "100k", map.space = FALSE, dir = space_dir)
#'
#' # load the full space
#' lss_100k <- lma_lspace("100k", dir = space_dir)
#'
#' ## or
#' lss_100k <- lma_lspace(space = "100k", dir = space_dir)
#' }
#' @export

lma_lspace <- function(dtm = "", space, map.space = TRUE, fill.missing = FALSE, term.map = NULL,
                       dim.cutoff = .5, keep.dim = FALSE, use.scan = FALSE, dir = getOption("lingmatch.lspace.dir")) {
  if (ckd <- dir == "") dir <- "~/Latent Semantic Spaces"
  if ((is.character(dtm) || is.factor(dtm)) && missing(space)) {
    if (length(dtm) > 1 && any(grepl(" ", dtm, fixed = TRUE))) {
      dtm <- lma_dtm(dtm)
    } else if (length(dtm) == 1 && dtm != "") {
      if (missing(use.scan)) use.scan <- TRUE
      space <- dtm
      dtm <- ""
    }
  }
  if (is.data.frame(dtm)) dtm <- as.matrix(dtm)
  if (missing(space)) {
    nr <- nrow(dtm)
    if (is.null(nr)) stop("enter a matrix for dtm, or specify a space")
    nc <- ncol(dtm)
    md <- min(nr, nc)
    s <- svd(dtm)
    s$v <- t(s$v)
    k <- cumsum(s$d) / sum(s$d)
    if (dim.cutoff > 1) dim.cutoff <- 1
    k <- if (length(k) == 1) 1 else seq_len(if (any(k < dim.cutoff)) which(k >= dim.cutoff)[1] else 1)
    if (keep.dim) {
      dtm[] <- s$u[, k] %*% (if (length(k) == 1) matrix(s$d[k]) else diag(s$d[k])) %*% s$v[k, ]
    } else {
      cn <- colnames(dtm)
      dtm <- t(s$v[k, , drop = FALSE])
      rownames(dtm) <- cn
    }
  } else {
    terms <- if (is.null(colnames(dtm))) {
      map.space <- FALSE
      dtm
    } else {
      colnames(dtm)
    }
    if (is.character(space)) {
      if (space == "default") space <- "100k_lsa"
      name <- gsub("^.*[/\\]|\\.[^/\\]*$", "", space)[1]
      spaces <- list.files(dir)
      ts <- grep(space, spaces, fixed = TRUE, value = TRUE)
      if (!length(ts)) {
        ts <- rownames(select.lspace(name)$selected)[1]
        if (!ckd && !is.na(ts) && grepl("^$|^[yt1]|^ent", readline(paste0(
          "would you like to download the ", ts, " space? (press Enter for yes): "
        )))) {
          download.lspace(ts, dir = dir)
          ts <- paste0(ts, ".dat")
        } else {
          stop("space (", space, ") not found in dir (", dir, ")",
            if (ckd) '\nspecify a directory (e.g., dir = "~") to locate or download; see ?download.lspace',
            call. = FALSE
          )
        }
      }
      space_path <- normalizePath(paste0(dir, "/", if (length(su <- grep("\\.dat$", ts))) {
        ts[su[[1]]]
      } else {
        use.scan <- TRUE
        ts[grep("[bgx]z[ip2]*$", ts)[[1]]]
      }), "/", FALSE)
      name <- sub("\\.[^.]*$", "", basename(space_path))
      if (name %in% colnames(term.map)) term.map <- term.map[term.map[, name] != 0, name]
      rex <- function(inds, f) {
        nc <- length(strsplit(readLines(f, 1), "\\s+")[[1]])
        l <- length(inds)
        all <- all(seq_len(l) == inds)
        r <- matrix(0, l, nc)
        i <- 1
        con <- file(f, "r")
        on.exit(close(con))
        while (i <= l) {
          if (all) {
            n <- l
          } else {
            n <- 1
            while (i + n < l && inds[i + n - 1] == inds[i + n] - 1) n <- n + 1
          }
          r[seq_len(n) + i - 1, ] <- matrix(scan(
            con,
            n = n * nc, quiet = TRUE, skip = (if (i == 1) {
              inds[i]
            } else {
              inds[i] - inds[i - 1]
            }) - 1, quote = "", comment.char = "", na.strings = ""
          ), n, nc, byrow = TRUE)
          i <- i + n
        }
        r
      }
      if (!is.null(term.map)) {
        if (is.character(term.map)) term.map <- structure(seq_along(term.map), names = term.map)
        su <- which(names(term.map) %in% terms)
        inds <- as.numeric(sort(if (length(terms) == 1 && terms == "") term.map else term.map[su]))
        if (length(inds)) {
          space <- if (use.scan) rex(inds, space_path) else t(extract_indices(inds, space_path))
          rownames(space) <- ts <- names(term.map)[inds]
        } else {
          stop("no matching terms in space ", space)
        }
      } else {
        if (!file.exists(normalizePath(paste0(dir, "/", name, "_terms.txt"), "/", FALSE))) {
          if (file.exists(normalizePath(paste0(dir, "/lma_term_map.rda")))) {
            lma_term_map <- NULL
            load(normalizePath(paste0(dir, "/lma_term_map.rda"), "/", FALSE))
            if (!is.null(lma_term_map) && !is.null(colnames(lma_term_map)) && name %in% colnames(lma_term_map)) {
              space_terms <- names(lma_term_map[lma_term_map[, name] != 0, name])
            } else {
              stop(
                "could not find terms file (", space, "_terms.txt) in dir (", dir, "),",
                " nor retrieve terms from them term map (lma_term_map.rda)."
              )
            }
          } else {
            stop("terms file (", space, "_terms.txt) not found in dir (", dir, ").")
          }
        } else {
          space_terms <- readLines(normalizePath(paste0(dir, "/", name, "_terms.txt"), "/", FALSE))
        }
        su <- if (length(terms) == 1 && terms == "") {
          terms <- space_terms
          !logical(length(space_terms))
        } else {
          space_terms %in% terms
        }
        if (sum(su) < length(terms)) {
          lsterms <- tolower(space_terms)
          su2 <- !duplicated(lsterms) & lsterms %in% terms[!terms %in% space_terms[su]]
          if (any(su2)) {
            space_terms[su2] <- lsterms[su2]
            su <- su | su2
          }
        }
        if (sum(su)) {
          space <- if (use.scan) rex(which(su), space_path) else t(extract_indices(which(su), space_path))
          rownames(space) <- space_terms[su]
          ts <- terms[terms %in% rownames(space)]
          space <- space[ts, , drop = FALSE]
        } else {
          stop("no matching terms in space ", space)
        }
      }
    } else {
      if (is.data.frame(space)) space <- as.matrix(space)
      name <- deparse(substitute(space))
      su <- terms %in% rownames(space)
      if (sum(su)) {
        ts <- terms[su]
        space <- space[ts, , drop = FALSE]
      } else if (sum(su <- terms %in% colnames(space))) {
        ts <- terms[su]
        space <- t(space[, ts, drop = FALSE])
      } else {
        stop("no matching terms in provided space")
      }
    }
    if (fill.missing) {
      su <- which(!terms %in% rownames(space))
      if (length(su)) {
        space <- rbind(space, matrix(0, length(su), ncol(space), dimnames = list(terms[su])))
        space <- space[terms, ]
      }
      ts <- rownames(space)
    }
    attr(space, "space") <- name
    if (map.space) {
      rep <- length(ts) / ncol(dtm)
      if (rep < .2) {
        warning(paste0(
          "only ", round(rep * 100, 2), "% of dtm terms appear in the provided space; ",
          "you might consider using a different source or cleaning/partial matching terms"
        ), call. = FALSE)
      }
      dtm <- dtm[, ts, drop = FALSE] %*% space
      attr(dtm, "space") <- name
    } else {
      return(space)
    }
  }
  dtm
}
