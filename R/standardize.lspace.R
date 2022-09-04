#' Standardize a Latent Semantic Space
#'
#' Reformat a .rda file which has a matrix with terms as row names, or a plain-text embeddings file
#' which has a term at the start of each line, and consistent delimiting characters. Plain-text files
#' are processed line-by-line, so large spaces can be reformatted RAM-conservatively.
#'
#' @param infile Name of the .rda or plain-text file relative to \code{dir}, \cr
#'   e.g., "default.rda" or "glove/glove.6B.300d.txt".
#' @param name Base name of the reformatted file and term file; e.g., "glove" would result in
#'   \code{glove.dat} and \code{glove_terms.txt} in \code{outdir}.
#' @param sep Delimiting character between values in each line, e.g., \code{" "} or \code{"\\t"}.
#'   Only applies to plain-text files.
#' @param digits Number of digits to round values to; default is 9.
#' @param dir Path to folder containing \code{infile}s. \cr Default is \code{getOption('lingmatch.lspace.dir')},
#'   which must be set in the current session. If this is not specified and \code{infile} is a full path,
#'   \code{dir} will be set to \code{infile}'s parent directory.
#' @param outdir Path to folder in which to save standardized files; default is \code{dir}.
#' @param remove A string with a regex pattern to be removed from term names \cr (i.e., \code{gsub(remove,}
#'   \code{"", term)}); default is \code{""}, which is ignored.
#' @param term_check A string with a regex pattern by which to filter terms; i.e., only lines with fully
#'   matched terms are written to the reformatted file. The default attempts to retain only regular words, including
#'   those with dashes, foreword slashes, and periods. Set to an empty string (\code{""}) to write all lines
#'   regardless of term.
#' @param verbose Logical: if \code{TRUE}, prints the current line number and its term to the console every 1,000 lines.
#'   Only applies to plain-text files.
#' @family Latent Semantic Space functions
#' @return Path to the standardized [1] data file and [2] terms file if applicable.
#' @examples
#' \dontrun{
#'
#' # from https://sites.google.com/site/fritzgntr/software-resources/semantic_spaces
#' standardize.lspace("EN_100k_lsa.rda", "100k_lsa")
#'
#' # from https://fasttext.cc/docs/en/english-vectors.html
#' standardize.lspace("crawl-300d-2M.vec", "facebook_crawl")
#'
#' # Standardized versions of these spaces can also be downloaded with download.lspace.
#' }
#' @export

standardize.lspace <- function(infile, name, sep = " ", digits = 9, dir = getOption("lingmatch.lspace.dir"),
                               outdir = dir, remove = "", term_check = "^[a-zA-Z]+$|^['a-zA-Z][a-zA-Z.'\\/-]*[a-zA-Z.]$", verbose = FALSE) {
  if (is.character(infile) && file.exists(infile) && missing(dir)) {
    dir <- dirname(normalizePath(infile, mustWork = FALSE))
    if (missing(outdir)) outdir <- dir
  } else if (dir == "") {
    if (outdir != "") {
      dir <- outdir
    } else {
      stop(paste(
        "specify a directory (dir), or set the lspace directory option\n(e.g.,",
        'options(lingmatch.lspace.dir = ~/Latent Semantic Spaces")) or call lma_initdir()'
      ), call. = FALSE)
    }
  }
  if (!is.character(term_check)) term_check <- ""
  if (is.character(infile)) {
    ip <- normalizePath(paste0(dir, "/", infile), "/", FALSE)
    if (!file.exists(ip)) ip <- infile
  }
  op <- normalizePath(paste0(outdir, "/", name), "/", FALSE)
  fs <- op
  if (!is.character(infile) || grepl("\\.rda$", infile)) {
    if (is.character(infile)) {
      f <- load(ip)
      o <- get(f)
    } else {
      o <- infile
    }
    o <- round(o, digits)
    ot <- rownames(o)
    if (remove != "") ot <- gsub(remove, "", ot)
    if (term_check != "") {
      su <- grepl(term_check, ot)
      o <- o[su, ]
      ot <- ot[su]
    }
    fs <- paste0(op, c(".dat", "_terms.txt"))
    writeLines(ot, fs[2])
    write(formatC(t(o), digits, 0, "f"), fs[1], ncol(o))
    if (is.character(infile)) rm(f, "o")
  } else {
    if (!file.exists(ip)) stop("infile does not exist: ", ip)
    if (!grepl(term_check, scan(ip, "", 1, sep = sep, quiet = TRUE))) {
      stop("infile does not appear to start with a term: ", ip)
    }
    fs <- paste0(op, ".dat")
    reformat_embedding(ip, op, sep, digits, remove, term_check, verbose)
  }
  message("created ", op, ".dat\nfrom ", ip)
  fs
}
