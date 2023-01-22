#' Read and Segment Multiple Texts
#'
#' Split texts by word count or specific characters. Input texts directly, or read them in from files.
#'
#' @param path Path to a folder containing files, or a vector of paths to files. If no folders or files are
#'   recognized in \code{path}, it is treated as \code{text}.
#' @param segment Specifies how the text of each file should be segmented. If a character, split at that character;
#'   '\\n' by default. If a number, texts will be broken into that many segments, each with a roughly equal number of
#'   words.
#' @param ext The extension of the files you want to read in. '.txt' by default.
#' @param subdir Logical; if \code{TRUE}, files in folders in \code{path} will also be included.
#' @param segment.size Logical; if specified, \code{segment} will be ignored, and texts will be broken into
#'   segments containing roughly \code{segment.size} number of words.
#' @param bysentence Logical; if \code{TRUE}, and \code{segment} is a number or \code{segment.size} is specified,
#'   sentences will be kept together, rather than potentially being broken across segments.
#' @param end_in_quotes Logical; if \code{FALSE}, sentence-ending marks (\code{.?!}) will not be considered when
#'   immediately followed by a quotation mark. For example, \code{'"Word." Word.'} would be considered one sentence.
#' @param preclean Logical; if \code{TRUE}, text will be cleaned with \code{lma_dict(special)} before
#'   segmentation.
#' @param text A character vector with text to be split, used in place of \code{path}. Each entry is treated as a file.
#' @return A \code{data.frame} with columns for file names (\code{input}),
#' segment number within file (\code{segment}), word count for each segment (\code{WC}), and the text of
#' each segment (\code{text}).
#' @examples
#' # split preloaded text
#' read.segments("split this text into two segments", 2)
#'
#' \dontrun{
#'
#' # read in all files from the package directory
#' texts <- read.segments(path.package("lingmatch"), ext = "")
#' texts[, -4]
#'
#' # segment .txt files in dir in a few ways:
#' dir <- "path/to/files"
#'
#' ## into 1 line segments
#' texts_lines <- read.segments(dir)
#'
#' ## into 5 even segments each
#' texts_5segs <- read.segments(dir, 5)
#'
#' ## into 50 word segments
#' texts_50words <- read.segments(dir, segment.size = 50)
#'
#' ## into 1 sentence segments
#' texts_1sent <- read.segments(dir, segment.size = 1, bysentence = TRUE)
#' }
#' @export

read.segments <- function(path = ".", segment = NULL, ext = ".txt", subdir = FALSE, segment.size = -1,
                          bysentence = FALSE, end_in_quotes = TRUE, preclean = FALSE, text = NULL) {
  if (any(path == "")) path[path == ""] <- "."
  if (!any(file.exists(sub("[\\/]+$", "", path)))) {
    ck_text <- TRUE
    files <- path
  } else {
    ck_text <- !is.null(text)
    files <- if (ck_text) {
      text
    } else {
      dirs <- list.dirs(path, recursive = subdir)
      files <- if (any(dir.exists(path))) {
        unique(list.files(path, ext, recursive = subdir, full.names = TRUE))
      } else {
        path[file.exists(path)]
      }
      files[!files %in% dirs]
    }
  }
  if (missing(segment) && missing(segment.size)) {
    segment <- if (length(path) == 1 && (ck_text || !dir.exists(path))) "\n" else 1
  }
  if (length(files)) {
    err <- function(e) NULL
    args <- list(what = character(), quote = "", na.strings = "", quiet = TRUE)
    if (is.character(segment) && segment.size == -1) args$sep <- segment
    do.call(rbind, lapply(seq_along(files), function(fi) {
      f <- files[fi]
      args[[if (ck_text) "text" else "file"]] <- f
      WC <- NULL
      if (ck_text || file.exists(f)) {
        if (is.numeric(segment) || segment.size > 0) {
          words <- tryCatch(do.call(scan, args), error = err)
          if (!length(words)) {
            return(NULL)
          }
          if (preclean) words <- lma_dict("special", as.function = gsub)(words)
          TWC <- length(words)
          if (segment.size == -1) segment.size <- ceiling(TWC / segment)
          if (bysentence) {
            if (!is.null(segment) && is.numeric(segment)) {
              lines <- character(segment)
              WC <- numeric(segment)
            } else {
              lines <- NULL
              WC <- NULL
            }
            sentends <- grep("[.?!]$", if (end_in_quotes) {
              gsub(if (preclean) {
                '["\']+'
              } else {
                paste0("(?:", paste(
                  c(
                    '["\']',
                    unlist(lma_dict("special")$special$CHARACTERS[c('"', "'")])
                  ),
                  collapse = "|"
                ), ")+$")
              }, "", words)
            } else {
              words
            })
            if (length(sentends)) {
              sentends <- sentends[!grepl(paste0(
                "\\.[a-z]|^(?:[a-z]|[ivxm]+|\\d+|ans|govt|apt|etc|",
                "st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof)[.?!]$"
              ), words[sentends], TRUE, TRUE)]
            }
            sentends <- c(1, sentends)
            nsents <- length(sentends)
            if (sentends[nsents] != TWC) {
              sentends <- c(sentends, TWC)
              nsents <- nsents + 1
            }
            i <- s <- p <- 1
            while (p < nsents && sum(WC) < TWC) {
              WC[i] <- 0
              while (p < nsents && WC[i] < segment.size) {
                p <- p + 1
                WC[i] <- (sentends[p] - s) + 1
              }
              lines[i] <- paste(words[seq(s, sentends[p])], collapse = " ")
              s <- sentends[p] + 1
              i <- i + 1
            }
          } else {
            segment <- ceiling(TWC / segment.size)
            lines <- character(segment)
            WC <- rep(segment.size, segment)
            WCC <- 0
            for (i in seq_len(segment)) {
              if (WCC + WC[i] > TWC) WC[i] <- TWC - WCC
              lines[i] <- paste(words[seq(WCC + 1, WCC + WC[i])], collapse = " ")
              WCC <- WCC + WC[i]
            }
          }
        } else {
          lines <- tryCatch(do.call(scan, args), error = err)
          if (!length(lines)) {
            return(NULL)
          }
        }
      } else {
        lines <- ""
      }
      data.frame(
        input = if (ck_text) fi else f, segment = seq_along(lines),
        WC = if (is.null(WC)) vapply(strsplit(lines, "\\s+"), function(sp) sum(sp != ""), 0) else WC,
        text = lines, stringsAsFactors = FALSE
      )
    }))
  } else {
    warning(
      "no files found", if (!subdir) "; might try setting subdir to TRUE to include files in folders"
    )
  }
}
