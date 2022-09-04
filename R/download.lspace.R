#' Download Latent Semantic Spaces
#'
#' Downloads the specified semantic space from \href{https://osf.io/489he}{osf.io/489he}.
#'
#' @param space Name of one or more spaces you want to download, or \code{'all'} for all available.
#'  \code{'100k_lsa'} is the default, and some other common options might be \code{'google'}, \code{'facebook'},
#'  or \code{'glove'}. See \href{https://osf.io/489he/wiki/home}{osf.io/489he/wiki} for more information,
#'  and a full list of spaces.
#' @param include.terms Logical; if \code{FALSE}, only the \code{.dat.bz2} file is downloaded
#'  (which only has numeric vectors).
#' @param decompress Logical; if \code{TRUE} (default), decompresses the downloaded file
#'  with the \code{bunzip2} system command assuming it is available \cr (as indicated by
#'  \code{Sys.which('bunzip2')}).
#' @param check.md5 Logical; if \code{TRUE} (default), retrieves the MD5 checksum from OSF,
#'  and compares it with that calculated from the downloaded file to check its integrity.
#' @param mode A character specifying the file write mode; default is 'wb'. See
#'  \code{\link{download.file}}.
#' @param dir Directory in which to save the space. Specify this here, or set the lspace directory option
#'  (e.g., \code{options(lingmatch.lspace.dir = '~/Latent Semantic Spaces')}), or use
#'  \code{\link{lma_initdirs}} to initialize a directory.
#' @family Latent Semantic Space functions
#' @return A character vector with paths to the [1] data and [2] term files.
#' @examples
#' \dontrun{
#'
#' download.lspace("glove_crawl", dir = "~/Latent Semantic Spaces")
#' }
#' @export
#' @importFrom utils download.file
#' @importFrom tools md5sum

download.lspace <- function(space = "100k_lsa", include.terms = TRUE, decompress = TRUE,
                            check.md5 = TRUE, mode = "wb", dir = getOption("lingmatch.lspace.dir")) {
  if (dir == "") {
    stop(paste(
      "specify a directory (dir), or set the lspace directory option\n(e.g., options(lingmatch.lspace.dir =",
      '"~/Latent Semantic Spaces")) or initialize it with lma_initdirs'
    ), call. = FALSE)
  }
  if (length(space) == 1 && space == "all") space <- rownames(select.lspace()$info)
  if (length(space) > 1) {
    res <- lapply(space, function(d) {
      m <- tryCatch(download.lspace(d,
        include.terms = include.terms, decompress = decompress,
        check.md5 = check.md5, mode = mode, dir = dir
      ), error = function(e) e$message)
      if (is.null(m)) "downloaded" else paste("failed: ", m)
    })
    names(res) <- space
    return(res)
  }
  dir <- normalizePath(dir, "/", FALSE)
  if (space == "default") space <- "100k_lsa"
  name <- grep(paste0("^", sub("\\..*$", "", space)), rownames(lss_info), value = TRUE)
  if (!length(name)) name <- grep(paste0("^", substr(space, 1, 4)), rownames(lss_info), TRUE, value = TRUE)
  if (!length(name)) {
    stop("space ", space, " not recognized; see https://osf.io/489he/wiki for available spaces")
  } else {
    name <- name[1]
  }
  urls <- list(
    info = function(id) paste0("https://api.osf.io/v2/files/", id),
    dl = function(id) paste0("https://osf.io/download/", id),
    versions = function(id) paste0("https://osf.io/", id, "/?show=revision")
  )
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dl <- function(id, ext, ck) {
    s <- urls$dl(id)
    o <- normalizePath(paste0(dir, "/", name, ext), "/", FALSE)
    status <- tryCatch(download.file(s, o, mode = mode), error = function(e) 1)
    if (!status && check.md5) {
      fi <- strsplit(readLines(urls$info(id), 1, TRUE, FALSE, "utf-8"), '[:,{}"]+')[[1]]
      ck <- md5sum(o)
      if (fi[which(fi == "md5") + 1] != ck) {
        warning(paste0(
          "MD5 (", ck, ") does not seem to match the one on record;\n",
          "double check and try manually downloading at ", urls$versions(id)
        ))
      }
    }
    if (status) warning("failed to download file from ", s, call. = FALSE)
    status
  }
  status <- if (include.terms) dl(lss_info[name, "osf_terms"], "_terms.txt", check.md5) else 0
  if (!status) status <- dl(lss_info[name, "osf_dat"], ".dat.bz2", check.md5)
  if (!status && decompress) {
    if (Sys.which("bunzip2") == "") {
      warning("could not find path to bunzip2 command for decompression")
    } else {
      o <- normalizePath(paste0(dir, "/", name, ".dat.bz2"), "/", FALSE)
      status <- tryCatch(system2("bunzip2", shQuote(path.expand(o))), error = function(e) 1)
      if (status) {
        warning(
          'failed to decompress; might try this from a system console:\n  bunzip2 "', path.expand(o), '"'
        )
      }
    }
  }
  dir <- paste0(dir, "/", name, c(if (!status && decompress) {
    ".dat"
  } else {
    ".dat.bz2"
  }, "_terms.txt"), collapse = "\n  ")
  message("downloaded ", name, " space:\n  ", dir)
  invisible(dir)
}
