#' Download Dictionaries
#'
#' Downloads the specified dictionaries from \href{https://osf.io/y6g5b}{osf.io/y6g5b}.
#'
#' @param dict One or more names of dictionaries to download, or \code{'all'} for all available. See
#'  \href{https://osf.io/y6g5b/wiki/home}{osf.io/y6g5b/wiki} for more information, and a list of available dictionaries.
#' @param check.md5 Logical; if \code{TRUE} (default), retrieves the MD5 checksum from OSF,
#'  and compares it with that calculated from the downloaded file to check its integrity.
#' @param mode A character specifying the file write mode; default is 'wb'. See
#'  \code{\link{download.file}}.
#' @param dir Directory in which to save the dictionary; \cr default is \code{getOption('lingmatch.dict.dir')}. \cr
#'  This must be specified, or the option must be set -- use \code{\link{lma_initdirs}} to initialize a directory.
#' @return Path to the downloaded dictionary, or a list of such if multiple were downloaded.
#' @family Dictionary functions
#' @examples
#' \dontrun{
#'
#' download.dict("lusi", dir = "~/Dictionaries")
#' }
#' @export

download.dict <- function(dict = "lusi", check.md5 = TRUE, mode = "wb", dir = getOption("lingmatch.dict.dir")) {
  if (dir == "") {
    stop(paste(
      'specify a dir, or set the dict directory option\n(e.g., options(lingmatch.dict.dir = "~/Dictionaries"))',
      " or initialize it with lma_initdirs"
    ), call. = FALSE)
  }
  if (length(dict) == 1 && dict == "all") dict <- rownames(select.dict()$info)
  if (length(dict) > 1) {
    res <- lapply(dict, function(d) {
      tryCatch(
        download.dict(d, check.md5 = check.md5, mode = mode, dir = dir),
        error = function(e) paste("failed:", e$message)
      )
    })
    names(res) <- dict
    return(res)
  }
  dir <- normalizePath(dir, "/", FALSE)
  name <- grep(paste0("^", sub("\\.[^.]*$", "", dict)), rownames(dict_info), value = TRUE)
  if (!length(name)) name <- grep(paste0("^", substr(dict, 1, 6)), rownames(dict_info), TRUE, value = TRUE)
  if (!length(name)) {
    stop("dictionary ", dict, " not recognized; see https://osf.io/y6g5b/wiki for available dictionaries")
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
  type <- if (dict_info[name, "weighted"]) ".csv" else ".dic"
  status <- dl(dict_info[name, "osf"], type, check.md5)
  dir <- paste0(dir, "/", name, type, collapse = "\n  ")
  message("downloaded ", name, " dictionary:\n  ", dir)
  invisible(dir)
}
