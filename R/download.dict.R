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
#' @param overwrite Logical; if \code{TRUE}, will replace existing files.
#' @return Path to the downloaded dictionary, or a list of such if multiple were downloaded.
#' @family Dictionary functions
#' @examples
#' \dontrun{
#'
#' download.dict("lusi", dir = "~/Dictionaries")
#' }
#' @export

download.dict <- function(
    dict = "lusi", check.md5 = TRUE, mode = "wb", dir = getOption("lingmatch.dict.dir"),
    overwrite = FALSE) {
  download.resource(
    "dict", dict,
    check.md5 = check.md5, mode = mode, dir = dir, overwrite = overwrite
  )
}
