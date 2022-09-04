#' Initialize Directories for Dictionaries and Latent Semantic Spaces
#'
#' Creates directories for dictionaries and latent semantic spaces if needed, sets them as the
#' \cr \code{lingmatch.dict.dir} and \code{lingmatch.lspace.dir} options if they are not already set,
#' and creates links to them in their expected locations (\code{'~/Dictionaries'} and
#' \code{'~/Latent Semantic Spaces'}) by default if applicable.
#' @param base Path to a directory in which to create the \code{dict} and \code{lspace} subdirectories.
#' @param dict Path to the dictionaries directory relative to \code{base}.
#' @param lspace Path to the latent semantic spaces directory relative to \code{base}.
#' @param link Logical; if \code{TRUE} (default), the full \code{dict} and/or \code{lspace} paths exist
#' (potentially after being created), and they are not \code{'~/Dictionaries'} or \code{'~/Latent Semantic Spaces'}
#' respectively, junctions (Windows) or symbolic links will be created: \code{~/Dictionaries} \code{<<===>>}
#' \code{dict} and \code{~/Latent Semantic Spaces} \code{<<===>>} \code{lspace}.
#' @return Paths to the [1] dictionaries and [2] latent semantic space directories, or a single path if only
#' \code{dict} or \code{lspace} is specified.
#' @examples
#' \dontrun{
#'
#' # set up the expected dictionary and latent semantic space directories
#' lma_initdirs("~")
#'
#' # set up directories elsewhere, and links to the expected locations
#' lma_initdirs("d:")
#'
#' # point options and create links to preexisting directories
#' lma_initdirs("~/NLP_Resources", "Dicts", "Dicts/Embeddings")
#'
#' # create just a dictionaries directory and set the
#' # lingmatch.dict.dir option without creating a link
#' lma_initdirs(dict = "z:/external_dictionaries", link = FALSE)
#' }
#' @export

lma_initdirs <- function(base = "", dict = "Dictionaries", lspace = "Latent Semantic Spaces", link = TRUE) {
  mck <- c(missing(dict), missing(lspace))
  if (base == "" && all(mck)) {
    base <- gsub('^[\'"]+|[\'"]+$', "", readline(paste0(
      "Enter the path to a directory; ~ is recommended: \n",
      "This is where ", dict, " and ", lspace, " subdirectories will be made."
    )))
    if (grepl("^(?:cancel|exit|stop|q|x|quit|no|nvm|nevermind)?$", tolower(base))) {
      stop(
        "Specify a path to a directory in which you want dictionaries",
        ' and latent semantic spaces to be stored; e.g., "~".',
        call. = FALSE
      )
    }
  }
  if (base == "") {
    dirs <- normalizePath(c(dict, lspace), "/", FALSE)
    names(dirs) <- c("dict", "lspace")
    if (!all(mck)) dirs <- dirs[which(!mck)]
  } else {
    dirs <- normalizePath(paste0(base, if (base != "") "/", c(dict, lspace)), "/", FALSE)
    names(dirs) <- c("dict", "lspace")
    if (!all(mck)) dirs <- dirs[!mck]
  }
  if ("dict" %in% names(dirs)) {
    if (!dir.exists(dirs[["dict"]])) dir.create(dirs[["dict"]], recursive = TRUE)
    if (getOption("lingmatch.dict.dir") == "") options(lingmatch.dict.dir = dirs[["dict"]])
  }
  if ("lspace" %in% names(dirs)) {
    if (!dir.exists(dirs[["lspace"]])) dir.create(dirs[["lspace"]], recursive = TRUE)
    if (getOption("lingmatch.lspace.dir") == "") options(lingmatch.lspace.dir = dirs[["lspace"]])
  }
  if (link) {
    linker <- if (Sys.info()[["sysname"]] == "Windows") Sys.junction else file.symlink
    if (dir.exists(dirs[["dict"]]) && !dir.exists("~/Dictionaries")) {
      linker(dirs[["dict"]], "~/Dictionaries")
      message("created dictionaries link:\n  ", dirs[["dict"]], " <<==>> ", path.expand("~/Dictionaries"))
    }
    if (dir.exists(dirs[["lspace"]]) && !dir.exists("~/Latent Semantic Spaces")) {
      linker(dirs[["lspace"]], "~/Latent Semantic Spaces")
      message("created latent space link:\n  ", dirs[["lspace"]], " <<==>> ", path.expand("~/Latent Semantic Spaces"))
    }
  }
  dirs
}
