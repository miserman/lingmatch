.onLoad <- function(lib, pkg) {
  if (is.null(getOption("lingmatch.lspace.dir"))) options(lingmatch.lspace.dir = "")
  if (is.null(getOption("lingmatch.dict.dir"))) options(lingmatch.dict.dir = "")
}

match_metric <- function(x) {
  mets <- c("jaccard", "euclidean", "canberra", "cosine", "pearson")
  sel <- if (is.null(x) || (length(x) == 1 && grepl(tolower(substr(x, 1, 1)), "a", fixed = TRUE))) {
    mets
  } else if (is.function(x)) {
    stop("only internal metrics are available: ", paste(mets, collapse = ", "), call. = FALSE)
  } else {
    if (is.numeric(x)) {
      mets[x]
    } else {
      if (is.call(x)) x <- eval(x)
      su <- grepl("^(?:cor|r)", x, TRUE)
      if (any(su)) x[su] <- "pearson"
      unique(unlist(lapply(substr(x, 1, 3), grep, mets, fixed = TRUE, value = TRUE)))
    }
  }
  list(all = mets, selected = sel, dummy = as.integer(mets %in% sel))
}

to_regex <- function(dict, intext = FALSE, isGlob = TRUE) {
  lapply(dict, function(l) {
    l <- gsub("([+*])[+*]+", "\\\\\\1+", sub("(?<=[^\\\\])\\\\$", "\\\\\\\\", l, perl = TRUE))
    if (isGlob) {
      l <- gsub("([.^$?(){}[-]|\\])", "\\\\\\1", l, perl = TRUE)
      if (!intext) l <- gsub("\\^\\*|\\*\\$", "", paste0("^", l, "$"))
      l <- gsub("\\*", "[^\\\\s]*", l)
    } else if (any(ck <- grepl("[[({]", l) + grepl("[})]|\\]", l) == 1)) {
      l[ck] <- gsub("([([{}\\])])", "\\\\\\1", l[ck], perl = TRUE)
    }
    l
  })
}

download.resource <- function(
    type, resource, include.terms = TRUE, decompress = TRUE,
    check.md5 = TRUE, mode = "wb", dir = "", overwrite = FALSE) {
  if (dir == "") {
    stop(paste0(
      "specify a directory (dir), or set the ", type,
      " directory option\n(e.g., options(lingmatch.", type, ".dir = ",
      '"~/', if (type == "dict") "Dictionaries" else "Latent Semantic Space",
      '")) or initialize it with lma_initdirs'
    ), call. = FALSE)
  }
  all_resources <- rownames(if (type == "dict") dict_info else lss_info)
  if (length(resource) == 1 && resource == "all") resource <- all_resources
  if (length(resource) > 1) {
    return(lapply(structure(resource, names = resource), function(d) {
      tryCatch(
        download.resource(
          type = type, resource = d, include.terms = include.terms, decompress = decompress,
          check.md5 = check.md5, mode = mode, dir = dir
        ),
        error = function(e) e$message
      )
    }))
  }
  dir <- normalizePath(dir, "/", FALSE)
  if (resource == "default") resource <- if (type == "dict") "lusi" else "100k_lsa"
  name <- grep(paste0("^", sub("\\..*$", "", resource)), all_resources, value = TRUE)
  if (!length(name)) {
    name <- grep(
      paste0("^", substr(resource, 1, 4)), all_resources, TRUE,
      value = TRUE
    )
  }
  if (!length(name)) {
    stop(
      type, " ", resource, " not recognized; see https://osf.io/",
      if (type == "dict") "y6g5b" else "489he", "/wiki for available resources"
    )
  } else {
    name <- name[1]
  }
  urls <- list(
    info = function(id) paste0("https://api.osf.io/v2/files/", id),
    dl = function(id) paste0("https://osf.io/download/", id),
    versions = function(id) paste0("https://osf.io/", id, "/?show=revision")
  )
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dl <- function(id, ext) {
    s <- urls$dl(id)
    o <- unique(normalizePath(paste0(
      dir, "/", name, c(ext, sub(".bz2", "", ext, fixed = TRUE))
    ), "/", FALSE))
    if (any(file.exists(o))) {
      if (overwrite) {
        unlink(o)
      } else {
        return(-1)
      }
    }
    status <- tryCatch(download.file(s, o[[1]], mode = mode), error = function(e) 1)
    if (!status && check.md5) {
      fi <- strsplit(readLines(urls$info(id), 1, TRUE, FALSE, "utf-8"), '[:,{}"]+')[[1]]
      ck <- md5sum(o[[1]])
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
  if (type == "lspace") {
    status <- if (include.terms) dl(lss_info[name, "osf_terms"], "_terms.txt") else 0
    if (status < 1) status <- dl(lss_info[name, "osf_dat"], ".dat.bz2")
    if (status < 1 && decompress) {
      if (Sys.which("bunzip2") == "") {
        warning("could not find path to bunzip2 command for decompression")
      } else {
        o <- normalizePath(paste0(dir, "/", name, ".dat.bz2"), "/", FALSE)
        if (file.exists(o)) {
          status <- tryCatch(system2("bunzip2", shQuote(path.expand(o))), error = function(e) 1)
          if (status) {
            warning(
              'failed to decompress; might try this from a system console:\n  bunzip2 "', path.expand(o), '"'
            )
          }
        }
      }
    }
    paths <- normalizePath(paste0(
      dir, "/", name, c(".dat", if (!decompress) ".bz2", "_terms.txt")
    ), "/", FALSE)
  } else {
    ext <- if (dict_info[name, "weighted"]) ".csv" else ".dic"
    status <- dl(dict_info[name, "osf"], ext)
    paths <- normalizePath(paste0(dir, "/", name, ext), "/", FALSE)
  }
  if (status < 1) {
    message(
      paste0(name, " ", type, " ", if (!status) "downloaded" else "exists", ":\n  "),
      paste(paths, collapse = "\n  ")
    )
  }
  invisible(paths)
}
