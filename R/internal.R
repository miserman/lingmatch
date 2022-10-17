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
