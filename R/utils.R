#' Process Text
#'
#' A wrapper to other pre-processing functions, potentially from \code{\link{read.segments}}, to \code{\link{lma_dtm}}
#' or \code{\link{lma_patcat}}, to \code{\link{lma_weight}}, then \code{\link{lma_termcat}} or \code{\link{lma_lspace}},
#' and optionally including \code{\link{lma_meta}} output.
#'
#' @param input A vector of text, or path to a text file or folder.
#' @param ... arguments to be passed to \code{\link{lma_dtm}}, \code{\link{lma_patcat}}, \code{\link{lma_weight}},
#'   \code{\link{lma_termcat}}, and/or \code{\link{lma_lspace}}. All arguments must be named.
#' @param meta Logical; if \code{FALSE}, metastatistics are not included. Only applies when raw text is available.
#'  If included, meta categories are added as the last columns, with names starting with "meta_".
#' @return A matrix with texts represented by rows, and features in columns, unless there are multiple rows per output
#'  (e.g., when a latent semantic space is applied without terms being mapped) in which case only the special output
#'  is returned (e.g., a matrix with terms as rows and latent dimensions in columns).
#' @seealso If you just want to compare texts, see the \code{\link{lingmatch}} function.
#' @examples
#' # starting with some texts in a vector
#' texts = c(
#'  'Firstly, I would like to say, and with all due respect...',
#'  'Please, proceed. I hope you feel you can speak freely...',
#'  "Oh, of course, I just hope to be clear, and not cause offense...",
#'  "Oh, no, don't monitor yourself on my account..."
#' )
#'
#' # by default, term counts and metastatistics are returned
#' lma_process(texts)
#'
#' # add dictionary and percent arguments for standard dictionary-based results
#' lma_process(texts, dict = lma_dict(), percent = TRUE)
#'
#' # add space and weight arguments for standard word-centroid vectors
#' lma_process(texts, space = lma_lspace(texts), weight = 'tfidf')
#' @export

lma_process = function(input = NULL, ..., meta = TRUE){
  inp = as.list(substitute(...()))
  funs = c('read.segments', 'lma_dtm', 'lma_weight', 'lma_lspace', 'lma_termcat', 'lma_patcat')
  arg_matches = lapply(funs, function(f){
    a = names(as.list(args(f)))
    a = a[-c(1, length(a))]
    inp[a[a %in% names(inp)]]
  })
  names(arg_matches) = funs
  # identify input
  op = NULL
  if(is.function(input)) stop('enter a character vector or matrix-like object as input')
  if(is.character(input) || is.factor(input)){
    ck_paths = length(input) != 1 && all(file.exists(input))
    op = if(length(arg_matches$read.segments) || ck_paths){
      an = names(arg_matches$read.segments)
      if(!any(grepl('path|text', an))) arg_matches$read.segments$path = input
      do.call(read.segments, arg_matches$read.segments)
    }else data.frame(
      text = if(length(input) == 1 && file.exists(input)) readLines(input) else input
    )
  }else{
    if(is.null(dim(input))) input = as.data.frame(input)
    op = input
  }
  # process
  ck_text = 'text' %in% colnames(op)
  ck_changed = FALSE
  if(ck_text){
    if(!length(arg_matches$lma_dtm) && length(arg_matches$lma_patcat) &&
        any(!names(arg_matches$lma_patcat) %in% names(arg_matches$lma_termcat))){
      if(!'return.dtm' %in% names(arg_matches$lma_patcat) && length(arg_matches$lma_weight))
        arg_matches$lma_patcat$return.dtm = TRUE
      arg_matches$lma_patcat$text = op[, 'text']
      x = do.call(lma_patcat, arg_matches$lma_patcat)
      ck_changed = TRUE
    }else{
      arg_matches$lma_dtm$text = op[, 'text']
      x = do.call(lma_dtm, arg_matches$lma_dtm)
      ck_changed = TRUE
    }
  }else x = op
  if(length(arg_matches$lma_weight)){
    arg_matches$lma_weight$dtm = x
    x = do.call(lma_weight, arg_matches$lma_weight)
    attr(x, 'categories') = attr(arg_matches$lma_weight$dtm, 'categories')
    ck_changed = TRUE
  }
  if(!is.null(attr(x, 'categories'))){
    categories = attr(x, 'categories')
    xc = as.data.frame(matrix(0, nrow(op), length(categories), dimnames = list(NULL, names(categories))))
    for(cat in names(categories)) xc[, cat] = rowSums(x[, categories[[cat]], drop = FALSE], na.rm = TRUE)
    x = xc
    ck_changed = TRUE
  }else if(length(arg_matches$lma_termcat)){
    arg_matches$lma_termcat$dtm = x
    x = do.call(lma_termcat, arg_matches$lma_termcat)
    ck_changed = TRUE
  }
  if(length(arg_matches$lma_lspace)){
    nr = NROW(x)
    arg_matches$lma_lspace$dtm = x
    x = do.call(lma_lspace, arg_matches$lma_lspace)
    colnames(x) = paste0('dim', seq_len(ncol(x)))
    if(nrow(x) != nr) return(x)
    ck_changed = TRUE
  }
  if(any(grepl('Matrix', class(x), fixed = TRUE))) x = as.matrix(x)
  if(is.matrix(x)) x = as.data.frame(x)
  op = if(ck_text && ck_changed) cbind(op, x) else x
  if(ck_text && meta){
    opm = lma_meta(op[, 'text'])
    if(length(arg_matches$lma_weight) &&
      (!'normalize' %in% names(arg_matches$lma_weight) || arg_matches$lma_weight$normalize)){
      cols = c(9, 14:23)
      opm_counts = opm[, cols]
      su = opm_counts != 0
      adj = if('percent' %in% names(arg_matches$lma_weight) && arg_matches$lma_weight$percent) 100 else 1
      opm_counts[su] = opm_counts[su] / rep(opm$words, length(cols))[which(su)] * adj
      opm[, cols] = opm_counts
    }
    colnames(opm) = paste0('meta_', colnames(opm))
    op = cbind(op, opm)
  }
  op
}

to_regex = function(dict, intext = FALSE){
  lapply(dict, function(l){
    l = gsub('([+*])[+*]+', '\\\\\\1+', l)
    if(any(ck <- grepl('[[({]', l) + grepl('[})]|\\]', l) == 1))
      l[ck] = gsub('([([{}\\])])', '\\\\\\1', l[ck], perl = TRUE)
    if(intext){
      sub('^\\*', '\\\\\\b\\\\\\w*', sub('\\*$', '\\\\\\w*\\\\\\b', l, TRUE), TRUE)
    }else{
      gsub('\\^\\*|\\*\\$', '', paste0('^', l, '$'))
    }
  })
}

#' Read/Write Dictionary Files
#'
#' Read in or write dictionary files in Comma-Separated Values (.csv; weighted) or
#' Linguistic Inquiry and Word Count (.dic; non-weighted) format.
#' @param path Path to a file, a name corresponding to a file in \code{getOption('lingmatch.dict.dir')} or
#'   one of the dictionaries available at \href{https://osf.io/y6g5b}{osf.io/y6g5b}, a matrix-like object
#'   to be categorized, or a list to be formatted.
#' @param cats A character vector of category names to be returned. All categories are returned by default.
#' @param type A character indicating whether and how terms should be altered. Unspecified or matching 'asis'
#'   leaves terms as they are. Other options change wildcards to regular expressions:
#'   \code{'pattern'} (\code{'^[poi]'}) replaces initial asterisks with \code{'\\\\b\\\\w*'},
#'   and terminal asterisks with \code{'\\\\w*\\\\b'}, to match terms within raw text;
#'   for anything else, terms are padded with '^' and '$', then those bounding marks are removed
#'   when an asterisk is present, to match tokenized terms.
#' @param as.weighted Logical; if \code{TRUE}, prevents weighted dictionaries from being converted to
#'   unweighted versions, or converts unweighted dictionaries to a binary weighted version
#'   -- a data.frame with a "term" column of unique terms, and a column for each category.
#' @param dir Path to a folder containing dictionaries, or where you would like dictionaries to be downloaded;
#'   passed to \code{\link{select.dict}} and/or \code{\link{download.dict}}.
#' @param ... Passes arguments to \code{\link{readLines}}.
#' @return \code{read.dic}: A \code{list} (unweighted) with an entry for each category containing
#'   character vectors of terms, or a \code{data.frame} (weighted) with columns for terms (first, "term") and
#'   weights (all subsequent, with category labels as names).
#' @family Dictionary functions
#' @importFrom utils read.table write.table
#' @export

read.dic = function(path, cats, type = 'asis', as.weighted = FALSE, dir = getOption('lingmatch.dict.dir'), ...){
  if(missing(path)) path = file.choose()
  if(is.character(path) && !any(file.exists(path)) && any(file.exists(paste0(dir, '/', path))))
    path = paste0(dir, '/', path)
  if(is.character(path) && !any(file.exists(path))){
    tp = select.dict(path, dir = dir)
    if(nrow(tp$selected) && length(path) <= nrow(tp$info)){
      if(any(tp$selected$downloaded == '')){
         td = rownames(tp$selected)[tp$selected$downloaded == '']
         if(grepl('^$|^[yt1]|^ent', readline(paste0(
           'would you like to download ', if(length(td) == 1) 'this dictionary' else 'these dictionaries', '?:\n',
           sub(',(?=[^,]+$)', if(length(td) == 2) ' and' else ', and', paste0(td, collapse = ', '), perl = TRUE),
           '\n(press Enter for yes): '
         )))) tp$selected[td, 'downloaded'] = download.dict(td, dir = dir)
      }
      path = tp$selected[tp$selected[, 'downloaded'] != '', 'downloaded']
      if(!length(path)) stop('none of the selected dictionaries are downloaded')
    }
  }
  if(is.character(path) && length(path) > 1 && any(file.exists(path))){
    dicts = list()
    for(p in path) if(file.exists(p)){
      name = gsub('^.*[/\\]+|\\.[^.]+$', '', p)
      dicts[[name]] = read.dic(p, dir = dir)
    }
    path = if(length(dicts) == 1) dicts[[1]] else dicts
  }
  if(!is.null(dim(path))){
    if(anyNA(path)) path[is.na(path)] = 0
    cats = colnames(path)
    if('term' %in% cats){
      terms = path[, 'term']
      cats = cats[cats != 'term']
    }else if(!is.null(rownames(path)) && any(grepl('[a-z]', rownames(path), TRUE))){
      terms = rownames(path)
    }else{
      su = which(vapply(cats, function(cat) !is.numeric(path[, cat]), TRUE))
      if(!length(su)){
        if(!is.null(colnames(path))){
          path = data.frame(term = colnames(path), t(path))
          terms = path$term
          cats = colnames(path)[-1]
          if(missing(as.weighted)) as.weighted = TRUE
        }else stop('no non-numeric columns found in path')
      }else{
        if(length(su) > 1){
          ssu = vapply(su, function(col) !anyDuplicated(path[, col]), TRUE)
          if(any(!ssu)) cats = path[, su[which(!ssu)[[1]]]]
          su = if(any(ssu)) su[which(ssu)[[1]]] else su[[1]]
        }
        terms = path[, su]
      }
    }
    if('category' %in% colnames(path)) cats = path[, 'category']
    if(length(cats) == nrow(path)){
      wl = split(terms, cats)
    }else{
      su = vapply(cats, function(col) !is.numeric(path[, col]) && anyDuplicated(path[, col]), TRUE)
      wl = if(any(su)){
        split(terms, path[, names(which(su))[[1]]])
      }else{
        cats = cats[vapply(cats, function(cat) is.numeric(path[, cat]), TRUE)]
        if(!length(cats)) stop('no numeric columns found in path')
        if(as.weighted){
          cbind(term = terms, path[, cats, drop = FALSE])
        }else{
          if(length(cats) == 1){
            weights = path[, cats]
            if(any(weights < 0) && any(weights > 0)){
              Filter(length, list(
                positive = terms[weights > 0],
                neutral = terms[weights == 0],
                negative = terms[weights < 0]
              ))
            }else if(anyDuplicated(weights)) split(terms, weights) else list(category = terms)
          }else{
            weights = as.data.frame(path[, cats])
            if(any(weights > 0) && any(weights < 0)){
              for(cat in cats){
                if(any(weights[, cat] > 0)) weights[, paste0(cat, '.positive')] = as.integer(weights[, cat] > 0)
                if(any(weights[, cat] == 0)) weights[, paste0(cat, '.neutral')] = as.integer(weights[, cat] == 0)
                if(any(weights[, cat] < 0)) weights[, paste0(cat, '.negative')] = as.integer(weights[, cat] < 0)
                weights = weights[, colnames(weights) != cat]
              }
              cats = sort(colnames(weights))
            }
            lvs = sort(unique(unlist(weights)))
            if(length(lvs) == 2 && all(lvs == c(0, 1))){
              wl = lapply(cats, function(cat) terms[weights[, cat] == 1])
              names(wl) = cats
              wl = Filter(length, wl)
            }else{
              wl = list()
              for(r in seq_len(nrow(path))){
                m = max(weights[r,])
                if(m != 0) for(cat in cats[weights[r,] == m]) wl[[cat]] = c(wl[[cat]], terms[r])
              }
            }
            wl
          }
        }
      }
    }
  }else if(is.list(path)){
    if(all(vapply(path, function(d) is.character(d) || is.factor(d), TRUE))){
      wl = path
      if(is.null(names(wl))) names(wl) = paste0('cat', seq_along(wl))
      if(!missing(cats)){
        wl = wl[names(wl) %in% cats]
        if(!length(wl)) stop('no cats were found in path')
      }
    }else{
      if(is.null(names(path))) names(path) = paste0('dict', seq_along(path))
      wl = if(any(vapply(path, function(d) !is.null(dim(d)), TRUE))){
        terms = NULL
        cats = NULL
        for(d in names(path)){
          if(is.null(dim(path[[d]]))) path[[d]] = read.dic(path[[d]], as.weighted = TRUE)
          if(!'term' %in% colnames(path[[d]])) path[[d]] = read.dic(path[[d]])
          terms = unique(c(terms, path[[d]]$term))
          cats = c(cats, paste0(d, '.', colnames(path[[d]])[-1]))
        }
        wl = as.data.frame(matrix(0, length(terms), length(cats), dimnames = list(terms, cats)))
        for(d in names(path)){
          cats = colnames(path[[d]])[-1] = paste0(d, '.', colnames(path[[d]])[-1])
          su = duplicated(path[[d]]$term)
          if(any(su)){
            su = path[[d]]$term %in% path[[d]]$term[su]
            td = path[[d]][su,, drop = FALSE]
            for(term in unique(td$term)) wl[term, cats] = colMeans(td[td$term == term, cats])
          }
          if(any(!su)) path[[d]] = path[[d]][!su,]
          rownames(path[[d]]) = path[[d]]$term
          wl[path[[d]]$term, cats] = path[[d]][, cats]
        }
        data.frame(term = rownames(wl), wl)
      }else if(any(vapply(path, is.list, TRUE))){
        do.call(c, path)
      }else{
        if(any(vapply(path, function(d) is.null(names(d)), TRUE))){
          if(all(vapply(path, length, 0) == length(path[[1]]))){
            data.frame(term = names(path), do.call(rbind, path))
          }else stop('failed to resolve path; as a list, entries should contain character or named numeric vectors')
        }else{
          terms = unique(unlist(lapply(path, names), use.names = FALSE))
          v = structure(numeric(length(terms)), names = terms)
          data.frame(term = terms, vapply(path, function(d){
            v[names(d)] = d
            v
          }, numeric(length(terms))))
        }
      }
    }
  }else{
    di = if(length(path) != 1) path else if(file.exists(path)) readLines(path, warn = FALSE, ...) else
      stop('assumed path is to a file, but ', path, ' it does not exist', call. = FALSE)
    lst = grep('%', di, fixed = TRUE)
    if(length(lst) > 1 && !grepl(',', di[lst[1]], fixed = TRUE)){
      if(length(lst) < 2) stop('could not identify the end of the header -- ',
        'this should be the second percent sign (%) following the last category name.')
      lst = lst[2]
      h = grep('^\\d', gsub('^\\s+|\\s*%+\\s*|\\s+$', '', di[seq_len(lst)]), value = TRUE)
      ci = character()
      for(cat in h) ci[sub('\\s.*$', '', cat)] = sub('^[^\\s]+\\s+', '', cat)
      if(missing(cats)) cats = ci
      sep = if(grepl('\t', di[lst + 1], fixed = TRUE)) '\t' else '\\s'
      cb = paste0('(?:', sep, '+(?:', paste(names(ci), collapse = '|'), ')(?=', sep, '|$))*$')
      di = di[-seq_len(lst - 1)]
      wl = lapply(structure(names(ci), names = ci), function(cat){
        unique(sub(cb, '', di[grep(paste0(sep, cat, cb), di, perl = TRUE)], perl = TRUE))
      })
      wl = wl[cats[cats %in% names(wl)]]
    }else{
      if(missing(as.weighted) && length(path) == 1) as.weighted = TRUE
      wl = if(any(grepl('[\\s,]', di))) tryCatch(read.dic(
        read.table(text = di, header = TRUE, sep = if(grepl('\t', di[[1]])) '\t' else ',',
          quote = '"', comment.char = ''), cats = cats, type = type, as.weighted = as.weighted
      ), error = function(e) e$message) else list(cat1 = di)
      if(length(wl) == 1 && is.character(wl))
        stop('assuming path is to a comma separated values file, but failed to read it in:\n', wl)
    }
  }
  if(!missing(type) && !grepl('^[Aa]', type)) wl = to_regex(wl, grepl('^[poi]', type, TRUE))
  if(as.weighted && is.null(dim(wl))){
    op = data.frame(term = unique(unlist(wl)))
    for(cat in names(wl)) op[, cat] = as.integer(op$term %in% wl[[cat]])
    op
  }else wl
}

#' @rdname read.dic
#' @param dict A \code{list} with a named entry of terms for each category, or a \code{data.frame}
#'   with terms in one column, and categories or weights in the rest.
#' @param filename The name of the file to be saved.
#' @param save Logical: if \code{FALSE}, does not write a file.
#' @return \code{write.dic}: A version of the written dictionary -- a raw character vector for
#'   unweighted dictionaries, or a \code{data.frame} for weighted dictionaries.
#' @examples
#' # make a small murder related dictionary
#' dict = list(
#'   kill = c('kill*', 'murd*', 'wound*', 'die*'),
#'   death = c('death*', 'dying', 'die*', 'kill*')
#' )
#'
#' # convert it to a weighted format
#' dict_weighted = read.dic(dict, as.weighted = TRUE)
#'
#' # categorize it back
#' read.dic(dict_weighted)
#'
#' \dontrun{
#'
#' # save it as a .dic file
#' write.dic(dict, 'murder')
#'
#' # read it back in as a list
#' read.dic('murder.dic')
#'
#' # read in the Moral Foundations or LUSI dictionaries from urls
#' moral_dict = read.dic('http://bit.ly/MoralFoundations2')
#' lusi_dict = read.dic('http://bit.ly/lusi_dict')
#'
#' # save and read in a version of the General Inquirer dictionary
#' inquirer = read.dic('inquirer')
#' }
#' @export

write.dic = function(dict, filename = 'custom', type = 'asis', as.weighted = FALSE, save = TRUE){
  if(!is.list(dict) || is.data.frame(dict)){
    if(save && (missing(as.weighted) || as.weighted)){
      as.weighted = TRUE
      o = dict
    }else dict = read.dic(dict)
  }
  if(is.null(dim(dict))){
    terms = unique(as.character(unlist(dict, use.names = FALSE)))
    terms = terms[terms != '']
    if(!missing(type) && !grepl('^[Aa]', type)) dict = to_regex(dict, grepl('^[poi]', type, TRUE))
    if(as.weighted){
      o = data.frame(term = terms)
      for(cat in names(dict)) o[, cat] = as.integer(o$term %in% dict[[cat]])
    }else{
      l = length(dict)
      m = as.data.frame(matrix('', length(terms) + l + 2, l + 1))
      m[, 1] = c('%', seq_len(l), '%', terms)
      m[seq_len(l) + 1, 2] = if(is.null(names(dict))) seq_len(l) else names(dict)
      for(i in seq_along(dict)) m[which(m[-seq_len(l + 2), 1] %in% dict[[i]]) + l + 2, i + 1] = i
      o = gsub('\t{2,}', '\t', paste(sub('\t+$', '', do.call(paste, c(m, sep = '\t'))), collapse = '\n'))
    }
  }
  if(save){
    filename = filename[[1]]
    filename = paste0(filename, if(!grepl('\\.[^.]+$', filename)) if(as.weighted) '.csv' else '.dic')
    if(as.weighted) write.table(o, filename, sep = ',', row.names = FALSE, qmethod = 'double') else
      write(o, filename)
    message('dictionary saved to ', filename)
  }
  invisible(o)
}

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
#' @param subdir Logical: if \code{TRUE}, files in folders in \code{path} will also be included.
#' @param segment.size Logical: if specified, \code{segment} will be ignored, and texts will be broken into segments containing
#'   roughly \code{segment.size} number of words.
#' @param bysentence A number: if \code{TRUE}, and \code{segment} is a number or \code{segment.size} is specified, sentences will
#'   be kept together, rather than potentially being broken across segments.
#' @param text A character vector with text to be split, used in place of \code{path}. Each entry is treates as a file.
#' @returns
#' A \code{data.frame} with columns for file names (\code{input}),
#' segment number within file (\code{segment}), word count for each segment (\code{WC}), and the text of
#' each segment (\code{text}).
#' @examples
#' # split preloaded text
#' read.segments('split this text into two segments', 2)
#'
#' # read in all files from the package directory
#' texts = read.segments(path.package('lingmatch'), ext = '')
#' texts[, -4]
#'
#' \dontrun{
#'
#' # segment .txt files in dir in a few ways:
#' dir = 'path/to/files'
#'
#' ## into 1 line segments
#' texts_lines = read.segments(dir)
#'
#' ## into 5 even segments each
#' texts_5segs = read.segments(dir, 5)
#'
#' ## into 50 word segments
#' texts_50words = read.segments(dir, segment.size = 50)
#'
#' ## into 1 sentence segments
#' texts_1sent = read.segments(dir, segment.size = 1, bysentence = TRUE)
#' }
#' @export

read.segments = function(path = '.', segment = NULL, ext = '.txt', subdir = FALSE, segment.size = -1,
  bysentence = FALSE, text = NULL){
  if(any(path == '')) path[path == ''] = '.'
  if(!any(file.exists(sub('[\\/]+$', '', path)))){
    ck_text = TRUE
    files = path
  }else{
    ck_text = !is.null(text)
    files = if(ck_text) text else{
      dirs = list.dirs(path, recursive = subdir)
      files = if(any(dir.exists(path)))
        unique(list.files(path, ext, recursive = subdir, full.names = TRUE)) else path[file.exists(path)]
      files[!files %in% dirs]
    }
  }
  if(missing(segment) && missing(segment.size))
    segment = if(length(path) == 1 && (ck_text || !dir.exists(path))) '\n' else 1
  if(length(files)){
    err = function(e) NULL
    args = list(what = character(), quote = '', na.strings = '', quiet = TRUE)
    if(is.character(segment) && segment.size == -1) args$sep = segment
    do.call(rbind, lapply(seq_along(files), function(fi){
      f = files[fi]
      args[[if(ck_text) 'text' else 'file']] = f
      WC = NULL
      if(ck_text || file.exists(f)){
        if(is.numeric(segment) || segment.size > 0){
          words = tryCatch(do.call(scan, args), error = err)
          if(!length(words)) return(NULL)
          TWC = length(words)
          if(segment.size == -1) segment.size = ceiling(TWC / segment)
          if(bysentence){
            if(!is.null(segment) && is.numeric(segment)){
              lines = character(segment)
              WC = numeric(segment)
            }else{
              lines = NULL
              WC = NULL
            }
            sentends = c(1, grep(paste0(
              '(?!<^(?:[a-z]+\\.[a-z.]+|\\d+|[a-z]|[iv]+|ans|govt|apt|etc|',
              'st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof))[.?!]+$'
            ), words, perl = TRUE))
            nsents = length(sentends)
            if(sentends[nsents] != TWC){
              sentends = c(sentends, TWC)
              nsents = nsents + 1
            }
            i = s = p = 1
            while(p < nsents && sum(WC) < TWC){
              WC[i] = 0
              while(p < nsents && WC[i] < segment.size){
                p = p + 1
                WC[i] = (sentends[p] - s) + 1
              }
              lines[i] = paste(words[seq(s, sentends[p])], collapse = ' ')
              s = sentends[p] + 1
              i = i + 1
            }
          }else{
            segment = ceiling(TWC / segment.size)
            lines = character(segment)
            WC = rep(segment.size, segment)
            WCC = 0
            for(i in seq_len(segment)){
              if(WCC + WC[i] > TWC) WC[i] = TWC - WCC
              lines[i] = paste(words[seq(WCC + 1, WCC + WC[i])], collapse = ' ')
              WCC = WCC + WC[i]
            }
          }
        }else{
          lines = tryCatch(do.call(scan, args), error = err)
          if(!length(lines)) return(NULL)
        }
      }else lines = ''
      data.frame(
        input = if(ck_text) fi else f, segment = seq_along(lines),
        WC = if(is.null(WC)) vapply(strsplit(lines, '\\s+'), function(sp) sum(sp != ''), 0) else WC,
        text = lines
      )
    }))
  }else warning(
    'no files found', if(!subdir) '; might try setting subdir to TRUE to include files in folders'
  )
}

#' Select Latent Semantic Spaces
#'
#' Retrieve information and links to latent semantic spaces
#' (sets of word vectors/embeddings) available at \href{https://osf.io/489he}{osf.io/489he},
#' and optionally download their term mappings (\href{https://osf.io/xr7jv}{osf.io/xr7jv}).
#'
#' @param query A character matching a space name, or a character vector of terms, used
#'   to select spaces. If length is over 1, \code{get.map} is set to \code{TRUE}.
#' @param dir Path to \code{lma_term_map.rda} and downloaded spaces.
#' @param get.map Logical; if \code{TRUE} and \code{lma_term_map.rda} is not found in
#'   \code{dir}, the term map (\href{https://osf.io/xr7jv}{lma_term_map.rda}) is
#'   downloaded and decompressed.
#' @param check.md5 Logical; if \code{TRUE} (default), retrieves the MD5 checksum from OSF,
#'   and compares it with that calculated from the downloaded file to check its integrity.
#' @param mode Passed to \code{\link{download.file}} when downloading the term map.
#' @return A list with varying entries:
#'   \tabular{ll}{
#'     info \tab The version of \href{https://osf.io/9yzca}{osf.io/9yzca} stored internally; a
#'       \code{data.frame}  with spaces as row names, and information about each space in columns:
#'         \tabular{ll}{
#'           \code{terms} \tab number of terms in the space \cr
#'           \code{corpus} \tab corpus(es) on which the space was trained \cr
#'           \code{model} \tab model from which the space was trained \cr
#'           \code{dimensions} \tab number of dimensions in the model (columns of the space) \cr
#'           \code{model_info} \tab some parameter details about the model \cr
#'           \code{original_max} \tab maximum value used to normalize the space; the original
#'             space would be \code{(vectors *} \code{original_max) /} \code{100} \cr
#'           \code{osf_dat} \tab OSF id for the \code{.dat} files; the URL would be
#'             https://osf.io/\code{osf_dat} \cr
#'           \code{osf_terms} \tab OSF id for the \code{_terms.txt} files; the URL would be
#'             https://osf.io/\code{osf_terms} \cr
#'           \code{wiki} \tab link to the wiki for the space \cr
#'           \code{downloaded} \tab path to the \code{.dat} file if downloaded,
#'             and \code{''} otherwise. \cr
#'         }
#'       \cr
#'     selected \tab A subset of \code{info} selected by \code{query}. \cr
#'     term_map \tab If \code{get.map} is \code{TRUE} or \code{lma_term_map.rda} is found in
#'       \code{dir}, a copy of \href{https://osf.io/xr7jv}{osf.io/xr7jv}, which has space names as
#'       column names, terms as row names, and indices as values, with 0 indicating the term is not
#'       present in the associated space. \cr
#'   }
#' @family Latent Semantic Space functions
#' @examples
#' # just retrieve information about available spaces
#' spaces = select.lspace()
#'
#' # retrieve all spaces that used word2vec
#' w2v_spaces = select.lspace('word2vec')$selected
#'
#' \dontrun{
#'
#' # select spaces by terms
#' select.lspace(c(
#'   'part-time', 'i/o', "'cause", 'brexit', 'debuffs'
#' ))$selected[, c('terms', 'coverage')]
#' }
#' @export

select.lspace = function(query = NULL, dir = getOption('lingmatch.lspace.dir'),
  get.map = FALSE, check.md5 = TRUE, mode = 'wb'){
  dir = sub('/*$', '/', path.expand(dir))
  map_path = paste0(dir, 'lma_term_map.rda')
  if(missing(get.map) && !missing(query) && length(query) > 1) get.map = TRUE
  if(!exists('lma_term_map')) lma_term_map = NULL
  if(get.map && !(file.exists(map_path) || !is.null(lma_term_map))){
    if(!file.exists(map_path)){
      status = tryCatch(download.file('https://osf.io/download/xr7jv',
        map_path, mode = mode), error = function(e) 1)
      if(!status && check.md5){
        fi = strsplit(readLines('https://api.osf.io/v2/files/xr7jv', 1, TRUE, FALSE, 'utf-8'), '[:,{}"]+')[[1]]
        ck = md5sum(map_path)
        if(fi[which(fi == 'md5') + 1] == ck){
          load(map_path)
          save(lma_term_map, file = map_path, compress = FALSE)
        }else warning(paste0(
          "The term map's MD5 (", ck, ') does not seem to match the one on record;\n',
          'double check and try manually downloading at https://osf.io/xr7jv/?show=revision'
        ))
      }
    }
  }
  r = list(info = lss_info, selected = lss_info[NULL,])
  r$info[, 'wiki'] = paste0('https://osf.io/489he/wiki/', rownames(lss_info))
  r$info[, 'downloaded'] = paste0(dir, rownames(r$info), '.dat')
  r$info[!r$info[, 'downloaded'] %in% list.files(dir, '\\.dat'), 'downloaded'] = ''
  if(get.map || missing(query)) if(!is.null(lma_term_map)){
    r$term_map = lma_term_map
  }else if(file.exists(map_path) && is.null(lma_term_map)){
    load(map_path)
    r$term_map = lma_term_map
    rm(list = 'lma_term_map')
  }
  if(!missing(query)){
    if(!is.character(query) && !is.null(colnames(query))) query = colnames(query)
    if(length(query) > 1 && 'term_map' %in% names(r)){
      query = tolower(query)
      overlap = query[query %in% rownames(r$term_map)]
      if(length(overlap)){
        r$info$coverage = colSums(r$term_map[overlap,, drop = FALSE] != 0) / length(query)
        r$selected = r$info[order(r$info$coverage, decreasing = TRUE)[1:5],]
        r$space_terms = overlap
      }else warning('query was treated as terms, but none were found')
    }else if(length(query) < nrow(lss_info) * 2){
      query = paste0(query, collapse = '|')
      if(!length(sel <- grep(query, rownames(lss_info), TRUE))){
        collapsed = vapply(seq_len(nrow(lss_info)),
          function(r) paste(c(rownames(lss_info)[r], lss_info[r,]), collapse = ' '), '')
        if(!length(sel <- grep(query, collapsed, TRUE)))
          sel <- grep(paste(strsplit(query, '[[:space:],|]+')[[1]], collapse = '|'), collapsed, TRUE)
      }
      if(length(sel)) r$selected = r$info[sel,]
    }
  }
  r
}

#' Download Latent Semantic Spaces
#'
#' Downloads the specified semantic space from \href{https://osf.io/489he}{osf.io/489he}.
#'
#' @param space Name of one or more spaces you want to download, or \code{'all'} for all available. \code{'100k'} is
#'  the default, and some other common options might be \code{'google'}, \code{'facebook'}, or \code{'glove'}.
#'  See \href{https://osf.io/489he/wiki/home}{osf.io/489he/wiki} for more information, and a full list of spaces.
#' @param include.terms Logical; if \code{FALSE}, only the \code{.dat.bz2} file is downloaded
#'  (which only has numeric vectors).
#' @param decompress Logical; if \code{TRUE} (default), decompresses the downloaded file
#'  with the \code{bunzip2} system command assuming it is available (as indicated by
#'  \code{Sys.which('bunzip2')}).
#' @param check.md5 Logical; if \code{TRUE} (default), retrieves the MD5 checksum from OSF,
#'  and compares it with that calculated from the downloaded file to check its integrity.
#' @param mode A character specifying the file write mode; default is 'wb'. See
#'  \code{\link{download.file}}.
#' @param dir Directory in which to save the space; default is
#'  \code{getOption('lingmatch.lspace.dir')}.
#' @family Latent Semantic Space functions
#' @return A character vector with paths to the [1] data and [2] term files.
#' @examples
#' \dontrun{
#'
#' download.lspace('glove_crawl')
#' }
#' @export
#' @importFrom utils download.file
#' @importFrom tools md5sum

download.lspace = function(space = '100k', include.terms = TRUE, decompress = TRUE,
  check.md5 = TRUE, mode = 'wb', dir = getOption('lingmatch.lspace.dir')){
  if(length(space) == 1 && space == 'all') space = rownames(select.lspace()$info)
  if(length(space) > 1){
    res = lapply(space, function(d){
      m = tryCatch(download.lspace(d, include.terms = include.terms, decompress = decompress,
        check.md5 = check.md5, mode = mode, dir = dir), error = function(e) e$message)
      if(is.null(m)) 'downloaded' else paste('failed: ', m)
    })
    names(res) = space
    return(res)
  }
  dir = sub('/+$', '', path.expand(dir))
  if(space == 'default') space = '100k'
  name = grep(paste0('^', sub('\\..*$', '', space)), rownames(lss_info), value = TRUE)
  if(!length(name)) name = grep(paste0('^', substr(space, 1, 4)), rownames(lss_info), TRUE, value = TRUE)
  if(!length(name)){
    stop('space ', space, ' not recognized; see https://osf.io/489he/wiki for available spaces')
  }else name = name[1]
  urls = list(
    info = function(id) paste0('https://api.osf.io/v2/files/', id),
    dl = function(id) paste0('https://osf.io/download/', id),
    versions = function(id) paste0('https://osf.io/', id, '/?show=revision')
  )
  dir = path.expand(dir)
  if(!dir.exists(dir)) dir.create(dir)
  dl = function(id, ext, ck){
    s = urls$dl(id)
    o = paste0(dir, '/', name, ext)
    status = tryCatch(download.file(s, o, mode = mode), error = function(e) 1)
    if(!status && check.md5){
      fi = strsplit(readLines(urls$info(id), 1, TRUE, FALSE, 'utf-8'), '[:,{}"]+')[[1]]
      ck = md5sum(o)
      if(fi[which(fi == 'md5') + 1] != ck) warning(paste0(
        'MD5 (', ck, ') does not seem to match the one on record;\n',
        'double check and try manually downloading at ', urls$versions(id)
      ))
    }
    if(status) warning('failed to download file from ', s, call. = FALSE)
    status
  }
  status = if(include.terms) dl(lss_info[name, 'osf_terms'], '_terms.txt', check.md5) else 0
  if(!status) status = dl(lss_info[name, 'osf_dat'], '.dat.bz2', check.md5)
  if(!status && decompress){
    if(Sys.which('bunzip2') == ''){
      warning('could not find path to bunzip2 command for decompression')
    }else{
      o = paste0(dir, '/', name, '.dat.bz2')
      status = tryCatch(system2('bunzip2', shQuote(path.expand(o))), error = function(e) 1)
      if(status) warning(
        'failed to decompress; might try this from a system console:\n  bunzip2 "', path.expand(o), '"'
      )
    }
  }
  dir = paste0(dir, '/', name, c(if(!status && decompress) '.dat' else
    '.dat.bz2', '_terms.txt'), collapse = '\n  ')
  message('downloaded ', name, ' space:\n  ', dir)
  invisible(dir)
}

#' Select Dictionaries
#'
#' Retrieve information and links to dictionaries
#' (lexicons/word lists) available at \href{https://osf.io/y6g5b}{osf.io/y6g5b}.
#'
#' @param query A character matching a dictionary name, or a set of keywords to search for in
#'   dictionary information.
#' @param dir Path to a folder containing dictionaries, or where you want them to be saved.
#' @param check.md5 Logical; if \code{TRUE} (default), retrieves the MD5 checksum from OSF,
#'   and compares it with that calculated from the downloaded file to check its integrity.
#' @param mode Passed to \code{\link{download.file}} when downloading files.
#' @return A list with varying entries:
#'   \tabular{ll}{
#'     info \tab The version of \href{https://osf.io/kjqb8}{osf.io/kjqb8} stored internally; a
#'       \code{data.frame}  with dictionary names as row names, and information about each dictionary in columns.
#'         Also described at
#'         \href{https://osf.io/y6g5b/wiki/dict_variables}{osf.io/y6g5b/wiki/dict_variables},
#'         here \code{short} (corresponding to the file name [\code{{short}.(csv|dic)}] and
#'         wiki urls [\code{https://osf.io/y6g5b/wiki/{short}}]) is set as row names and removed:
#'         \tabular{ll}{
#'           \code{name} \tab Full name of the dictionary. \cr
#'           \code{description} \tab Description of the dictionary, relating to its purpose and
#'             development. \cr
#'           \code{note} \tab Notes about processing decisions that additionally alter the original. \cr
#'           \code{constructor} \tab How the dictionary was constructed:
#'             \tabular{ll}{
#'               \code{algorithm} \tab Terms were selected by some automated process, potentially
#'                 learned from data or other resources. \cr
#'               \code{crowd} \tab Several individuals rated the terms, and in aggregate those ratings
#'                 translate to categories and weights. \cr
#'               \code{mixed} \tab Some combination of the other methods, usually in some iterative
#'                 process. \cr
#'               \code{team} \tab One of more individuals make decisions about term inclusions,
#'                 categories, and weights. \cr
#'             } \cr
#'           \code{subject} \tab Broad, rough subject or purpose of the dictionary:
#'             \tabular{ll}{
#'               \code{emotion} \tab Terms relate to emotions, potentially exemplifying or expressing
#'                 them. \cr
#'               \code{general} \tab A large range of categories, aiming to capture the content of the
#'                 text. \cr
#'               \code{impression} \tab Terms are categorized and weighted based on the impression they
#'                 might give. \cr
#'               \code{language} \tab Terms are categorized or weighted based on their linguistic
#'                 features, such as part of speech, specificity, or area of use. \cr
#'               \code{social} \tab Terms relate to social phenomena, such as characteristics or concerns
#'                 of social entities. \cr
#'             } \cr
#'           \code{terms} \tab Number of unique terms across categories. \cr
#'           \code{term_type} \tab Format of the terms:
#'             \tabular{ll}{
#'               \code{glob} \tab Include asterisks which denote inclusion of any characters until a
#'                 word boundary. \cr
#'               \code{glob+} \tab Glob-style asterisks with regular expressions within terms. \cr
#'               \code{ngram} \tab Includes any number of words as a term, separated by spaces. \cr
#'               \code{pattern} \tab A string of characters, potentially within or between words, or
#'                 spanning words. \cr
#'               \code{regex} \tab Regular expressions. \cr
#'               \code{stem} \tab Unigrams with common endings removed. \cr
#'               \code{unigram} \tab Complete single words. \cr
#'             } \cr
#'           \code{weighted} \tab Indicates whether weights are associated with terms. This
#'             determines the file type of the dictionary: dictionaries with weights are stored
#'             as .csv, and those without are stored as .dic files. \cr
#'           \code{regex_characters} \tab Logical indicating whether special regular expression
#'             characters are present in any term, which might need to be escaped if the terms are used
#'             in regular expressions. Glob-type terms allow complete parens (at least one open and one
#'             closed, indicating preceding or following words), and initial and terminal asterisks. For
#'             all other terms, \code{[](){}*.^$+?\|} are counted as regex characters. These could be
#'             escaped in R with \code{gsub('([][)(}{*.^$+?\\\\|])', '\\\\\\1', terms)} if \code{terms}
#'             is a character vector, and in Python with (importing re)
#'             \code{[re.sub(r'([][(){}*.^$+?\|])', r'\\\1', term) for term in terms]} if \code{terms}
#'             is a list. \cr
#'           \code{categories} \tab Category names in the order in which they appear in the dictionary
#'             file, separated by commas. \cr
#'           \code{ncategories} \tab Number of categories.\cr
#'           \code{original_max} \tab Maximum value of the original dictionary before standardization:
#'             \code{original values / max(original values) * 100}. Dictionaries with no weights are
#'             considered to have a max of \code{1}. \cr
#'           \code{osf} \tab ID of the file on OSF, translating to the file's URL:
#'             https://osf.io/\code{osf} \cr
#'           \code{wiki} \tab URL of the dictionary's wiki. \cr
#'           \code{downloaded} \tab Path to the file if downloaded, and \code{''} otherwise. \cr
#'         }
#'       \cr
#'     selected \tab A subset of \code{info} selected by \code{query}. \cr
#'   }
#' @family Dictionary functions
#' @examples
#' # just retrieve information about available dictionaries
#' dicts = select.dict()$info
#'
#' # select all dictionaries mentioning sentiment or emotion
#' sentiment_dicts = select.dict('sentiment emotion')$selected
#' @export

select.dict = function(query = NULL, dir = getOption('lingmatch.dict.dir'),
  check.md5 = TRUE, mode = 'wb'){
  dir = sub('/+$', '', path.expand(dir))
  r = list(info = dict_info, selected = dict_info[NULL,])
  r$info[, 'wiki'] = paste0('https://osf.io/y6g5b/wiki/', rownames(dict_info))
  r$info[, 'downloaded'] = paste0(sub('/+$', '', dir), '/', rownames(r$info),
    ifelse(r$info$weighted, '.csv', '.dic'))
  r$info[!file.exists(r$info[, 'downloaded']), 'downloaded'] = ''
  if(!missing(query) && length(query) < nrow(dict_info) * 2){
    query = paste0(query, collapse = '|')
    if(!length(sel <- grep(query, rownames(dict_info), TRUE))){
      collapsed = vapply(seq_len(nrow(dict_info)),
        function(r) paste(c(rownames(dict_info)[r], dict_info[r,]), collapse = ' '), '')
      if(!length(sel <- grep(query, collapsed, TRUE)))
        sel <- grep(paste(strsplit(query, '[[:space:],|]+')[[1]], collapse = '|'), collapsed, TRUE)
    }
    if(length(sel)) r$selected = r$info[sel,]
  }
  r
}

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
#' @param dir Directory in which to save the dictionary; default is
#'  \code{getOption('lingmatch.dict.dir')}.
#' @return Path to the downloaded dictionary, or a list of such if multiple were downloaded.
#' @family Dictionary functions
#' @examples
#' \dontrun{
#'
#' download.dict('lusi')
#' }
#' @export

download.dict = function(dict = 'lusi', check.md5 = TRUE, mode = 'wb', dir = getOption('lingmatch.dict.dir')){
  if(length(dict) == 1 && dict == 'all') dict = rownames(select.dict()$info)
  if(length(dict) > 1){
    res = lapply(dict, function(d) tryCatch(
      download.dict(d, check.md5 = check.md5, mode = mode, dir = dir),
      error = function(e) paste('failed:', e$message)
    ))
    names(res) = dict
    return(res)
  }
  dir = sub('/+$', '', path.expand(dir))
  name = grep(paste0('^', sub('\\.[^.]*$', '', dict)), rownames(dict_info), value = TRUE)
  if(!length(name)) name = grep(paste0('^', substr(dict, 1, 6)), rownames(dict_info), TRUE, value = TRUE)
  if(!length(name)){
    stop('dictionary ', dict, ' not recognized; see https://osf.io/y6g5b/wiki for available dictionaries')
  }else name = name[1]
  urls = list(
    info = function(id) paste0('https://api.osf.io/v2/files/', id),
    dl = function(id) paste0('https://osf.io/download/', id),
    versions = function(id) paste0('https://osf.io/', id, '/?show=revision')
  )
  dir = path.expand(dir)
  if(!dir.exists(dir)) dir.create(dir)
  dl = function(id, ext, ck){
    s = urls$dl(id)
    o = paste0(dir, '/', name, ext)
    status = tryCatch(download.file(s, o, mode = mode), error = function(e) 1)
    if(!status && check.md5){
      fi = strsplit(readLines(urls$info(id), 1, TRUE, FALSE, 'utf-8'), '[:,{}"]+')[[1]]
      ck = md5sum(o)
      if(fi[which(fi == 'md5') + 1] != ck) warning(paste0(
        'MD5 (', ck, ') does not seem to match the one on record;\n',
        'double check and try manually downloading at ', urls$versions(id)
      ))
    }
    if(status) warning('failed to download file from ', s, call. = FALSE)
    status
  }
  type = if(dict_info[name, 'weighted']) '.csv' else '.dic'
  status = dl(dict_info[name, 'osf'], type, check.md5)
  dir = paste0(dir, '/', name, type, collapse = '\n  ')
  message('downloaded ', name, ' dictionary:\n  ', dir)
  invisible(dir)
}

#' Standardize a Latent Semantic Space
#'
#' Reformat a .rda file which has a matrix with terms as row names, or a plain-text embeddings file
#' which has a term at the start of each line, and consistent delimiting characters. Plain-text files
#' are processed line-by-line, so large spaces can be reformatted RAM-conservatively.
#'
#' @param infile Name of the .rda or plain-text file relative to \code{dir},
#'   e.g., "default.rda" or "glove/glove.6B.300d.txt".
#' @param name Base name of the reformatted file and term file; e.g., "glove" would result in
#'   \code{glove.dat} and \code{glove_terms.txt} in \code{outdir}.
#' @param sep Delimiting character between values in each line, e.g., \code{" "} or \code{"\\t"}.
#'   Only applies to plain-text files.
#' @param digits Number of digits to round values to; default is 9.
#' @param dir Path to folder containing \code{infile}s; default is \code{getOption('lingmatch.lspace.dir')}.
#' @param outdir Path to folder in which to save standardized files; default is \code{dir}.
#' @param remove A string with a regex pattern to be removed from term names (i.e., \code{gsub(}\code{remove,}
#'   \code{"", term)}); default is \code{""}, which is ignored.
#' @param term_check A string with a regex pattern by which to filter terms; i.e., only lines with fully
#'   matched terms are written to the reformatted file. The default attempts to retain only regular words, including
#'   those with dashes, foreword slashes, and periods. Set to an empty string (\code{""}) to write all lines
#'   regardless of term.
#' @param verbose Logical: if \code{TRUE}, prints the current line number and its term to the console every 1,000 lines.
#'   Only applies to plain-text files.
#' @family Latent Semantic Space functions
#' @examples
#' \dontrun{
#'
#' # from https://sites.google.com/site/fritzgntr/software-resources/semantic_spaces
#' standardize.lspace('EN_100k_lsa.rda', '100k_lsa')
#'
#' # from https://fasttext.cc/docs/en/english-vectors.html
#' standardize.lspace('crawl-300d-2M.vec', 'facebook_crawl')
#'
#' # Standardized versions of these spaces can also be downloaded with download.lspace.
#'
#' }
#' @export

standardize.lspace = function(infile, name, sep = ' ', digits = 9, dir = getOption('lingmatch.lspace.dir'),
  outdir = dir, remove = '', term_check = "^[a-zA-Z]+$|^['a-zA-Z][a-zA-Z.'\\/-]*[a-zA-Z.]$", verbose = FALSE){
  if(!is.character(term_check)) term_check = ''
  ip = paste0(sub('/+$', '', path.expand(dir)), '/', infile)
  op = paste0(sub('/+$', '', path.expand(outdir)), '/', name)
  if(!is.character(infile) || grepl('\\.rda$', infile)){
    if(is.character(infile)){
      f = load(ip)
      o = get(f)
    }else o = infile
    o = round(o, digits)
    ot = rownames(o)
    if(remove != '') ot = gsub(remove, '', ot)
    if(term_check != ''){
      su = grepl(term_check, ot)
      o = o[su,]
      ot = ot[su]
    }
    writeLines(ot, paste0(op, '_terms.txt'))
    write(formatC(t(o), digits, 0, 'f'), paste0(op, '.dat'), ncol(o))
    if(is.character(infile)) rm(f, 'o')
  }else{
    if(!file.exists(ip)) stop('infile does not exist: ', ip)
    if(!grepl(term_check, scan(ip, '', 1, sep = sep, quiet = TRUE)))
      stop('infile does not appear to start with a term: ', ip)
    reformat_embedding(ip, op, sep, digits, remove, term_check, verbose)
  }
  message('created ', op, '.dat\nfrom ', ip)
}



#' Categorize Texts
#'
#' Categorize raw texts using a pattern-based dictionary.
#'
#' @param text A vector of text to be categorized. Texts are padded by 2 spaces, and potentially lowercased.
#' @param dict At least a vector of terms (patterns), usually a matrix-like object with columns for terms,
#'   categories, and weights.
#' @param pattern.weights A vector of weights corresponding to terms in \code{dict}, or the column name of
#'   weights found in \code{dict}.
#' @param pattern.categories A vector of category names corresponding to terms in \code{dict}, or the column name of
#'   category names found in \code{dict}.
#' @param bias A constant to add to each category after weighting and summing. Can be a vector with names
#'   corresponding to the unique values in \code{dict[, category]}, but is usually extracted from dict based
#'   on the intercept included in each category (defined by \code{name.map['intname']}).
#' @param to.lower Logical indicating whether \code{text} should be converted to lowercase before processing.
#' @param return.dtm Logical; if \code{TRUE}, only a document-term matrix will be returned, rather than the
#'   summed and biased category values.
#' @param exclusive Logical; if \code{FALSE}, each dictionary term is searched for in the original text.
#'   Otherwise (by default), terms are sorted by length (with longer terms being searched for first), and
#'   matches are removed from the text (avoiding subsequent matches to matched patterns).
#' @param boundary A string to add to the beginning and end of each dictionary term. If \code{TRUE},
#'   \code{boundary} will be set to \code{' '}, avoiding pattern matches within words. By default, dictionary
#'   terms are left as entered.
#' @param fixed Logical; if \code{FALSE}, patterns are treated as regular expressions.
#' @param globtoregex Logical; if \code{TRUE}, initial and terminal asterisks are replaced with \code{\\\\b\\\\w`*`}
#'   and \code{\\\\w`*`\\\\b} respectively. This will also set \code{fixed} to \code{FALSE} unless fixed is specified.
#' @param name.map A named character vector:
#'   \tabular{ll}{
#'     \code{intname} \tab term identifying category biases within the term list; defaults to \code{'_intercept'} \cr
#'     \code{term} \tab name of the column containing terms in \code{dict}; defaults to \code{'term'} \cr
#'   }
#'   Missing names are added, so names can be specified positional (e.g., \code{c('_int', 'terms')}),
#'   or only some can be specified by name (e.g., \code{c(term = 'patterns')}), leaving the rest default.
#' @param dir Path to a folder in which to look for \code{dict} if it is the name of a file to be passed to
#'   \code{\link{read.dic}}.
#' @seealso For applying term-based dictionaries (to a document-term matrix) see \code{\link{lma_termcat}}.
#' @family Dictionary functions
#' @examples
#' # example text
#' text = c(
#'   paste(
#'     "Oh, what youth was! What I had and gave away.",
#'     "What I took and spent and saw. What I lost. And now? Ruin."
#'   ),
#'   paste(
#'     "God, are you so bored?! You just want what's gone from us all?",
#'     "I miss the you that was too. I love that you."
#'   ),
#'   paste(
#'     "Tomorrow! Tomorrow--nay, even tonight--you wait, as I am about to change.",
#'     "Soon I will off to revert. Please wait."
#'   )
#' )
#'
#' # make a document-term matrix with pre-specified terms only
#' lma_patcat(text, c('bored?!', 'i lo', '. '), return.dtm = TRUE)
#'
#' # get counts of sets of letter
#' lma_patcat(text, list(c('a', 'b', 'c'), c('d', 'e', 'f')))
#'
#' # same thing with regular expressions
#' lma_patcat(text, list('[abc]', '[def]'), fixed = FALSE)
#'
#' # match only words
#' lma_patcat(text, list('i'), boundary = TRUE)
#'
#' # match only words, ignoring punctuation
#' lma_patcat(
#'   text, c('you', 'tomorrow', 'was'), fixed = FALSE,
#'   boundary = '\\b', return.dtm = TRUE
#' )
#'
#' \dontrun{
#'
#' # read in the temporal orientation lexicon from the World Well-Being Project
#' tempori = read.csv('https://wwbp.org/downloads/public_data/temporalOrientationLexicon.csv')
#'
#' lma_patcat(text, tempori)
#' }
#' @export

lma_patcat = function(text, dict = NULL, pattern.weights = 'weight', pattern.categories = 'category', bias = NULL,
  to.lower = TRUE, return.dtm = FALSE, exclusive = TRUE, boundary = NULL, fixed = TRUE, globtoregex = FALSE,
  name.map = c(intname = '_intercept', term = 'term'), dir = getOption('lingmatch.dict.dir')){
  if(is.factor(text)) text = as.character(text)
  if(!is.character(text)) stop('enter a character vector as the first argument')
  text = paste(' ', text, ' ')
  if(is.null(names(name.map)) && length(name.map) < 3) names(name.map) = c('intname', 'term')[seq_along(name.map)]
  wide = FALSE
  if(missing(dict) && missing(pattern.weights) && missing(pattern.categories)) dict = lma_dict()
  if(is.character(dict) && length(dict) == 1 && missing(pattern.weights) && missing(pattern.categories)){
    if(!any(file.exists(dict)) && any(file.exists(paste0(dir, dict)))) dict = paste0(dir, dict)
    td = tryCatch(read.dic(dict), error = function(e) NULL)
    dict = if(is.null(td)) list(cat1 = dict) else td
  }
  if(!is.null(dim(dict))){
    if(is.null(colnames(dict))){
      colnames(dict) = paste0('X', seq_len(ncol(dict)))
    }else{
      if(!is.data.frame(dict)) dict = as.data.frame(as.matrix(dict))
      terms = if(name.map[['term']] %in% colnames(dict)) colnames(dict) != name.map[['term']] else !logical(ncol(dict))
      if(missing(pattern.weights) && !any(pattern.weights %in% colnames(dict))){
        if(any(su <- terms & vapply(dict, is.numeric, TRUE))){
          terms = terms & !su
          pattern.weights = dict[, su]
        }
      }
      if(missing(pattern.categories) && !pattern.categories %in% colnames(dict)){
        if(any(su <- terms & vapply(dict, function(v) !is.numeric(v) && anyDuplicated(v), TRUE))){
          terms = terms & !su
          pattern.categories = dict[, su]
          if(sum(su) > 1) pattern.categories = do.call(paste, pattern.categories)
        }
      }
      if(name.map[['term']] %in% colnames(dict)) dict[, name.map[['term']]] else if(!all(terms))
        dict = if(any(terms)) dict[, which(terms)[1]] else rownames(dict)
    }
  }
  # independently entered wide weights
  if((is.null(dict) || is.null(dim(dict))) && (!is.null(ncol(pattern.weights)) || !is.null(ncol(pattern.categories)))){
    weights = if(!is.null(ncol(pattern.weights))) pattern.weights else pattern.categories
    if(!is.null(rownames(weights)) && any(grepl('[^0-9]', rownames(weights)))){
      dict = rownames(weights)
    }else if(is.list(dict) && (length(dict) == 1 ||
      (length(dict[[1]]) == nrow(weights) && all(vapply(dict, length, 0) == nrow(weights))))){
      dict = dict[[1]]
    }
    if(length(dict) != nrow(weights)) stop('dict and wide weights do not align')
    wide = TRUE
    if(!missing(pattern.categories) && is.character(pattern.categories) && any(su <- pattern.categories %in% weights))
      weights = weights[, pattern.categories[su], drop = FALSE]
    weights = weights[, vapply(seq_len(ncol(weights)), function(col) is.numeric(weights[, col]), TRUE), drop = FALSE]
    if(!ncol(weights)) stop('could not identify numeric weights in wide weights')
    lex = list(terms = dict, weights = weights, category = colnames(weights))
  # wide weights in dict
  }else if(!is.null(dim(dict)) && (
      (length(pattern.weights) > 1 && is.character(pattern.weights)) ||
      (length(pattern.categories) > 1 &&
          (length(pattern.categories) != nrow(dict) || all(pattern.categories %in% colnames(dict)))) ||
      (!any(pattern.weights %in% colnames(dict)) && !any(pattern.categories %in% colnames(dict)))
    )){
    if(any(su <- pattern.weights %in% colnames(dict))){
      categories = pattern.weights[su]
    }else if(any(su <- pattern.categories %in% colnames(dict))){
      categories = pattern.categories
    }else if(any(su <- vapply(colnames(dict), function(v) is.numeric(dict[, v]), TRUE))){
      categories = colnames(dict)[su]
    }else stop('could not find weights in dict column names')
    wide = TRUE
    if(!name.map[['term']] %in% colnames(dict)){
      terms = colnames(dict)[vapply(colnames(dict), function(v) !is.numeric(dict[, v]), TRUE)]
      if(!length(terms)) stop('could not find terms in dict')
      name.map[['term']] = if(length(terms) > 1){
        su = vapply(terms, function(v) !anyDuplicated(dict[, v]), TRUE)
        if(any(su)) terms[which(su)[1]] else terms[1]
      }else terms
    }
    lex = list(term = dict[, name.map[['term']]], weights = dict[, categories, drop = FALSE], category = categories)
  # independently entered weights and categories
  }else if(is.null(dim(dict))){
    if((is.numeric(dict) && is.null(names(dict))) || (is.list(dict) && is.numeric(dict[[1]]) &&
        is.null(names(dict[[1]])))) stop('could not recognize terms in dict')
    n = length(dict)
    lex = data.frame(
      term = if(is.character(dict)) dict else if(is.numeric(dict)) names(dict) else if(is.list(dict) &&
        is.numeric(dict[[1]])) unlist(lapply(dict, names), use.names = FALSE) else unlist(dict, use.names = FALSE),
      category = if(length(pattern.categories) == n) if(is.list(dict) && !is.null(names(dict)))
        names(dict) else pattern.categories else if(is.list(dict)) rep(if(!is.null(names(dict))) names(dict) else
          paste0('cat', seq_along(dict)), vapply(dict, length, 0)) else 'cat1',
      weights = if(is.numeric(dict)) unname(dict) else if(is.numeric(pattern.weights))
        if(!is.null(names(pattern.weights)) && is.character(dict) && all(dict %in% names(pattern.weights)))
          pattern.weights[dict] else pattern.weights else if(is.list(dict)) if(is.numeric(dict[[1]]))
        unlist(dict, use.names = FALSE) else if(is.list(pattern.weights) && is.numeric(pattern.weights[[1]]))
          unlist(pattern.weights, use.names = FALSE) else 1 else 1
    )
  }else{
    term = if('term' %in% names(name.map)) name.map[['term']] else 'term'
    en = colnames(dict)
    if(!term %in% en){
      su = vapply(en, function(v) !is.numeric(dict[, v]), TRUE)
      if(any(su)){
        term = en[which(su)[1]]
        if(sum(su) > 1){
          su = su & vapply(en, function(v) !anyDuplicated(dict[, v]), TRUE)
          if(any(su)) term = en[which(su)[1]]
        }
      }else stop('could not recognize terms in dict')
    }
    lex = data.frame(
      term = dict[[term]],
      category = if(length(pattern.categories) == nrow(dict)) pattern.categories else
        if(pattern.categories %in% en) dict[[pattern.categories]] else 'cat1',
      weights = if(length(pattern.weights) == nrow(dict)) pattern.weights else
        if(all(pattern.weights %in% en)) dict[[pattern.weights]] else 1
    )
  }
  if(globtoregex){
    lex$term = to_regex(list(lex$term), TRUE)[[1]]
    if(missing(fixed)) fixed = FALSE
  }
  if(wide && return.dtm){
    return.dtm = FALSE
    warning('cannot return dtm when multiple weights are specified -- remove weights for a dtm')
  }
  if(is.null(bias)){
    if(!'intname' %in% names(name.map)) name.map[['intname']] = '_intercept'
    if(any(su <- lex$term == name.map[['intname']])){
      if(wide){
        bias = structure(lex$weights[su,], names = lex$categories[su])
        lex$term = lex$term[!su]
        lex$weights = lex$weights[!su,]
      }else{
        bias = structure(lex[su, 'weights'], names = lex[su, 'category'])
        lex = lex[!su,]
      }
    }
  }
  if(exclusive){
    cls = tryCatch(-nchar(lex$term), error = function(e) NULL)
    if(is.null(cls)){
      warning('dict appears to be miss-encoded, so results may not be as expected;\n',
        'might try reading the dictionary in with encoding = "latin1"')
      lex$term = iconv(lex$term, sub = '#')
      cls = -nchar(lex$term)
    }
    if(wide){
      o = order(cls)
      lex$term = lex$term[o]
      lex$weights = lex$weights[o,]
    }else lex = lex[order(cls),]
  }
  lex$category = factor(lex$category, unique(lex$category))
  categories = levels(lex$category)
  if(length(bias)){
    if(is.null(names(bias)) && length(bias) == length(categories)) names(bias) = categories
    if(any(su <- !categories %in% names(bias))) bias[categories[su]] = 0
  }else bias = structure(integer(length(categories)), names = categories)
  bias = bias[categories]
  if(is.logical(boundary) && boundary) boundary = ' '
  if(missing(to.lower)){
    if(any(grepl('[A-Z]', lex$term))){
      to.lower = FALSE
      if(!any(grepl('[a-z]', lex$term))) text = toupper(text)
    }
  }
  if(to.lower) text = tolower(text)
  st = proc.time()[[3]]
  terms = unique(lex$term)
  op = pattern_search(
    text, if(is.character(boundary)) paste0(boundary, terms, boundary) else terms,
    seq_along(terms) - 1L, fixed, exclusive
  )
  colnames(op[[1]]) = terms
  if(return.dtm){
    attr(op[[1]], 'categories') = lapply(categories, function(cat)
      which(colnames(op[[1]]) %in% lex[lex$category == cat, 'term']))
    names(attr(op[[1]], 'categories')) = categories
  }else{
    op[[1]] = vapply(categories, function(cat){
      l = if(wide) data.frame(term = lex$term, weights = if(cat %in% colnames(lex$weights)) lex$weights[, cat] else
        lex$weights) else lex[lex$category == cat,]
      as.numeric(op[[1]][, l$term, drop = FALSE] %*% l$weights + bias[[cat]])
    }, numeric(length(text)))
  }
  attr(op[[1]], 'WC') = op[[2]]
  attr(op[[1]], 'time') = c(patcat = proc.time()[[3]] - st)
  op[[1]]
}

#' Calculate Text-Based Metastatistics
#'
#' Calculate simple descriptive statistics from text.
#'
#' @param text A character vector of texts.
#' @return A data.frame: \tabular{ll}{
#'   \code{characters} \tab Total number of characters.\cr
#'   \code{syllables} \tab Total number of syllables, as estimated by split length of
#'     \code{'a+[eu]*|e+a*|i+|o+[ui]*|u+|y+[aeiou]*'} - 1.\cr
#'   \code{words} \tab Total number of words (raw word count).\cr
#'   \code{unique_words} \tab Number of unique words (binary word count).\cr
#'   \code{clauses} \tab Number of clauses, as marked by commas, colons, semicolons, dashes, or brackets
#'     within sentences.\cr
#'   \code{sentences} \tab Number of sentences, as marked by periods, question marks, exclamation points,
#'     or new line characters.\cr
#'   \code{words_per_clause} \tab Average number of words per clause.\cr
#'   \code{words_per_sentence} \tab Average number of words per sentence.\cr
#'   \code{sixltr} \tab Number of words 6 or more characters long.\cr
#'   \code{characters_per_word} \tab Average number of characters per word
#'     (\code{characters} / \code{words}).\cr
#'   \code{syllables_per_word} \tab Average number of syllables per word
#'     (\code{syllables} / \code{words}).\cr
#'   \code{type_token_ratio} \tab Ratio of unique to total words: \code{unique_words} / \code{words}.\cr
#'   \code{reading_grade} \tab Flesch-Kincaid grade level: .39 * \code{words} / \code{sentences} +
#'     11.8 * \code{syllables} / \code{words} - 15.59.\cr
#'   \code{numbers} \tab Number of terms starting with numbers. \cr
#'   \code{punct} \tab Number of terms starting with non-alphanumeric characters.\cr
#'   \code{periods} \tab Number of periods.\cr
#'   \code{commas} \tab Number of commas.\cr
#'   \code{qmarks} \tab Number of question marks.\cr
#'   \code{exclams} \tab Number of exclamation points.\cr
#'   \code{quotes} \tab Number of quotation marks (single and double).\cr
#'   \code{apostrophes} \tab Number of apostrophes, defined as any modified letter apostrophe, or backtick
#'     or single straight or curly quote surrounded by letters.\cr
#'   \code{brackets} \tab Number of bracketing characters (including parentheses, and square,
#'     curly, and angle brackets).\cr
#'   \code{orgmarks} \tab Number of characters used for organization or structuring (including
#'     dashes, foreword slashes, colons, and semicolons).
#' }
#'
#' @export

lma_meta = function(text){
  text = gsub('^\\s+|\\s+$', '', text)
  dtm = lma_dtm(text, numbers = TRUE, punct = TRUE, urls = FALSE)
  text = gsub(paste0(
    '((?:^|\\s)[a-z]+\\.[a-z.]+|\\d|(?:^|\\s)[a-z]|(?:^|\\s)[iv]+|',
    'ans|govt|apt|etc|st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof)\\.'
  ), '', text, TRUE)
  terms = colnames(dtm)
  dwm = dtm[, grepl("^[a-z']", terms), drop = FALSE]
  words = colnames(dwm)
  word_lengths = nchar(words)
  word_syllables = vapply(strsplit(words, 'a+[eu]*|e+a*|i+|o+[ui]*|u+|y+[aeiou]*'), length, 0) - 1
  word_syllables[word_syllables == 0] = 1
  res = data.frame(
    characters = as.numeric(dwm %*% word_lengths),
    syllables = as.numeric(dwm %*% word_syllables),
    words = rowSums(dwm),
    unique_words = rowSums(dwm != 0),
    clauses = vapply(strsplit(text, '([.?!\n,:;)}>-]|\\])([.?!\n,:;)}>\n\'"-]|\\s|\\])*'), length, 0),
    sentences = vapply(strsplit(text, '[.?!\n]([.?!\n\'"]|\\s)*'), length, 0)
  )
  cbind(res, with(res, data.frame(
    words_per_clause = words / clauses,
    words_per_sentence = words / sentences,
    sixltr = as.numeric(dwm %*% (word_lengths > 5)),
    characters_per_word = characters / words,
    syllables_per_word = syllables / words,
    type_token_ratio = unique_words / words,
    reading_grade = .39 * words / sentences + 11.8 * syllables / words - 15.59,
    numbers = if(any(su <- grepl('^[0-9]', terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    puncts = if(any(su <- grepl('^[^a-z0-9]', terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    periods = if('.' %in% terms) dtm[, '.'] else 0,
    commas = if(',' %in% terms) dtm[, ','] else 0,
    qmarks = if('?' %in% terms) dtm[, '?'] else 0,
    exclams = if('!' %in% terms) dtm[, '!'] else 0,
    quotes = if(any(su <- grepl('^[\'"]', terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    apostrophes = vapply(strsplit(text, "[\u02bc]+|[a-zA-Z][\u0027\u0060\u2019]+[a-zA-Z]"), length, 0) - 1,
    brackets = if(any(su <- grepl('[(\\)<>{\\}[]|\\]', terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    orgmarks = if(any(su <- grepl('[/:;-]', terms))) rowSums(dtm[, su, drop = FALSE]) else 0
  )))
}

#' English Function Word Category and Special Character Lists
#'
#' Returns a list of function words based on the Linguistic Inquiry and Word Count 2015 dictionary
#' (in terms of category names -- words were selected independently), or a list of special characters and patterns.
#' @param ... Numbers or letters corresponding to category names: ppron, ipron, article,
#' adverb, conj, prep, auxverb, negate, quant, interrog, number, interjection, or special.
#' @param as.regex Logical: if \code{FALSE}, lists are returned without regular expression.
#' @param as.function Logical or a function: if specified and \code{as.regex} is \code{TRUE}, the selected dictionary
#' will be collapsed to a regex string (terms separated by `|`), and a function for matching characters to that
#' string will be returned. The regex string is passed to the matching function (\code{\link{grepl}} by default)
#' as a 'pattern' argument, with the first argument of the returned function being passed as an 'x' argument.
#' See examples.
#' @note
#' The \code{special} category is not returned unless specifically requested. It is a list of regular expression
#' strings attempting to capture special things like ellipses and emojis, or sets of special characters (those outside
#' of the Basic Latin range; \code{[^\\u0020-\\u007F]}), which can be used for character conversions.
#' If \code{special} is part of the returned list, \code{as.regex} is set to \code{TRUE}.
#'
#' The \code{special} list is always used by both \code{\link{lma_dtm}} and \code{\link{lma_termcat}}. When creating a
#' dtm, \code{special} is used to clean the original input (so that, by default, the punctuation involved in ellipses
#' and emojis are treated as different -- as ellipses and emojis rather than as periods and parens and colons and such).
#' When categorizing a dtm, the input dictionary is passed by the special lists to be sure the terms in the dtm match up
#' with the dictionary (so, for example, ": (" would be replaced with "repfrown" in both the text and dictionary).
#' @seealso To score texts with these categories, use \code{\link{lma_termcat}}.
#' @examples
#' # return the full dictionary (excluding special)
#' lma_dict()
#'
#' # return the standard 7 category lsm categories
#' lma_dict(1:7)
#'
#' # return just a few categories without regular expression
#' lma_dict(neg, ppron, aux, as.regex=FALSE)
#'
#' # return special specifically
#' lma_dict(special)
#'
#' # returning a function
#' is.ppron = lma_dict(ppron, as.function = TRUE)
#' is.ppron(c('i', 'am', 'you', 'were'))
#'
#' in.lsmcat = lma_dict(1:7, as.function = TRUE)
#' in.lsmcat(c('a', 'frog', 'for', 'me'))
#'
#' ## use as a stopword filter
#' is.stopword = lma_dict(as.function = TRUE)
#' dtm = lma_dtm('Most of these words might not be all that relevant.')
#' dtm[, !is.stopword(colnames(dtm))]
#'
#' ## use to replace special characters
#' clean = lma_dict(special, as.function = gsub)
#' clean(c(
#'   "\u201Ccurly quotes\u201D", 'na\u00EFve', 'typographer\u2019s apostrophe',
#'   'en\u2013dash', 'em\u2014dash'
#' ))
#' @export

lma_dict = function(..., as.regex = TRUE, as.function = FALSE){
  cats = as.character(substitute(...()))
  dict = list(
    ppron = c("^dae$", "^dem$", "^eir$", "^eirself$", "^em$", "^he$", "^he'", "^her$", "^hers$", "^herself$", "^hes$",
      "^him$", "^himself$", "^hir$", "^hirs$", "^hirself$", "^his$", "^hisself$", "^i$", "^i'", "^id$", "^idc$",
      "^idgaf$", "^idk$", "^idontknow$", "^idve$", "^iirc$", "^iknow$", "^ikr$", "^ill$", "^ily$", "^im$", "^ima$",
      "^imean$", "^imma$", "^ive$", "^lets$", "^let's$", "^me$", "^methinks$", "^mine$", "^my$", "^myself$", "^omfg$",
      "^omg$", "^oneself$", "^our$", "^ours", "^she$", "^she'", "^shes$", "^thee$", "^their$", "^their'", "^theirs",
      "^them$", "^thems", "^they$", "^they'", "^theyd$", "^theyll$", "^theyve$", "^thine$", "^thou$", "^thoust$",
      "^thy$", "^thyself$", "^u$", "^u'", "^ud$", "^ull$", "^ur$", "^ure$", "^us$", "^we$", "^we'", "^weve$", "^y'",
      "^ya'", "^yall", "^yins$", "^yinz$", "^you$", "^you'", "^youd$", "^youll$", "^your$", "^youre$", "^yours$",
      "^yourself$", "^yourselves$", "^youve$", "^zer$", "^zir$", "^zirs$", "^zirself$", "^zis$"),
    ipron = c("^another$", "^anybo", "^anyone", "^anything", "^dat$", "^de+z$", "^dis$", "^everyb", "^everyone",
      "^everything", "^few$", "^it$", "^it'$", "^it'", "^itd$", "^itll$", "^its$", "^itself$", "^many$", "^nobod",
      "^nothing$", "^other$", "^others$", "^same$", "^somebo", "^somebody'", "^someone", "^something", "^stuff$",
      "^that$", "^that'", "^thatd$", "^thatll$", "^thats$", "^these$", "^these'", "^thesed$", "^thesell$", "^thesere$",
      "^thing", "^this$", "^this'", "^thisd$", "^thisll$", "^those$", "^those'", "^thosed$", "^thosell$", "^thosere$",
      "^what$", "^what'", "^whatd$", "^whatever$", "^whatll$", "^whats$", "^which", "^who$", "^who'", "^whod$",
      "^whoever$", "^wholl$", "^whom$", "^whomever$", "^whos$", "^whose$", "^whosever$", "^whosoever$"),
    article = c("^a$", "^an$", "^da$", "^teh$", "^the$"),
    adverb = c("^absolutely$", "^actively$", "^actually$", "^afk$", "^again$", "^ago$", "^ahead$", "^almost$",
      "^already$", "^altogether$", "^always$", "^angrily$", "^anxiously$", "^any$", "^anymore$", "^anyway$",
      "^anywhere$", "^apparently$", "^automatically$", "^away$", "^awhile$", "^back$", "^badly$", "^barely$",
      "^basically$", "^below$", "^brietermsy$", "^carefully$", "^causiously$", "^certainly$", "^clearly$", "^closely$",
      "^coldly$", "^commonly$", "^completely$", "^constantly$", "^continually$", "^correctly$", "^coz$", "^currently$",
      "^daily$", "^deeply$", "^definitely$", "^definitly$", "^deliberately$", "^desperately$", "^differently$",
      "^directly$", "^early$", "^easily$", "^effectively$", "^elsewhere$", "^enough$", "^entirely$", "^equally$",
      "^especially$", "^essentially$", "^etc$", "^even$", "^eventually$", "^ever$", "^every$", "^everyday$",
      "^everywhere", "^exactly$", "^exclusively$", "^extremely$", "^fairly$", "^far$", "^finally$", "^fortunately$",
      "^frequently$", "^fully$", "^further$", "^generally$", "^gently$", "^genuinely$", "^good$", "^greatly$",
      "^hardly$", "^heavily$", "^hence$", "^henceforth$", "^hereafter$", "^herein$", "^heretofore$", "^hesitantly$",
      "^highly$", "^hither$", "^hopefully$", "^hotly$", "^however$", "^immediately$", "^importantly$", "^increasingly$",
      "^incredibly$", "^indeed$", "^initially$", "^instead$", "^intensely$", "^jus$", "^just$", "^largely$", "^lately$",
      "^least$", "^legitimately$", "^less$", "^lightly$", "^likely$", "^literally$", "^loudly$", "^luckily$",
      "^mainly$", "^maybe$", "^meanwhile$", "^merely$", "^more$", "^moreover$", "^most$", "^mostly$", "^much$",
      "^namely$", "^naturally$", "^nearly$", "^necessarily$", "^nervously$", "^never$", "^nevertheless$", "^no$",
      "^nonetheless$", "^normally$", "^not$", "^notwithstanding$", "^obviously$", "^occasionally$", "^often$", "^once$",
      "^only$", "^originally$", "^otherwise$", "^overall$", "^particularly$", "^passionately$", "^perfectly$",
      "^perhaps$", "^personally$", "^physically$", "^please$", "^possibly$", "^potentially$", "^practically$",
      "^presently$", "^previously$", "^primarily$", "^probability$", "^probably$", "^profoundly$", "^prolly$",
      "^properly$", "^quickly$", "^quietly$", "^quite$", "^randomly$", "^rarely$", "^rather$", "^readily$", "^really$",
      "^recently$", "^regularly$", "^relatively$", "^respectively$", "^right$", "^roughly$", "^sadly$", "^seldomly$",
      "^seriously$", "^shortly$", "^significantly$", "^similarly$", "^simply$", "^slightly$", "^slowly$", "^so$",
      "^some$", "^somehow$", "^sometimes$", "^somewhat$", "^somewhere$", "^soon$", "^specifically$", "^still$",
      "^strongly$", "^subsequently$", "^successfully$", "^such$", "^suddenly$", "^supposedly$", "^surely$",
      "^surprisingly$", "^technically$", "^terribly$", "^thence$", "^thereafter$", "^therefor$", "^therefore$",
      "^thither$", "^thoroughly$", "^thus$", "^thusfar$", "^thusly$", "^together$", "^too$", "^totally$", "^truly$",
      "^typically$", "^ultimately$", "^uncommonly$", "^unfortunately$", "^unfortunatly$", "^usually$", "^vastly$",
      "^very$", "^virtually$", "^well$", "^whence$", "^where", "^wherefor", "^whither$", "^wholly$", "^why$", "^why'",
      "^whyd$", "^whys$", "^widely$", "^wither$", "^yet$"),
    conj = c("^also$", "^altho$", "^although$", "^and$", "^b/c$", "^bc$", "^because$", "^besides$", "^both$", "^but$",
      "^'cause$", "^cos$", "^cuz$", "^either$", "^else$", "^except$", "^for$", "^how$", "^how'", "^howd$", "^howll$",
      "^hows$", "^if$", "^neither$", "^nor$", "^or$", "^than$", "^tho$", "^though$", "^unless$", "^unlike$", "^versus$",
      "^vs$", "^when$", "^when'", "^whenever$", "^whereas$", "^whether$", "^while$", "^whilst$"),
    prep = c("^about$", "^above$", "^abt$", "^across$", "^acrost$", "^afk$", "^after$", "^against$", "^along$", "^amid",
      "^among", "^around$", "^as$", "^at$", "^atop$", "^before$", "^behind$", "^beneath$", "^beside$", "^betwe",
      "^beyond$", "^by$", "^despite$", "^down$", "^during$", "^excluding$", "^from$", "^here$", "^here'", "^heres$",
      "^in$", "^including$", "^inside$", "^into$", "^minus$", "^near$", "^now$", "^of$", "^off$", "^on$", "^onto$",
      "^out$", "^outside$", "^over$", "^plus$", "^regarding$", "^sans$", "^since$", "^then$", "^there$", "^there'",
      "^thered$", "^therell$", "^theres$", "^through$", "^throughout$", "^thru$", "^til$", "^till$", "^to$", "^toward",
      "^under$", "^underneath$", "^until$", "^untill$", "^unto$", "^up$", "^upon$", "^via$", "^with$", "^within$",
      "^without$", "^worth$"),
    auxverb = c("^am$", "^are$", "^arent$", "^aren't$", "^be$", "^been$", "^bein$", "^being$", "^brb$", "^can$",
      "^could$", "^could'", "^couldnt$", "^couldn't$", "^couldve$", "^did$", "^didnt$", "^didn't$", "^do$", "^does$",
      "^doesnt$", "^doesn't$", "^doing$", "^dont$", "^don't$", "^had$", "^hadnt$", "^hadn't$", "^has$", "^hasnt$",
      "^hasn't$", "^have$", "^havent$", "^haven't$", "^having$", "^is$", "^isnt$", "^isn't$", "^may$", "^might$",
      "^might'", "^mightnt$", "^mightn't$", "^mightve$", "^must$", "^mustnt$", "^mustn't$", "^mustve$", "^ought",
      "^shant$", "^shan't$", "^sha'nt$", "^shall$", "^should$", "^shouldnt$", "^shouldn't$", "^shouldve$", "^was$",
      "^wasnt$", "^wasn't$", "^were$", "^werent$", "^weren't$", "^will$", "^would$", "^would'", "^wouldnt", "^wouldn't",
      "^wouldve$"),
    negate = c("^ain't$", "^aint$", "^aren't$", "^arent$", "^can't$", "^cannot$", "^cant$", "^couldn't$", "^couldnt$",
      "^didn't$", "^didnt$", "^doesn't$", "^doesnt$", "^don't$", "^dont$", "^hadn't$", "^hadnt$", "^hasn't$", "^hasnt$",
      "^haven't$", "^havent$", "^idk$", "^isn't$", "^isnt$", "^must'nt$", "^mustn't$", "^mustnt$", "^nah", "^need'nt$",
      "^needn't$", "^neednt$", "^negat", "^neither$", "^never$", "^no$", "^nobod", "^noes$", "^none$", "^nope$",
      "^nor$", "^not$", "^nothing$", "^nowhere$", "^np$", "^ought'nt$", "^oughtn't$", "^oughtnt$", "^shant$",
      "^shan't$", "^sha'nt$", "^should'nt$", "^shouldn't$", "^shouldnt$", "^uh-uh$", "^wasn't$", "^wasnt$", "^weren't$",
      "^werent$", "^without$", "^won't$", "^wont$", "^wouldn't$", "^wouldnt$"),
    quant = c("^add$", "^added$", "^adding$", "^adds$", "^all$", "^allot$", "^alot$", "^amount$", "^amounts$",
      "^another$", "^any$", "^approximat", "^average$", "^bit$", "^bits$", "^both$", "^bunch$", "^chapter$", "^couple$",
      "^doubl", "^each$", "^either$", "^entire", "^equal", "^every$", "^extra$", "^few$", "^fewer$", "^fewest$",
      "^group", "^inequal", "^least$", "^less$", "^lot$", "^lotof$", "^lots$", "^lotsa$", "^lotta$", "^majority$",
      "^many$", "^mo$", "^mo'", "^more$", "^most$", "^much$", "^mucho$", "^multiple$", "^nada$", "^none$", "^part$",
      "^partly$", "^percent", "^piece$", "^pieces$", "^plenty$", "^remaining$", "^sampl", "^scarce$", "^scarcer$",
      "^scarcest$", "^section$", "^segment", "^series$", "^several", "^single$", "^singles$", "^singly$", "^some$",
      "^somewhat$", "^ton$", "^tons$", "^total$", "^triple", "^tripling$", "^variety$", "^various$", "^whole$"),
    interrog = c("^how$", "^how'd$", "^how're$", "^how's$", "^howd$", "^howre$", "^hows$", "^wat$", "^wattt", "^what$",
      "^what'd$", "^what'll$", "^what're$", "^what's$", "^whatd$", "^whatever$", "^whatll$", "^whatre$", "^whatt",
      "^when$", "^when'", "^whence$", "^whenever$", "^where$", "^where'd$", "^where's$", "^wherefore$", "^wherever$",
      "^whether$", "^which$", "^whichever$", "^whither$", "^who$", "^who'd$", "^who'll$", "^who's$", "^whoever$",
      "^wholl$", "^whom$", "^whomever$", "^whos$", "^whose$", "^whosever$", "^whoso", "^why$", "^why'", "^whyever$",
      "^wut$"),
    number = c("^billion", "^doubl", "^dozen", "^eight", "^eleven$", "^fift", "^first$", "^firstly$", "^firsts$",
      "^five$", "^four", "^half$", "^hundred", "^infinit", "^million", "^nine", "^once$", "^one$", "^quarter",
      "^second$", "^seven", "^single$", "^six", "^ten$", "^tenth$", "^third$", "^thirt", "^thousand", "^three$",
      "^trillion", "^twel", "^twent", "^twice$", "^two$", "^zero$", "^zillion"),
    interjection = c("^a+h+$", "^a+w+$", "^allas$", "^alright", "^anyhoo$", "^anyway[ysz]", "^bl[eh]+$", "^g+[eah]+$",
      "^h[ah]+$", "^h[hu]+$", "^h[mh]+$", "^l[ol]+$", "^m[hm]+$", "^meh$", "^o+h+$", "^o+k+$", "^okie", "^oo+f+$",
      "^soo+$", "^u[uh]+$", "^u+g+h+$", "^w[ow]+$", "^wee+ll+$", "^y[aes]+$", "^ya+h+$", "^yeah$", "^yus+$"),
    special = list(
      ELLIPSIS = '\\.{3, }|\\. +\\. +[. ]+',
      SMILE = '\\s(?:[[{(<qd]+[\\s<-]*[;:8=]|[;:8=][\\s>-]*[]})>Dpb]+|[uUnwWmM^=+-]_[uUnwWmM^=+-])(?=\\s)',
      FROWN = '\\s(?:[]D)}>]+[\\s.,<-]*[;:8=]|[;:8=][\\s.,>-]*[[{(<]+|[Tt:;]_[Tt;:]|[uUtT;:][mMn][uUtT;:])(?=\\s)',
      LIKE = c('(?<=could not) like\\b', '(?<=did not) like\\b', '(?<=did) like\\b','(?<=didn\'t) like\\b',
        '(?<=do not) like\\b', '(?<=do) like\\b', '(?<=does not) like\\b', '(?<=does) like\\b', '(?<=doesn\'t) like\\b',
        '(?<=don\'t) like\\b', '(?<=i) like\\b', '(?<=should not) like\\b', '(?<=they) like\\b', '(?<=we) like\\b',
        '(?<=will not) like\\b', '(?<=will) like\\b', '(?<=won\'t) like\\b', '(?<=would not) like\\b',
        '(?<=you) like\\b'),
      CHARACTERS = c(
        ` ` = '\\s',
        `'` = paste0('[\u00B4\u2018\u2019\u201A\u201B\u2032\u2035\u02B9\u02BB\u02BE\u02BF\u02C8\u02CA\u02CB\u02F4',
          '\u0300\u0301\u030D\u0312\u0313\u0314\u0315\u031B\u0321\u0322\u0326\u0328\u0329\u0340\u0341\u0343\u0351',
          '\u0357]'),
        `"` = '[\u201C\u201D\u201E\u201F\u2033\u2034\u2036\u2037\u2057\u02BA\u02DD\u02EE\u02F5\u02F6\u030B\u030F]',
        `...` = '\u2026',
        `-` = '[\u05BE\u1806\u2010\u2011\u2013\uFE58\uFE63\uFF0D]',
        ` - ` = '[\u2012\u2014\u2015\u2E3A\u2E3B]|--+',
        a = paste0('[\u00C0\u00C1\u00C2\u00C3\u00C4\u00C5\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5\u0100\u0101\u0102',
          '\u0103\u0104\u105\u0200\u0201\u0202\u0203\u0226\u0227\u0245\u0250\u0251\u0252\u0255\u0363\u0386\u0391',
          '\u0410\u0430]'),
        ae = '[\u00C6\u00E6\u0152\u0153\u0276]',
        b = paste0('[\u00DF\u0180\u0181\u0182\u0183\u0184\u0185\u0186\u0187\u0188\u0189\u018A\u018B\u018C\u0243',
          '\u0253\u0299\u0411\u0412\u0431\u0432\u0462\u0463\u0494\u0495\u212C]'),
        c = paste0('[\u00C7\u00E7\u0106\u0107\u0108\u0109\u0186\u0187\u0188\u0254\u0297\u0368\u0421\u0441\u2102',
          '\u2103]'),
        d = paste0('[\u00D0\u00DE\u00FE\u010D\u010E\u010F\u0110\u0111\u0189\u0221\u0256\u0256\u0257\u0369\u0392',
          '\u0434\u0500\u2145\u2146]'),
        e = paste0('[\u00C8\u00C9\u00CA\u00CB\u00E8\u00E9\u00EA\u00EB\u0112\u0113\u0114\u0115\u0116\u0117\u0118',
          '\u0119\u011A\u011B\u018E\u018F\u0190\u0204\u0205\u0206\u0207\u0228\u0229\u0246\u0247\u0258\u0259\u0364',
          '\u0388\u0395\u0400\u0401\u0404\u0415\u0417\u0435\u0437\u0450\u0451\u0454\u0498\u0499\u2107\u2108\u2128',
          '\u212E\u212F\u2130\u2147]'),
        f = '[\u0191\u0192\u0492\u0493\u2109\u2231\u2132\u214E]',
        g = '[\u011C\u011D\u011E\u011F\u0120\u0121\u0122\u0123\u0193\u0222\u0260\u0261\u0262\u210A\u2141]',
        h = '[\u0124\u0125\u0127\u0195\u0266\u0267\u0389\u0397\u0452\u210B\u210C\u210D\u210E\u210F]',
        i = paste0('[\u00CC\u00CD\u00CE\u00CF\u00EC\u00ED\u00EE\u00EF\u0128\u0129\u012A\u012B\u012C\u012D\u012E\u012F',
          '\u0130\u0131\u0197\u019A\u0208\u0209\u0365\u0390\u0399\u0406\u0407\u0456\u0457]'),
        j = '[\u0135\u0236\u0237\u0248\u0249\u0408\u0458\u2129\u2139\u2149]',
        k = '[\u0137\u0138\u0198\u0199\u212A]',
        l = '[\u0139\u013A\u013B\u013C\u013D\u013E\u013F\u0140\u0141\u0142\u0234]',
        m = '[\u0271\u0460\u2133]',
        n = paste0('[\u00D1\u00F1\u0143\u0144\u0145\u0146\u0147\u0148\u0149\u014A\u014B\u0220\u0235\u0272\u0273',
          '\u0274\u0376\u0377\u0418\u0419\u0438\u0439\u2115\u2135]'),
        h = '\u0149',
        o = paste0('[\u00D2\u00D3\u00D4\u00D5\u00D6\u00D8\u00F0\u00F2\u00F3\u00F4\u00F5\u00F6\u00F8\u014C\u014D',
          '\u014E\u014F\u0150\u0151\u0150\u0151\u0230\u0231\u0275\u0298\u0366\u0398\u0424\u0444\u0472\u0473\u2134]'),
        p = '[\u0420\u0440\u2117\u2118\u2119]',
        q = '[\u018D\u211A\u213A]',
        r = paste0('[\u0154\u0155\u0156\u0157\u0158\u0159\u0211\u0212\u0213\u0279\u0280\u0281\u0433\u0453\u0490',
          '\u0491\u211B\u211C\u211D\u211F\u213E]'),
        s = '[\u015A\u015C\u015D\u015E\u015F\u0160\u0161\u0160\u0161\u0218\u0219\u0405\u0455]',
        t = '[\u0162\u0163\u0164\u0165\u0166\u0167\u0371\u0373\u0422\u0442]',
        u = paste0('[\u00D9\u00DA\u00DB\u00DC\u00F9\u00FA\u00FB\u00FC\u00FC\u0168\u0169\u016A\u016B\u016C\u016D',
          '\u016E\u016F\u0170\u0171\u0172\u0173\u01D3\u01D4\u01D5\u01D6\u01D7\u01D8\u01D9\u01DA\u01DB\u01DC\u0214',
          '\u0217\u0244\u0289\u0367\u0426\u0446]'),
        v = '[\u0474\u0475\u0476\u0477]',
        w = '[\u0174\u0175\u0270\u0428\u0429\u0448\u0449\u0461]',
        y = '[\u00DD\u00FD\u00FF\u0176\u0177\u0178\u0232\u0233\u0423\u0427\u0443\u0447]',
        z = '[\u0179\u017A\u017B\u017C\u017E\u0224\u0225\u0240\u0290\u0291\u0396\u2124]',
        x = '[\u00D7\u0416\u0425\u0436\u0445\u0496\u0497]'
      ),
      SYMBOLS = c(
        `(cc)` = '\u00A9',
        number = '\u2116',
        sm = '\u2120',
        tel = '\u2121',
        `(tm)` = '\u2122',
        omega = '\u2126',
        alpha = '\u2127',
        fax = '\u213B',
        pi = '[\u213C\u213F]',
        sigma = '\u2140'
      )
    )
  )
  if(length(cats) == 0) cats = names(dict)[-length(dict)]
  if(length(cats) == 1 && grepl('\\(|\\[', cats)) cats = eval(parse(text = cats))
  if(any(grepl('[0-9]|seq', cats))) cats = if(length(cats) > 1) as.numeric(cats) else eval(parse(text = cats))
  if(is.numeric(cats)){
    cats = cats[cats < length(dict)]
  }else if(any(!cats %in% names(dict))) cats = grep(paste(paste0('^', cats), collapse = '|'), names(dict), value = TRUE)
  if(length(cats) == 0) stop(
    '\n  enter numbers between 1 and ', length(dict) - 1,
    ', or letters matching a category:\n  ', paste(names(dict), collapse = ', ')
  )
  if('special' %in% cats) as.regex = TRUE
  if(as.regex){
    if(!missing(as.function)){
      if('special' %in% cats && is.function(as.function) && grepl('sub', substitute(as.function))){
        dict = c(dict$special$CHARACTERS, dict$special$SYMBOLS)
        fun = as.function
        if(substitute(as.function) == 'gsub'){
          charmap = as.data.frame(unlist(lapply(as.list(dict), strsplit, '')))
          charmap = data.frame(to = sub('[0-9]+', '', rownames(charmap)), from = charmap[[1]])
          charmap = charmap[grepl('^\\w$', charmap$to) & !charmap$from %in% c('[', ']'),]
          dict = dict[!names(dict) %in% charmap$to]
          charmap = list(to = paste(charmap$to, collapse = ''), from = paste(charmap$from, collapse = ''))
        }else charmap = NULL
        function(terms, ...){
          args = list(...)
          args$x = terms
          if(!is.null(charmap)) args$x = chartr(charmap$from, charmap$to, args$x)
          for(s in names(dict)){
            args$pattern = dict[s]
            args$replacement = s
            args$x = do.call(fun, args)
          }
          args$x
        }
      }else{
        dict = paste(unlist(dict[cats]), collapse = '|')
        fun = if(is.function(as.function)) as.function else grepl
        function(terms, ...){
          args = list(...)
          args$pattern = dict
          args$x = terms
          if(!is.function(as.function) && !'perl' %in% names(args)) args$perl = TRUE
          do.call(fun, args)
        }
      }
    }else dict[cats]
  }else lapply(dict[cats], function(l) gsub('\\^|\\$', '', sub('(?<=[^$])$', '*', l, perl = TRUE)))
}
