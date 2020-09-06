#' Process text using a combination of pre-processing steps
#'
#' A wrapper to other pre-processing functions, potentially from \code{\link{read.segments}}, to \code{\link{lma_dtm}}
#' or \code{\link{lma_patcat}}, to \code{\link{lma_weight}}, then \code{\link{lma_termcat}} or \code{\link{lma_lspace}},
#' and potentially including \code{\link{lma_meta}} output.
#'
#' @param input A vector of text, or path to a text file or folder.
#' @param ... arguments to be passed to \code{\link{lma_dtm}}, \code{\link{lma_patcat}}, \code{\link{lma_weight}},
#'   \code{\link{lma_termcat}}, and/or \code{\link{lma_lspace}}. All arguments must be named.
#' @param meta Logical; if \code{FALSE}, metastatistics are not included. Only applies when raw text is available.
#'  If included, meta categories are added as the last columns, with names starting with "meta_".
#' @return A matrix with texts represented by rows, and features in columns.
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
    attr(x, 'category') = attr(arg_matches$lma_weight$dtm, 'category')
    ck_changed = TRUE
  }
  if(!is.null(attr(x, 'category'))){
    categories = attr(x, 'category')
    cats = unique(categories)
    xc = as.data.frame(matrix(0, nrow(op), length(unique(cats)), dimnames = list(NULL, cats)))
    for(cat in cats) xc[, cat] = rowSums(x[, categories == cat, drop = FALSE], na.rm = TRUE)
    x = xc
    ck_changed = TRUE
  }else if(length(arg_matches$lma_termcat)){
    arg_matches$lma_termcat$dtm = x
    x = do.call(lma_termcat, arg_matches$lma_termcat)
    ck_changed = TRUE
  }
  if(length(arg_matches$lma_lspace)){
    arg_matches$lma_lspace$dtm = x
    x = do.call(lma_lspace, arg_matches$lma_lspace)
    colnames(x) = paste0('dim', seq_len(ncol(x)))
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

#' Read/write LIWC dictionary files
#'
#' Read in or write Linguistic Inquiry and Word Count dictionary (.dic) files.
#' @param path Path to a .dic file.
#' @param cats A character vector of category names to be returned. All categories are returned by default.
#' @param to.regex Logical; if \code{TRUE}, each dictionary entry is converted to regular expression
#'   (starts are marked with ^ and ends with $ when an * is present, and unmatched brackets/parens are escaped).
#' @export

read.dic = function(path, cats, to.regex = FALSE){
  if(missing(path)) path = file.choose()
  di = if(length(path) != 1) path else tryCatch(
    readLines(path, warn = FALSE),
    error = function(e) stop('failed to read path: ', e$message, call. = FALSE)
  )
  lst = grep('%', di, fixed = TRUE)
  if(length(lst) > 1){
    di = di[-seq_len(lst[1])]
    lst = lst[2] - 2
  }else stop('file is not in the expected format')
  ci = strsplit(di[seq_len(lst)], '\\s+')
  names(ci) = vapply(ci, '[[', '', 2)
  if(missing(cats)) cats = names(ci)
  ci = lapply(ci[names(ci) %in% cats], '[[', 1)
  di = strsplit(di[seq_along(di)[-(1:3)]], '[ \t]+(?=[0-9]|$)', perl = TRUE)
  di = di[vapply(di, length, 0) > 1]
  names(di) = vapply(di, '[', '', 1)
  di = lapply(di, '[', -1)
  wl = list()
  for(w in names(di)){
    ck = ci %in% di[[w]]
    if(any(ck)){
      cm = names(ci[ck])
      for(c in cm) wl[[c]] = c(wl[[c]], w)
    }
  }
  if(to.regex) lapply(wl, function(l){
    if(any(ck <- grepl('[[({]', l) + grepl('[})]|\\]', l) == 1))
      l[ck] = gsub('([([{}\\])])', '\\\\\\1', l[ck], perl = TRUE)
    gsub('\\^\\*|\\*\\$', '', paste0('^', l, '$'))
  }) else wl
}

#' @rdname read.dic
#' @param dict A list object with names of categories and a vector of their words.
#' @param filename The name of the file to be saved.
#' @param save Logical; if \code{FALSE}, does not write a file.
#' @return a character vector
#' @examples
#' # make a small murder related dictionary
#' dict = list(
#'   kill = c('kill*', 'murd*', 'wound*', 'die*'),
#'   death = c('death*', 'dying', 'die*', 'kill*')
#' )
#'
#' \dontrun{
#'
#' write.dic(dict, 'murder') # save it as a .dic file
#' read.dic('murder.dic') # read it back in as a list
#'
#' # read in the Moral Foundations or LUSI dictionaries from urls
#' moral_dict = read.dic('http://bit.ly/MoralFoundations2')
#' lusi_dict = read.dic('http://bit.ly/lusi_dict')
#' }
#' @export

write.dic = function(dict, filename = 'custom', save = TRUE){
  filename = filename[[1]]
  filename = paste0(if(!grepl(':', filename, fixed = TRUE)) paste0(getwd(), '/'), filename,
    if(!grepl('\\.[a-zA-Z0-9]+$', filename)) '.dic')
  terms = unique(as.character(unlist(dict, use.names = FALSE)))
  l = length(dict)
  m = as.data.frame(matrix('', length(terms) + l + 2, l + 1))
  m[, 1] = c('%', seq_len(l), '%', terms)
  m[seq_len(l) + 1, 2] = if(is.null(names(dict))) seq_len(l) else names(dict)
  for(i in seq_along(dict)) m[which(m[-seq_len(i + 2), 1] %in% dict[[i]]) + i + 2, i + 1] = i
  o = gsub('\t{2,}', '\t', paste(sub('\t+$', '', do.call(paste, c(m, sep = '\t'))), collapse = '\n'))
  if(save){
    write(o, filename)
    message('dictionary saved to ', filename)
  }
  invisible(o)
}

#' Read and segment multiple texts
#'
#' Split texts by word count or specific characters. Input texts directly, or read them in from files.
#'
#' @param path Path to a folder containing files, or a vector of paths to files. If no folders or files are
#'   recognized in \code{path}, it is treated as \code{text}.
#' @param segment Specifies how the text of each file should be segmented. If a character, split at that character;
#'   '\\n' by default. If a number, texts will be broken into that many segments, each with a roughly equal number of words.
#' @param ext The extension of the files you want to read in. '.txt' by default.
#' @param subdir If \code{TRUE} files in folders in \code{path} will also be included.
#' @param segment.size If specified, \code{segment} will be ignored, and texts will be broken into segments containing roughly
#'   \code{segment.size} number of words.
#' @param bysentence If \code{TRUE}, and \code{segment} is a number or \code{segment.size} is specified, sentences will be
#'   kept together, rather than potentially being broken across segments.
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
  if(!any(dir.exists(path) | file.exists(path))){
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
  if(missing(segment) && missing(segment.size)) segment = 1
  if(length(files)){
    err = function(e) NULL
    args = list(what = character(), quote = '', na.strings = '', quiet = TRUE)
    if(is.character(segment) && segment.size == -1) args$sep = segment
    do.call(rbind, lapply(seq_along(files), function(fi){
      f = files[fi]
      args[[if(ck_text) 'text' else 'file']] = f
      WC = NULL
      if(is.numeric(segment) || segment.size > 0){
        words = tryCatch(do.call(scan, args), error = err)
        if(!length(words)) return(NULL)
        TWC = length(words)
        if(segment.size == -1) segment.size = ceiling(TWC / segment)
        if(bysentence){
          if(!is.null(segment)){
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
          i = 1
          s = 1
          p = 2
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
#'   \code{dir}, the term map \href{https://osf.io/xr7jv}{lma_term_map.rda} is
#'   downloaded and decompressed.
#' @param check.md5 Logical; if \code{TRUE} (default), retrieves the MD5 checksum from OSF,
#'   and compares it with that calculated from the downloaded file to check its integrity.
#' @param mode Passed to \code{\link{download.file}} when downloading the term map.
#' @return a list with varying entries:
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
#'             https://osf.io/{osf_dat} \cr
#'           \code{osf_terms} \tab OSF id for the \code{_terms.txt} files; the URL would be
#'             https://osf.io/{osf_terms} \cr
#'           \code{wiki} \tab link to the wiki for the space \cr
#'           \code{downloaded} \tab indicates whether the space's files are found in \code{dir} \cr
#'         }
#'       \cr
#'     selected \tab A subset of \code{spaces} selected by \code{query}. \cr
#'     term_map \tab If \code{get.map} is \code{TRUE} or \code{lma_term_map.rda} is found in
#'       \code{dir}, a copy of \href{https://osf.io/xr7jv}{osf.io/xr7jv}, which has space names as
#'       column names, terms as row names, and indices as values, with 0 indicating the term is not
#'       present in the associated space. \cr
#'   }
#' @family Latent Semantic Space functions
#' @examples
#' # just retrieve information about available spaces
#' spaces = select.lsspace()
#'
#' # retrieve all spaces that used word2vec
#' w2v_spaces = select.lsspace('word2vec')$selected
#'
#' \dontrun{
#'
#' # select spaces by terms
#' select.lsspace(c(
#'   'part-time', 'i/o', "'cause", 'brexit', 'debuffs'
#' ))$selected[, c('terms', 'coverage')]
#' }
#' @export

select.lsspace = function(query = NULL, dir = getOption('lingmatch.lspace.dir'),
  get.map = FALSE, check.md5 = TRUE, mode = 'wb'){
  dir = sub('/+$', '', path.expand(dir))
  map_path = paste0(dir, '/lma_term_map.rda')
  if(!missing(query) && length(query) > 1) get.map = TRUE
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
          'double check and try manually downloading at https://osf.io/9yzca/?show=revision'
        ))
      }
    }
  }
  r = list(info = lss_info, selected = lss_info[NULL,])
  r$info[, 'wiki'] = paste0('https://osf.io/489he/wiki/', rownames(lss_info))
  r$info[, 'downloaded'] = grepl(paste(sub('\\..*$', '', list.files(dir, '\\.dat')), collapse = '|'),
    rownames(r$info), TRUE)
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
        r$info$coverage = colMeans(r$term_map[overlap,] != 0)
        r$selected = r$info[order(r$info$coverage, decreasing = TRUE)[1:5],]
        r$space_terms = overlap
      }else warning('query was treated as terms, but non were found')
    }else{
      if(!length(sel <- grep(query, rownames(lss_info), TRUE))){
        collapsed = vapply(seq_len(nrow(lss_info)),
          function(r) paste(c(rownames(lss_info)[r], lss_info[r,]), collapse = ' '), '')
        if(!length(sel <- grep(query, collapsed, TRUE)))
          sel <- grep(paste(strsplit(query, '[ ,]+')[[1]], collapse = '|'), collapsed, TRUE)
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
#' @param space Name of the space you want to download. '100k' is the default, and
#'  some other common options might be 'google', 'facebook', or 'glove'. See
#'  \href{https://osf.io/489he/wiki/home}{osf.io/489he/wiki} for more information, and a full
#'  list of spaces.
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
#' @examples
#' \dontrun{
#'
#' download.lsspace('glove_crawl')
#' }
#' @export
#' @importFrom utils download.file
#' @importFrom tools md5sum

download.lsspace = function(space = '100k', include.terms = TRUE, decompress = TRUE,
  check.md5 = TRUE, mode = 'wb', dir = getOption('lingmatch.lspace.dir')){
  dir = sub('/+$', '', path.expand(dir))
  if(space == 'default') space = '100k'
  name = grep(sub('\\..*$', '', space), rownames(lss_info), value = TRUE, fixed = TRUE)
  if(!length(name)) name = grep(substr(space, 1, 4), rownames(lss_info), TRUE, value = TRUE)
  if(!length(name)){
    stop('space ', space, ' not recognized; see https://osf.io/489he/wiki/home for available spaces')
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
        'failed to decompress; might try this from a system console:\nbunzip2 "', path.expand(o), '"'
      )
    }
  }
  message('downloaded ', name, ':\n  ', paste0(dir, '/', name, c(if(!status && decompress) '.dat' else
    '.dat.bz2', '_terms.txt'), collapse = '\n  '))
}

#' Standardize a Latent Semantic Space
#'
#' Reformat a .rda file which has a matrix with terms as row names, or a plain-text embeddings file
#' which has a term at the start of each line, and consistent delimiting characters. Plain-text files
#' are processed line-by-line, so large spaces can be reformatted RAM-conservatively.
#'
#' @param infile name of the .rda or plain-text file relative to \code{dir},
#'   e.g., "default.rda" or "glove/glove.6B.300d.txt".
#' @param name base name of the reformatted file and term file; e.g., "glove" would result in
#'   \code{glove.dat} and \code{glove_terms.txt} in \code{outdir}.
#' @param sep delimiting character between values in each line, e.g., \code{" "} or \code{"\\t"}.
#'   Only applies to plain-text files.
#' @param digits number of digits to round values to; default is 9.
#' @param dir path to folder containing \code{infile}s; default is \code{getOption('lingmatch.lspace.dir')}.
#' @param outdir path to folder in which to save standardized files; defaults to \code{dir}.
#' @param remove a string with a regex pattern to be removed from term names (as in \code{gsub(}\code{remove,}
#'   \code{"", term)}); default is \code{""}, which is ignored.
#' @param term_check a string with a regex pattern by which to filter terms; i.e., only lines with fully
#'   matched terms are written to the reformatted file. The default attempts to retain only regular words, including
#'   those with dashes, foreword slashes, and periods. Set to an empty string (\code{""}) to write all lines
#'   regardless of term.
#' @param verbose logical; if \code{TRUE}, prints the current line number and its term to the console every 1,000 lines.
#'   Only applies to plain-text files.
#' @family Latent Semantic Space functions
#' @examples
#' \dontrun{
#'
#' # from https://sites.google.com/site/fritzgntr/software-resources/semantic_spaces
#' standardize.lsspace('EN_100k_lsa.rda', '100k_lsa')
#'
#' # from https://fasttext.cc/docs/en/english-vectors.html
#' standardize.lsspace('crawl-300d-2M.vec', 'facebook_crawl')
#' }
#' @export

standardize.lsspace = function(infile, name, sep = ' ', digits = 9, dir = options('lingmatch.lspace.dir'),
  outdir = dir, remove = '', term_check = "^[a-zA-Z]+$|^['a-zA-Z][a-zA-Z.'\\/-]*[a-zA-Z.]$", verbose = FALSE){
  if(!is.character(term_check)) term_check = ''
  ip = paste0(sub('/+$', '', path.expand(dir)), '/', infile)
  op = paste0(sub('/+$', '', path.expand(outdir)), '/', name)
  cop = options(scipen = digits + 1)
  on.exit(options(cop))
  if(!is.character(infile) || grepl('\\.rda$', infile)){
    if(is.character(infile)){
      f = load(ip)
      o = get(f)
    }else o = infile
    m = max(o)
    o = round(o, digits)
    ot = rownames(o)
    if(term_check != ''){
      su = grepl(term_check, ot)
      o = o[su,]
      ot = ot[su]
    }
    if(remove != '') ot = gsub(remove, '', ot)
    writeLines(ot, paste0(op, '_terms.txt'))
    write(o, paste0(op, '.dat'), ncol(o))
    if(is.character(infile)) rm(f, 'o')
  }else reformat_embedding(ip, op, sep, digits, remove, term_check, verbose)
  message('created ', op, '.dat\nfrom ', ip)
}

#' Text Categorization
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
#'   summed and biased category value.
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
#' @seealso For applying term-based dictionaries (to a document-term matrix) see \code{\link{lma_termcat}}.
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
#' lma_patcat(text, 'i', boundary = TRUE)
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

lma_patcat = function(text, dict, pattern.weights = 'weight', pattern.categories = 'category', bias = NULL, to.lower = TRUE,
  return.dtm = FALSE, exclusive = TRUE, boundary = NULL, fixed = TRUE, globtoregex = FALSE,
  name.map = c(intname = '_intercept', term = 'term')){
  if(is.factor(text)) text = as.character(text)
  if(!is.character(text)) stop('enter a character vector as the first argument')
  text = paste(' ', text, ' ')
  if(to.lower) text = tolower(text)
  if(is.null(names(name.map)) && length(name.map) < 3) names(name.map) = c('intname', 'term')[seq_along(name.map)]
  wide = FALSE
  # independently entered wide weights
  if(is.null(colnames(dict)) && (!is.null(ncol(pattern.weights)) || !is.null(ncol(pattern.categories)))){
    weights = if(!is.null(ncol(pattern.weights))) pattern.weights else pattern.categories
    if(length(dict) != nrow(weights)) stop('dict and wide weights do not align')
    wide = TRUE
    if(!missing(pattern.categories) && is.character(pattern.categories) && any(su <- pattern.categories %in% weights))
      weights = weights[, pattern.categories[su], drop = FALSE]
    weights = weights[, vapply(seq_len(ncol(weights)), function(col) is.numeric(weights[, col]), TRUE), drop = FALSE]
    if(!ncol(weights)) stop('could not identify numeric weights in wide weights')
    lex = list(terms = dict, weights = weights, category = colnames(weights))
  # wide weights in dict
  }else if(!is.null(colnames(dict)) && (
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
  }else if(is.null(colnames(dict))){
    if((is.numeric(dict) && is.null(names(dict))) || (is.list(dict) && is.numeric(dict[[1]]) && is.null(names(dict[[1]]))))
      stop('could not recognize terms in dict')
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
        unlist(dict, use.names = FALSE) else if(is.numeric(dict[[1]])) unlist(pattern.weights, use.names = FALSE) else 1 else 1
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
    lex$term = sub('^\\*', '\\\\\\b\\\\\\w*', sub('\\*$', '\\\\\\w*\\\\\\b', lex$term, TRUE), TRUE)
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
    if(wide){
      o = order(-nchar(lex$term))
      lex$term = lex$term[o]
      lex$weights = lex$weights[o,]
    }else lex = lex[order(-nchar(lex$term)),]
  }
  lex$category = factor(lex$category, unique(lex$category))
  categories = levels(lex$category)
  if(length(bias)){
    if(is.null(names(bias)) && length(bias) == length(categories)) names(bias) = categories
    if(any(su <- !categories %in% names(bias))) bias[categories[su]] = 0
  }else bias = structure(integer(length(categories)), names = categories)
  bias = bias[categories]
  if(is.logical(boundary) && boundary) boundary = ' '
  st = proc.time()[[3]]
  op = pattern_search(
    text, if(is.character(boundary)) paste0(boundary, lex$term, boundary) else lex$term,
    if(return.dtm) 0L else nlevels(lex$category),
    (if(return.dtm) seq_along(lex$term) else as.integer(lex$category)) - 1L,
    if(!is.data.frame(lex)) as.numeric(as.matrix(lex$weight)) else lex$weight, as.numeric(bias), fixed,
    exclusive, if(wide) nlevels(lex$category) else 0
  )
  colnames(op[[1]]) = if(return.dtm) lex$term else categories
  if(return.dtm) attr(op[[1]], 'category') = lex$category
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

#' English function word category lists
#'
#' Returns a list of function words based on the Linguistic Inquiry and Word Count 2015 dictionary
#' (in terms of category names -- words were selected independently).
#' @param ... Numbers or letters corresponding to category names: ppron, ipron, article,
#' adverb, conj, prep, auxverb, negate, quant, interrog, number, interjection, or special.
#' @param as.regex Logical: if \code{FALSE}, lists are returned without regular expression.
#' @param as.function Logical or a function: if specified and \code{as.regex} is \code{TRUE}, the selected dictionary
#' will be collapsed to a regex string (terms separated by `|`) and function for matching characters to that
#' string will be returned. The regex string is passed to the matching function (\code{\link{grepl}} by default)
#' as a 'pattern' argument, with the first argument of the returned function being passed as an 'x' argument.
#' See examples.
#' @note
#' The \code{special} category is not returned unless specifically requested. It is a list of regular expression
#' strings attempting to capture special things like ellipses and emojis. If \code{special} is part of the returned list,
#' \code{as.regex} is set to \code{TRUE}.
#'
#' The \code{special} list is always used by both \code{\link{lma_dtm}} and \code{\link{lma_termcat}}. When creating a dtm,
#' \code{special} is used to clean the original input (so that, by default, the punctuation involved in ellipses and emojis
#' are treated as different -- as ellipses and emojis rather than as periods and parens and colons and such). When categorizing
#' a dtm, the input dictionary is passed by the special lists to be sure the terms in the dtm match up with the dictionary
#' (so, for example, ": (" would be replaced with "FROWN" in both the text and dictionary).
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
#' is.lsmcat = lma_dict(1:7, as.function = TRUE)
#' is.lsmcat(c('a', 'frog', 'for', 'me'))
#'
#' ## use as a stopword filter
#' is.stopword = lma_dict(as.function = TRUE)
#' dtm = lma_dtm('Most of these words might not be all that relevant.')
#' dtm[, !is.stopword(colnames(dtm))]
#'
#' @export

lma_dict=function(..., as.regex = TRUE, as.function = FALSE){
  cats = as.character(substitute(...()))
  dict=list(
    ppron=c("^dae$","^dem$","^eir$","^eirself$","^em$","^he$","^he'","^her$","^hers$","^herself$","^hes$","^him$","^himself$",
      "^hir$","^hirs$","^hirself$","^his$","^hisself$","^i$","^i'","^id$","^idc$","^idgaf$","^idk$","^idontknow$","^idve$",
      "^iirc$","^iknow$","^ikr$","^ill$","^ily$","^im$","^ima$","^imean$","^imma$","^ive$","^lets$","^let's$","^me$",
      "^methinks$","^mine$","^my$","^myself$","^omfg$","^omg$","^oneself$","^our$","^ours","^she$","^she'","^shes$","^thee$",
      "^their$","^their'","^theirs","^them$","^thems","^they$","^they'","^theyd$","^theyll$","^theyve$","^thine$","^thou$",
      "^thoust$","^thy$","^thyself$","^u$","^u'","^ud$","^ull$","^ur$","^ure$","^us$","^we$","^we'","^weve$","^y'","^ya'",
      "^yall","^yins$","^yinz$","^you$","^you'","^youd$","^youll$","^your$","^youre$","^yours$","^yourself$","^yourselves$",
      "^youve$","^zer$","^zir$","^zirs$","^zirself$","^zis$"),
    ipron=c("^another$","^anybo","^anyone","^anything","^dat$","^de+z$","^dis$","^everyb","^everyone","^everything","^few$",
      "^it$","^it'$","^it'","^itd$","^itll$","^its$","^itself$","^many$","^nobod","^nothing$","^other$","^others$","^same$",
      "^somebo","^somebody'","^someone","^something","^stuff$","^that$","^that'","^thatd$","^thatll$","^thats$","^these$",
      "^these'","^thesed$","^thesell$","^thesere$","^thing","^this$","^this'","^thisd$","^thisll$","^those$","^those'",
      "^thosed$","^thosell$","^thosere$","^what$","^what'","^whatd$","^whatever$","^whatll$","^whats$","^which","^who$",
      "^who'","^whod$","^whoever$","^wholl$","^whom$","^whomever$","^whos$","^whose$","^whosever$","^whosoever$"),
    article=c("^a$","^an$","^da$","^teh$","^the$"),
    adverb=c("^absolutely$","^actively$","^actually$","^afk$","^again$","^ago$","^ahead$","^almost$","^already$",
      "^altogether$","^always$","^angrily$","^anxiously$","^any$","^anymore$","^anyway$","^anywhere$","^apparently$",
      "^automatically$","^away$","^awhile$","^back$","^badly$","^barely$","^basically$","^below$","^brietermsy$","^carefully$",
      "^causiously$","^certainly$","^clearly$","^closely$","^coldly$","^commonly$","^completely$","^constantly$",
      "^continually$","^correctly$","^coz$","^currently$","^daily$","^deeply$","^definitely$","^definitly$","^deliberately$",
      "^desperately$","^differently$","^directly$","^early$","^easily$","^effectively$","^elsewhere$","^enough$","^entirely$",
      "^equally$","^especially$","^essentially$","^etc$","^even$","^eventually$","^ever$","^every$","^everyday$","^everywhere",
      "^exactly$","^exclusively$","^extremely$","^fairly$","^far$","^finally$","^fortunately$","^frequently$","^fully$",
      "^further$","^generally$","^gently$","^genuinely$","^good$","^greatly$","^hardly$","^heavily$","^hence$","^henceforth$",
      "^hereafter$","^herein$","^heretofore$","^hesitantly$","^highly$","^hither$","^hopefully$","^hotly$","^however$",
      "^immediately$","^importantly$","^increasingly$","^incredibly$","^indeed$","^initially$","^instead$","^intensely$",
      "^jus$","^just$","^largely$","^lately$","^least$","^legitimately$","^less$","^lightly$","^likely$","^literally$",
      "^loudly$","^luckily$","^mainly$","^maybe$","^meanwhile$","^merely$","^more$","^moreover$","^most$","^mostly$","^much$",
      "^namely$","^naturally$","^nearly$","^necessarily$","^nervously$","^never$","^nevertheless$","^no$","^nonetheless$",
      "^normally$","^not$","^notwithstanding$","^obviously$","^occasionally$","^often$","^once$","^only$","^originally$",
      "^otherwise$","^overall$","^particularly$","^passionately$","^perfectly$","^perhaps$","^personally$","^physically$",
      "^please$","^possibly$","^potentially$","^practically$","^presently$","^previously$","^primarily$","^probability$",
      "^probably$","^profoundly$","^prolly$","^properly$","^quickly$","^quietly$","^quite$","^randomly$","^rarely$","^rather$",
      "^readily$","^really$","^recently$","^regularly$","^relatively$","^respectively$","^right$","^roughly$","^sadly$",
      "^seldomly$","^seriously$","^shortly$","^significantly$","^similarly$","^simply$","^slightly$","^slowly$","^so$",
      "^some$","^somehow$","^sometimes$","^somewhat$","^somewhere$","^soon$","^specifically$","^still$","^strongly$",
      "^subsequently$","^successfully$","^such$","^suddenly$","^supposedly$","^surely$","^surprisingly$","^technically$",
      "^terribly$","^thence$","^thereafter$","^therefor$","^therefore$","^thither$","^thoroughly$","^thus$","^thusfar$",
      "^thusly$","^together$","^too$","^totally$","^truly$","^typically$","^ultimately$","^uncommonly$","^unfortunately$",
      "^unfortunatly$","^usually$","^vastly$","^very$","^virtually$","^well$","^whence$","^where","^wherefor","^whither$",
      "^wholly$","^why$","^why'","^whyd$","^whys$","^widely$","^wither$","^yet$"),
    conj=c("^also$","^altho$","^although$","^and$","^b/c$","^bc$","^because$","^besides$","^both$","^but$","^'cause$","^cos$",
      "^cuz$","^either$","^else$","^except$","^for$","^how$","^how'","^howd$","^howll$","^hows$","^if$","^neither$","^nor$",
      "^or$","^than$","^tho$","^though$","^unless$","^unlike$","^versus$","^vs$","^when$","^when'","^whenever$","^whereas$",
      "^whether$","^while$","^whilst$"),
    prep=c("^about$","^above$","^abt$","^across$","^acrost$","^afk$","^after$","^against$","^along$","^amid","^among",
      "^around$","^as$","^at$","^atop$","^before$","^behind$","^beneath$","^beside$","^betwe","^beyond$","^by$","^despite$",
      "^down$","^during$","^excluding$","^from$","^here$","^here'","^heres$","^in$","^including$","^inside$","^into$",
      "^minus$","^near$","^now$","^of$","^off$","^on$","^onto$","^out$","^outside$","^over$","^plus$","^regarding$","^sans$",
      "^since$","^then$","^there$","^there'","^thered$","^therell$","^theres$","^through$","^throughout$","^thru$","^til$",
      "^till$","^to$","^toward","^under$","^underneath$","^until$","^untill$","^unto$","^up$","^upon$","^via$","^with$",
      "^within$","^without$","^worth$"),
    auxverb=c("^am$","^are$","^arent$","^aren't$","^be$","^been$","^bein$","^being$","^brb$","^can$","^could$","^could'",
      "^couldnt$","^couldn't$","^couldve$","^did$","^didnt$","^didn't$","^do$","^does$","^doesnt$","^doesn't$","^doing$",
      "^dont$","^don't$","^had$","^hadnt$","^hadn't$","^has$","^hasnt$","^hasn't$","^have$","^havent$","^haven't$","^having$",
      "^is$","^isnt$","^isn't$","^may$","^might$","^might'","^mightnt$","^mightn't$","^mightve$","^must$","^mustnt$",
      "^mustn't$","^mustve$","^ought","^shant$","^shan't$","^sha'nt$","^shall$","^should$","^shouldnt$","^shouldn't$",
      "^shouldve$","^was$","^wasnt$","^wasn't$","^were$","^werent$","^weren't$","^will$","^would$","^would'","^wouldnt",
      "^wouldn't","^wouldve$"),
    negate=c("^ain't$","^aint$","^aren't$","^arent$","^can't$","^cannot$","^cant$","^couldn't$","^couldnt$","^didn't$",
      "^didnt$","^doesn't$","^doesnt$","^don't$","^dont$","^hadn't$","^hadnt$","^hasn't$","^hasnt$","^haven't$","^havent$",
      "^idk$","^isn't$","^isnt$","^must'nt$","^mustn't$","^mustnt$","^nah","^need'nt$","^needn't$","^neednt$","^negat",
      "^neither$","^never$","^no$","^nobod","^noes$","^none$","^nope$","^nor$","^not$","^nothing$","^nowhere$","^np$",
      "^ought'nt$","^oughtn't$","^oughtnt$","^shant$","^shan't$","^sha'nt$","^should'nt$","^shouldn't$","^shouldnt$","^uh-uh$",
      "^wasn't$","^wasnt$","^weren't$","^werent$","^without$","^won't$","^wont$","^wouldn't$","^wouldnt$"),
    quant=c("^add$","^added$","^adding$","^adds$","^all$","^allot$","^alot$","^amount$","^amounts$","^another$","^any$",
      "^approximat","^average$","^bit$","^bits$","^both$","^bunch$","^chapter$","^couple$","^doubl","^each$","^either$",
      "^entire","^equal","^every$","^extra$","^few$","^fewer$","^fewest$","^group","^inequal","^least$","^less$","^lot$",
      "^lotof$","^lots$","^lotsa$","^lotta$","^majority$","^many$","^mo$","^mo'","^more$","^most$","^much$","^mucho$",
      "^multiple$","^nada$","^none$","^part$","^partly$","^percent","^piece$","^pieces$","^plenty$","^remaining$","^sampl",
      "^scarce$","^scarcer$","^scarcest$","^section$","^segment","^series$","^several","^single$","^singles$","^singly$",
      "^some$","^somewhat$","^ton$","^tons$","^total$","^triple","^tripling$","^variety$","^various$","^whole$"),
    interrog=c("^how$","^how'd$","^how're$","^how's$","^howd$","^howre$","^hows$","^wat$","^wattt","^what$","^what'd$",
      "^what'll$","^what're$","^what's$","^whatd$","^whatever$","^whatll$","^whatre$","^whatt","^when$","^when'","^whence$",
      "^whenever$","^where$","^where'd$","^where's$","^wherefore$","^wherever$","^whether$","^which$","^whichever$",
      "^whither$","^who$","^who'd$","^who'll$","^who's$","^whoever$","^wholl$","^whom$","^whomever$","^whos$","^whose$",
      "^whosever$","^whoso","^why$","^why'","^whyever$","^wut$"),
    number=c("^billion","^doubl","^dozen","^eight","^eleven$","^fift","^first$","^firstly$","^firsts$","^five$","^four",
      "^half$","^hundred","^infinit","^million","^nine","^once$","^one$","^quarter","^second$","^seven","^single$","^six",
      "^ten$","^tenth$","^third$","^thirt","^thousand","^three$","^trillion","^twel","^twent","^twice$","^two$","^zero$",
      "^zillion"),
    interjection=c("^a+h+$","^a+w+$","^allas$","^alright","^anyhoo$","^anyway[ysz]","^bl[eh]+$","^g+[eah]+$","^h[ah]+$",
      "^h[hu]+$","^h[mh]+$","^l[ol]+$","^m[hm]+$","^meh$","^o+h+$","^o+k+$","^okie","^oo+f+$","^soo+$","^u[uh]+$","^u+g+h+$",
      "^w[ow]+$","^wee+ll+$","^y[aes]+$","^ya+h+$","^yeah$","^yus+$"),
    special=list(
      ELLIPSIS='\\.{3,}|\\. +\\. +[. ]+',
      SMILE='[([{q][ <-]+[;:8=]|[([{q][;:8=]|[;:8=][ >-][p3)}D]|[:;8=][ >-]\\]|[:;8=][p3)}D]|[:;8=]\\]',
      FROWN='\\][ <.,-]+[;:8=]|[)}D/\\>][ <.,-]+[;:8=]|\\][:;8=]|[)}D/\\>][;:8=]|[;:8=][ >.,-][([{/\\<]|[:;8=][([{/\\<]',
      LIKE=c(
        '(?<=could not) like[ .,?!:;/"\']','(?<=did not) like[ .,?!:;/"\']','(?<=did) like[ .,?!:;/"\']',
        '(?<=didn\'t) like[ .,?!:;/"\']','(?<=do not) like[ .,?!:;/"\']','(?<=do) like[ .,?!:;/"\']',
        '(?<=does not) like[ .,?!:;/"\']','(?<=does) like[ .,?!:;/"\']','(?<=doesn\'t) like[ .,?!:;/"\']',
        '(?<=don\'t) like[ .,?!:;/"\']','(?<=i) like[^ /-]*','(?<=should not) like[ .,?!:;/"\']',
        '(?<=they) like[^ /-]*','(?<=we) like[^ /-]*','(?<=will not) like[ .,?!:;/"\']','(?<=will) like[ .,?!:;/"\']',
        '(?<=won\'t) like[ .,?!:;/"\']','(?<=would not) like[ .,?!:;/"\']','(?<=you) like[^ /-]*'
      )
    )
  )
  if(length(cats)==0) cats=names(dict)[-length(dict)]
  if(length(cats)==1 && grepl('\\(|\\[',cats)) cats=eval(parse(text=cats))
  if(any(grepl('[0-9]|seq',cats))) cats=if(length(cats)>1) as.numeric(cats) else eval(parse(text=cats))
  if(is.numeric(cats)){
    cats=cats[cats<length(dict)]
  }else if(any(!cats%in%names(dict))) cats=grep(paste(paste0('^',cats),collapse='|'),names(dict),value=TRUE)
  if(length(cats)==0) stop(
    '\n  enter numbers between 1 and ',length(dict)-1,
    ', or letters matching a category:\n  ',paste(names(dict),collapse=', ')
  )
  if('special'%in%names(cats)) as.regex=TRUE
  if(as.regex){
    if(!missing(as.function)){
      dict = paste(unlist(dict[cats]), collapse = '|')
      fun = if(is.function(as.function)) as.function else grepl
      function(terms, ...){
        args = list(...)
        args$pattern = dict
        args$x = terms
        if(!is.function(as.function) && !'perl' %in% names(args)) args$perl = TRUE
        do.call(fun, args)
      }
    }else dict[cats]
  }else lapply(dict[cats],function(l)gsub('\\^|\\$','',sub('(?<=[^$])$','*',l,perl=TRUE)))
}
