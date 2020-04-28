#' Read/write LIWC dictionary files
#'
#' Read in or write Linguistic Inquiry and Word Count dictionary (.dic) files.
#' @param path Path to a .dic file.
#' @param cats A character vector of category names to be returned. All categories are returned by default.
#' @param to.regex Logical; if \code{TRUE}, each dictionary entry is converted to regular expression
#'   (starts are marked with ^ and ends with $ when an * is present, and unmatched brackets/parens are escaped).
#' @export

read.dic=function(path,cats,to.regex=FALSE){
  di=tryCatch(
    readLines(if(missing(path))file.choose() else path,warn=FALSE)
    ,error=function(e)stop('failed to read path: ',e$message,call.=FALSE)
  )
  lst=grep('%',di)
  if(length(lst)>1){
    di=di[-seq_len(lst[1])]
    lst=lst[2]-2
  }else stop('file is not in the expected format')
  ci=lapply(di[seq_len(lst)],function(l)strsplit(l,'[ \t]+')[[1]])
  names(ci)=vapply(ci,'[[','',2)
  if(missing(cats)) cats=names(ci)
  ci=lapply(ci[names(ci)%in%cats],'[[',1)
  di=strsplit(di[seq_along(di)[-(1:3)]],'[ \t]+(?=[0-9]|$)',perl=TRUE)
  di=di[vapply(di,length,0)>1]
  names(di)=vapply(di,'[','',1)
  di=lapply(di,'[',-1)
  wl=list()
  for(w in names(di)){
    ck=ci%in%di[[w]]
    if(any(ck)){
      cm=names(ci[ck])
      for(c in cm) wl[[c]]=c(wl[[c]],w)
    }
  }
  if(to.regex) lapply(wl, function(l){
    if(any(ck <- grepl('[[({]', l) + grepl('[})]|\\]', l) == 1))
      l[ck] = gsub('([([{}\\])])', '\\\\\\1', l[ck], perl = TRUE)
    gsub('\\^\\*|\\*\\$', '', paste0('^', l, '$'))
  }) else wl
}

#' @rdname read.dic
#' @param x A list object with names of categories and a vector of their words.
#' @param filename The name of the file to be saved.
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
#' moral_dict = read.dic('http://bit.ly/moral_foundations')
#' lusi_dict = read.dic('http://bit.ly/lusi_dict')
#' }
#' @export

write.dic=function(x,filename='custom'){
  filename=paste0(if(!grepl(':',filename,fixed=TRUE))paste0(getwd(),'/'),filename,'.dic')
  fl=unique(as.character(unlist(x)))
  lx=length(x)
  m=matrix('',length(fl)+lx+2,lx+1)
  m[,1]=c('%',seq_len(lx),'%',fl)
  m[seq_len(lx)+1,2]=if(is.null(names(x))) seq_len(lx) else names(x)
  for(l in seq_along(x)) m[which(m[-seq_len(lx+2),1]%in%x[[l]])+lx+2,l+1]=l
  write(paste0(sub('\t+$','',apply(m,1,function(r)paste(r,collapse='\t'))),collapse='\n'),filename)
  message('dictionary saved to ',filename)
}

#' Process texts in a folder
#'
#' Read in and optionally segment all texts within a folder.
#'
#' @param path Path to a folder containing files, or a vector of paths to files.
#' @param segment Specifies how the text of each file should be segmented. If a number, texts will be broken into
#'   that many segments, each with a roughly equal number of words. If a character, texts will be broken at that character;
#'   for example, a string matching \code{join} will split texts on returns.
#' @param subdir If \code{TRUE} files in folders in \code{path} will also be included.
#' @param ext The extension of the files you want to read in. '.txt' by default.
#' @param fixed If \code{FALSE}, and \code{segment} is a character, \code{segment} will be treated as a regular expression.
#' @param segment.size If specified, \code{segment} will be ignored, and texts will be broken into segments containing roughly
#'   \code{segment.size} number of words.
#' @param bysentence If \code{TRUE}, and \code{segment} is a number or \code{wordcount} is specified, sentences will be kept
#'   together, rather than being broken across segments.
#' @param reader The function used to read files. File paths are always passed as the first argument.
#'   \code{\link[base]{readLines}} by default.
#' @param readarg A list of additional arguments to pass to \code{reader}, starting in the second position.
#' @param ncores Number of CPU cores to use; defaults to total number of cores minus 2.
#'
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar% registerDoSEQ

read.folder=function(path=NULL,segment=NULL,subdir=FALSE,ext='.txt',fixed=TRUE,
  segment.size=NULL,bysentence=FALSE,reader=readLines,readarg=list(warn=FALSE),ncores = parallel::detectCores() - 2){
  if(missing(path)) stop("path must be specified; enter the path to a folder, e.g., read.folder('~/texts')")
  fs=if(length(path)==1 && dir.exists(path)) fs=list.files(path,ext,recursive=subdir,full.names=TRUE) else path
  fs=data.frame(rbind(fs,gsub('^.*[\\/]+','',fs)),stringsAsFactors=FALSE)
  if(!missing(segment.size)) segment=NULL
  if(ncores > 1 && (!missing(ncores) || length(fs) > 4)){
    cl = makeCluster(ncores)
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
  }else registerDoSEQ()
  i = 0
  d = foreach(i = seq_along(fs), .combine = rbind) %dopar%{
    f = fs[[i]]
    txt=tryCatch(do.call(reader,c(f[1],readarg)),error=function(e)NULL)
    if(!is.null(txt)){
      txt=paste(txt,collapse='\r\n')
      if(is.character(segment)){
        txt=strsplit(txt,segment,fixed=fixed)[[1]]
      }else if(is.numeric(segment) || !is.null(segment.size)){
        txt=if(bysentence){
          txt=gsub('(?<=st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof)\\.','__PERIOD__',txt,TRUE,TRUE)
          txt=strsplit(gsub('(?<=[.?!][ ")}\\]])','__BREAK__',txt,perl=TRUE),'__BREAK__',fixed=TRUE)[[1]]
          lapply(txt,function(t)strsplit(sub('__PERIOD__','.',t,fixed=TRUE),' [^A-Za-z0-9]+ | +|\r\n',perl=TRUE)[[1]])
        }else strsplit(txt,' [^A-Za-z0-9]+ | +|\r\n',perl=TRUE)[[1]]
        ns=length(txt)
        sls=vapply(txt,function(s)sum(s!=''),0)
        wc=sum(sls)
        seg=if(is.null(segment.size)) round(wc/segment+.49) else segment.size
        txt = vapply(
          unname(split(txt, cut(cumsum(sls), c(-Inf, seq_len(round(wc / seg + .49) - 1) * seg, Inf)))),
          paste, '', collapse = ' '
        )
        txt = txt[grepl('\\w', txt)]
      }
      data.frame(file = f[2], segment = seq_along(txt), text = txt)
    }
  }
  rownames(d) = seq_len(nrow(d))
  d
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
  dir = sub('/+$', '', dir)
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
#' @importFrom utils download.file
#' @importFrom tools md5sum

download.lsspace = function(space = '100k', include.terms = TRUE, decompress = TRUE,
  check.md5 = TRUE, mode = 'wb', dir = getOption('lingmatch.lspace.dir')){
  dir = sub('/+$', '', dir)
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
  ip = paste0(sub('/+$', '', dir), '/', infile)
  op = paste0(sub('/+$', '', outdir), '/', name)
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

#' Categorize raw texts using a pattern-based dictionary
#'
#' @param text A vector of raw text to be categorized.
#' @param dict At least a vector of terms (patterns), usually a matrix-like object with columns for terms,
#'   categories, and weights.
#' @param term,category,weight Strings specifying the relevant column names in \code{dict}.
#' @param to.lower Logical indicating whether \code{text} should be converted to lower case.
#' @param to.percent Logical indicating whether term-counts should be divided by document-counts before
#'   being weighted (defaults to \code{FALSE}).
#' @param bias A constant to add to each category after weighting and summing. Can be a vector with names
#'   corresponding to the unique values in \code{dict[,category]}, but is usually extracted from dict based
#'   on the intercept included in each category (defined by \code{intname}).
#' @param intname The term representing the intercept (bias) of a category, to be extracted from \code{dict}
#'   and used as \code{bias}.
#' @param return_dtm Logical; if \code{TRUE}, only a document-term matrix will be returned, rather than the
#'   weighted, summed, and adjusted category value.
#' @param exclusive Logical; if \code{FALSE}, each dictionary term is searched for in the original text.
#'   Otherwise (by default), terms are sorted by length (with longer terms being searched for first), and
#'   matches are removed from the text (avoiding subsequent matches to matched patterns).
#' @param boundary A string to add to the beginning and end of each dictionary term. If \code{TRUE},
#'   \code{boundary} will be set to \code{' '}, avoiding pattern matches within words. By default, dictionary
#'   terms are left as entered.
#' @param fixed Logical; if \code{FALSE}, patterns can be regular expressions.
#' @param perl Logical; passed to \code{\link{strsplit}}. This is set to \code{FALSE} if \code{fixed} is
#'   \code{TRUE}.
#' @param ncores Number of CPU cores to use. Default is number of cores - 2 if \code{text} has more than 100
#'   entries. Otherwise, if not specified, only 1 core will be used.
#' @seealso For applying term-based dictionaries (to a document-term matrix) see \code{\link{lma_termcat}}.
#' @examples
#' \dontrun{
#'
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
#' # read in the temporal orientation lexicon from the World Well-Being Project
#' tempori = read.csv('https://wwbp.org/downloads/public_data/temporalOrientationLexicon.csv')
#'
#' lma_patcat(text, tempori)
#' }
#' @export

lma_patcat = function(text, dict, term = 'term', category = 'category', weight = 'weight',
  to.lower = TRUE, to.percent = FALSE, bias = NULL, intname = '_intercept', return_dtm = FALSE,
  exclusive = TRUE, boundary = NULL, fixed = TRUE, perl = TRUE, ncores = detectCores() - 2){
  text = paste(' ', text, ' ')
  if(to.lower) text = tolower(text)
  if(is.null(colnames(dict))){
    if(is.list(dict)){
      if(is.null(names(dict))) names(dict) = seq_along(dict)
      dict = lapply(dict, as.character)
      dict = data.frame(
        term = unlist(dict, use.names = FALSE),
        category = unlist(lapply(names(dict), function(n) rep(n, length(dict[[n]]))))
      )
    }else dict = data.frame(term = dict)
    term = 'term'
    category = 'category'
  }
  if(!weight %in% names(dict)) dict[, weight] = 1
  if((missing(bias) || (is.logical(bias) && bias)) && any(bs <- !is.na(dict[, term]) & dict[, term] == intname)){
    bias = dict[bs,, drop = FALSE]
    bias = if(sum(bs) != 1 && category %in% names(bias)){
      rownames(bias) = bias[, category]
      t(bias[, weight, drop = FALSE])[1, ]
    }else bias[1, weight]
    dict = dict[!bs, ]
  }
  terms = na.omit(as.character(unique(dict[, term])))
  mfun = if(exclusive){
    terms = terms[order(-nchar(terms))]
    function(w){
      if(txt == '') 0 else{
        tt = strsplit(txt, w, fixed = fixed, perl = perl)[[1]]
        txt <<- paste(tt, collapse = ' ')
        length(tt) - 1
      }
    }
  }else function(w) length(strsplit(txt, w, fixed = fixed, perl = perl)[[1]]) - 1
  if(is.logical(boundary)) boundary = if(boundary) ' ' else NULL
  if(!is.null(boundary)){
    oterms = terms
    terms = paste0(boundary, terms, boundary)
  }
  if(fixed) perl = FALSE
  l = length(text)
  if(ncores > 1 && (!missing(ncores) || l > 100)){
    clust = makeCluster(ncores)
    registerDoParallel(clust)
    on.exit(stopCluster(clust))
  }else registerDoSEQ()
  txt = ''
  dtm = foreach(txt = text, .combine = cbind) %dopar% Matrix(vapply(terms, mfun, 0), sparse = TRUE)
  if(!is.null(boundary)) terms = oterms
  dimnames(dtm) = list(terms, seq_len(l))
  if(to.percent){
    rs = colSums(dtm)
    if(any(rs != 0)){
      su = rs > 0
      dtm[, su] = t(t(dtm[, su]) / rs[su]) * 100
    }
  }
  if(return_dtm) return(t(dtm))
  if(category %in% names(dict)){
    cats = unique(dict[, category])
    terms = split(dict[, c(term, weight)], dict[, category])[cats]
    if(!is.null(bias) && is.null(names(bias))){
      bias = rep_len(bias, length(cats))
      names(bias) = cats
    }
    om = matrix(0, l, length(cats), dimnames = list(NULL, cats))
    for(cat in cats){
      ct = na.omit(terms[[cat]])
      if(nrow(ct)) om[, cat] = colSums(dtm[as.character(ct[, 1]),, drop = FALSE] * ct[, 2]) +
        if(!is.null(bias) && cat %in% names(bias)) bias[cat] else 0
    }
  }else om = rowSums(dtm * dict[, weight]) + if(!is.null(bias)) bias else 0
  om
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
      "^automatically$","^away$","^awhile$","^back$","^badly$","^barely$","^basically$","^below$","^briefly$","^carefully$",
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
