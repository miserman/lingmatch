.onLoad = function(lib, pkg){
  if(is.null(getOption('lingmatch.lspace.dir'))) options(lingmatch.lspace.dir = path.expand('~/Latent Semantic Spaces'))
  if(is.null(getOption('lingmatch.dict.dir'))) options(lingmatch.dict.dir = path.expand('~/Dictionaries'))
}

#' Linguistic Matching and Accommodation
#'
#' Offers a variety of methods to assess linguistic matching or accommodation, where \emph{matching}
#' is general similarity (sometimes called \emph{homophily}), and \emph{accommodation} is some form
#' of conditional similarity (accounting for some base-rate or precedent; sometimes called
#' \emph{alignment}).
#'
#' There are a great many points of decision in the assessment of linguistic similarity and/or
#' accommodation, partly inherited from the great many point of decision inherent in the numerical
#' representation of language. Two general types of matching are implemented here as sets of
#' defaults: Language/Linguistic Style Matching (LSM; Niederhoffer & Pennebaker, 2002; Ireland &
#' Pennebaker, 2010), and Latent Semantic Analysis/Similarity (LSA; Landauer & Dumais, 1997;
#' Babcock, Ta, & Ickes, 2014). See the \code{type} argument for specifics.
#'
#' @param input Texts to be compared; a vector, document-term matrix (dtm; with terms as column names),
#'   or path to a file (.txt or .csv, with texts separated by one or more lines/rows).
#' @param comp Defines the comparison to be made:
#' \itemize{
#'   \item If a function, this will be applied to \code{input} within each group (overall if there is
#'     no group; i.e., \code{apply(input, 2, comp)}; e.g., \code{comp = mean} would compare each text to
#'     the mean profile of its group.)
#'   \item If a character with a length of 1 and no spaces, if it partially matches one of
#'     \code{lsm_profiles}'s rownames, that row will be used as the comparison; if it partially
#'     matches \code{'auto'}, the highest correlating \code{lsm_profiles} row will be used; if it
#'     partially matches \code{'pairwise'}, each text will be compared to one another; if it
#'     partially matches \code{'sequential'}, the last variable in \code{group} will be treated as
#'     a speaker ID (see the grouping and comparisons section).
#'   \item If a character vector, this will be processed in the same way as \code{input}.
#'   \item If a vector, either of the same length as \code{input} has rows and logical or factor-like
#'     (having  n levels < length), or a numeric range or logical of length less than \code{nrow(input)}
#'     , this will be used to select a subset of
#'     \code{input} (e.g., \code{comp = 1:10} would treat the first 10 rows of \code{input} as the comparison;
#'     \code{comp = type=='prompt'} would make a logical vector identifying prompts, assuming
#'     "type" was the name of a column in \code{data}, or a variable in the global environment,
#'     and the value "prompt" marked the prompts).
#'   \item If a matrix-like object (having multiple rows and columns), or a named vector, this will
#'     be treated as a sort of dtm, assuming there are common (column) names between \code{input} and
#'     \code{comp} (e.g., if you had prompt and response texts that were already processed separately).
#' }
#' @param data A matrix-like object as a reference for column names, if variables are refereed to in
#'   other arguments (e.g., \code{lingmatch(text, data=data)} would be the same as
#'   \code{lingmatch(data$text)}.
#' @param group A logical or factor-like vector the same length as \code{NROW(input)}, used to defined
#'   groups.
#' @param ... Passes arguments to \code{\link{lma_dtm}}, \code{\link{lma_weight}},
#'   \code{\link{lma_termcat}}, and/or \code{\link{lma_lspace}},
#'   depending on \code{input} and \code{comp}, and \code{\link{lma_simets}}.
#' @param comp.data A matrix-like object as a reference to \code{comp} variables.
#' @param comp.group The column name of the grouping variable(s) in \code{comp.data}; if
#'   \code{group} contains references to column names, and \code{comp.group} is not specified,
#'   \code{group} variables will be looked for in \code{comp.data}.
#' @param order A numeric vector the same length as \code{nrow(input)} indicating the order of the
#'   texts and grouping variables if the type of comparison is sequential. Only necessary if the
#'   texts are not already ordered as desired.
#' @param drop logical; if \code{FALSE}, columns with a sum of 0 are retained.
#' @param all.levels logical; if \code{FALSE}, multiple groups are combined. See the Grouping and
#'   Comparisons section.
#' @param type A character at least partially matching 'lsm' or 'lsa'; applies default settings
#'   aligning with the standard calculations of each type:
#'   \tabular{ll}{
#'     LSM \tab \code{lingmatch(text, weight='freq', dict=lma_dict(1:9), metric='canberra')}\cr
#'     LSA \tab \code{lingmatch(text, weight='tfidf', space='100k_lsa', metric='cosine')}\cr
#'   }
#' @section Grouping and Comparisons:
#' Defining groups and comparisons can sometimes be a bit complicated, and requires dataset
#' specific knowledge, so it can't always (readily) be done automatically. Variables entered in the
#' \code{group} argument are treated differently depending on their position and other arguments:
#'
#' \describe{
#'   \item{Splitting}{By default, groups are treated as if they define separate chunks of data in
#'     which comparisons should be calculated. Functions used to calculated comparisons, and
#'     pairwise comparisons are performed separately in each of these groups. For example, if you
#'     wanted to compare each text with the mean of all texts in its condition, a \code{group}
#'     variable could identify and split by condition. Given multiple grouping variables,
#'     calculations will either be done in each split (if \code{all.levels = TRUE}; applied in
#'     sequence so that groups become smaller and smaller), or once after all splits are made (if
#'     \code{all.levels = FALSE}). This makes for 'one to many' comparisons with either calculated
#'     or preexisting standards (i.e., the profile of the current data, or a precalculated profile,
#'     respectively).}
#'   \item{Comparison ID}{When comparison data is identified in \code{comp}, groups are assumed
#'     to apply to both \code{input} and \code{comp} (either both in \code{data}, or separately
#'     between \code{data} and \code{comp.data}, in which case \code{comp.group} may be needed if
#'     the same grouping variable have different names between \code{data} and \code{comp.data}).
#'     In this case, multiple grouping variables are combined into a single factor assumed to
#'     uniquely identify a comparison. This makes for 'one to many' comparisons with specific texts
#'     (as in the case of manipulated prompts or text-based conditions).}
#'   \item{Speaker ID}{If \code{comp} matches \code{'sequential'}, the last grouping variable
#'     entered is assumed to identify something like speakers (i.e., a factor with two or more
#'     levels and multiple observations per level). In this case the data is assumed to be ordered
#'     (or ordered once sorted by \code{order} if specified). Any additional grouping variables
#'     before the last are treated as splitting groups. This can set up for probabilistic
#'     accommodation metrics. At the moment, when sequential comparisons are made within groups,
#'     similarity scores between speakers are averaged, resulting in mean matching between speakers
#'     within the group.}
#' }
#' @references
#' Babcock, M. J., Ta, V. P., & Ickes, W. (2014). Latent semantic similarity and language style
#'   matching in initial dyadic interactions. \emph{Journal of Language and Social Psychology, 33},
#'   78-88.
#'
#' Ireland, M. E., & Pennebaker, J. W. (2010). Language style matching in writing: synchrony in
#'   essays, correspondence, and poetry. \emph{Journal of Personality and Social Psychology, 99},
#'   549.
#'
#' Landauer, T. K., & Dumais, S. T. (1997). A solution to Plato's problem: The latent semantic
#'   analysis theory of acquisition, induction, and representation of knowledge.
#'   \emph{Psychological Review, 104}, 211.
#'
#' Niederhoffer, K. G., & Pennebaker, J. W. (2002). Linguistic style matching in social interaction.
#'   \emph{Journal of Language and Social Psychology, 21}, 337-360.
#' @seealso For a general text processing function, see \code{\link{lma_process}}.
#' @examples
#' # compare single strings
#' lingmatch('Compare this sentence.', 'With this other sentence.')
#'
#' # compare each entry in a character vector with...
#' texts = c(
#'   'One bit of text as an entry...',
#'   'Maybe multiple sentences in an entry. Maybe essays or posts or a book.',
#'   'Could be lines or a column from a read-in file...'
#' )
#'
#' ## one another
#' lingmatch(texts)
#'
#' ## the first
#' lingmatch(texts, 1)
#'
#' ## the next
#' lingmatch(texts, 'seq')
#'
#' ## the set average
#' lingmatch(texts, mean)
#'
#' ## other entries in a group
#' lingmatch(texts, group = c('a', 'a', 'b'))
#'
#' ## one another, without stop words
#' lingmatch(texts, exclude = 'function')
#'
#' ## a standard average (based on function words)
#' lingmatch(texts, 'auto', dict = lma_dict(1:9))
#'
#' @export
#' @import methods Matrix
#' @importFrom stats na.omit dpois ppois
#' @importFrom Rcpp sourceCpp
#' @importFrom RcppParallel RcppParallelLibs
#' @useDynLib lingmatch, .registration = TRUE

lingmatch=function(input=NULL,comp=mean,data=NULL,group=NULL,...,comp.data=NULL,comp.group=NULL,order=NULL,
  drop=TRUE,all.levels=FALSE,type='lsm'){
  inp = as.list(substitute(...()))
  #setting up a default type if specified
  if(!missing(type) && !is.null(type)){
    type = if(grepl('lsm|lang|ling|style|match', type, TRUE)) 'lsm' else 'lsa'
    ni = names(inp)
    if(type == 'lsm' && !'dict' %in% ni) inp$dict = lma_dict(1:9)
    if(type != 'lsm' && !'space' %in% ni) inp$space = '100k_lsa'
    if(!'metric' %in% ni) inp$metric = if(type == 'lsm') 'canberra' else 'cosine'
    if(is.null(attr(input, 'type')) || length(attr(input, 'type')) == 1){
      if(type == 'lsm' && !'percent' %in% ni) inp$percent = TRUE
      if(type != 'lsm' && !'weight' %in% ni) inp$weight = 'tfidf'
    }
  }
  mets = c('jaccard', 'euclidean', 'canberra', 'cosine', 'pearson')
  inp$metric = if(!is.null(inp$metric)) match_metric(inp$metric)$selected else 'cosine'
  vs=c('input','comp','group','order','data','comp.data','comp.group')
  opt=as.list(match.call(expand.dots=FALSE))[vs]
  names(opt)=vs
  # organizing options for preprocessing
  dsp=lapply(c('lma_dtm','lma_weight','lma_lspace','lma_termcat','lma_simets'),function(f){
    a=names(as.list(args(f)))
    a = a[-c(1, length(a))]
    inp[a[a%in%names(inp)]]
  })
  names(dsp)=c('p','w','m','c','s')
  # fetches input from data or environment
  gv = function(a, data = NULL){
    ta = a
    if(is.character(ta)){
      if(!is.null(data) && ta %in% colnames(data)) return(unlist(data[, ta])) else
        if(length(ta) == 1 || !any(grepl(' ', ta, fixed = TRUE))) ta = parse(text = a)
    }
    ta = tryCatch(eval(ta, data, parent.frame(2)), error = function(e) NULL)
    if(length(ta) == 0 || (!is.null(dim(ta)) && dim(ta)[1] == 0))
      ta = tryCatch(eval(a, globalenv()), error = function(e) NULL)
    if(is.null(ta)) ta = tryCatch(eval(a, data), error = function(e) NULL)
    if(is.null(ta)){
      p = 2
      while(is.null(ta) && p < 99){
        p = p + 1
        ta = tryCatch(eval(a, parent.frame(p)), error = function(e) NULL)
      }
    }
    if(is.null(ta)) stop('could not find ', deparse(a), call. = FALSE)
    ta
  }
  gd = function(a, data = NULL){
    r = if(is.character(a) && length(a) == 1 && grepl('\\.(?:csv|txt|tsv|tab)$', a, TRUE)){
      if(file.exists(a)){
        r = if(grepl('txt$', a)) readLines(a, warn = FALSE) else{
          r = read.table(a, TRUE, if(grepl('csv$', a)) ',' else '\t', '"', comment.char = '')
          r[, which(!vapply(r, is.numeric, TRUE))[1]]
        }
        r[r != '']
      }else stop(a, ' does not exist', call. = FALSE)
    }else if(is.character(a)) a else gv(a, data)
    if(is.factor(r)) r = as.character(r)
    if(is.character(r) && length(r) == 1 && grepl('\\.(?:csv|txt|tsv|tab)$', r, TRUE)) r = gd(r)
    r
  }
  # weight, categorize, and/or map
  wmc = function(a){
    if(!is.null(colnames(a)) || (length(dsp$c) == 0 && length(dsp$m) == 0)){
      if(length(dsp$w) != 0) a = do.call(lma_weight, c(list(a), lapply(dsp$w, eval, parent.frame(2))))
      if(length(dsp$c) != 0) a = do.call(lma_termcat, c(list(a), lapply(dsp$c, eval, parent.frame(2))))
      if(length(dsp$m) != 0) a = do.call(lma_lspace, c(list(a), lapply(dsp$m, eval, parent.frame(2))))
    }
    a
  }
  # initial data parsing
  # input
  if(missing(input)) input = file.choose()
  if(is.function(input)) stop('enter a character vector or matrix-like object as input')
  if(missing(data)) data = input
  input = if(!missing(data) && is.character(input) && all(input %in% colnames(data))) data[, input] else
    gd(opt$input, data)
  if(missing(data) && !missing(group) && is.data.frame(input))
    input = as.matrix(input[, vapply(input, is.numeric, TRUE)])
  rx=NROW(input)
  cx=NCOL(input)
  # comp
  if(!missing(comp)){
    comp = gd(opt$comp, if(missing(comp.data)) if(is.call(opt$comp)) NULL else data else comp.data)
    if(is.logical(comp)) comp=which(comp)
    if(missing(comp.data) && !is.null(colnames(comp))) comp.data=comp
  }else if(missing(comp) && missing(group) && missing(comp.data) && missing(comp.group)){
    opt$comp = comp = 'pairwise'
  }else opt$comp='mean'
  if(length(opt$comp) > 1) opt$comp = deparse(opt$comp)
  if(is.factor(input)) input=as.character(input)
  if(is.factor(comp)) comp = as.character(comp) else if(is.data.frame(comp))
    comp = comp[, vapply(comp, is.numeric, TRUE)]
  do.wmc=TRUE
  if('dict' %in% names(inp) && any(class(input) %in% c('matrix', 'data.frame')) &&
      is.null(attr(input, 'Type'))){
    cn = colnames(input)
    dn = gv(inp$dict)
    if(is.list(dn)) dn = names(dn)
    if(any(!(ck <- dn %in% cn))){
      if('prep' %in% dn && !'prep' %in% cn) colnames(input)[cn == 'preps'] = 'prep'
      if('article' %in% dn && !'article' %in% cn) colnames(input)[cn == 'articles'] = 'article'
      ck = dn %in% colnames(input)
    }
    if(all(ck)){
      inp$dict = NULL
      if(missing(data)) data=input
      if(any(!ck)) dn=dn[ck]
      cx = length(dn)
      input=input[,dn]
      do.wmc=FALSE
      if(!missing(comp)){
        if(any(class(comp)%in%c('matrix','data.frame'))){
          if(all(dn%in%colnames(comp))) comp=comp[,dn]
        }else{
          if(is.character(comp) && (length(comp)>1 || grepl(' ',comp,fixed=TRUE)))
            comp=wmc(do.call(lma_dtm,c(list(comp),dsp$p)))
        }
      }
    }
  }
  if(!is.matrix(input) && is.character(input)){
    if(!any(grepl('[^[:digit:][:space:].-]', input))){
      input = as.numeric(input)
    }else{
      # if input looks like text, seeing if other text can be added, then converting to a dtm
      if(is.character(comp) && (length(comp)>1 ||  grepl(' ',comp,fixed=TRUE))){
        input=c(comp,input)
        comp=seq_along(comp)
        opt$comp='text'
      }
      input=do.call(lma_dtm,c(list(input),dsp$p))
    }
  }
  if(is.data.frame(comp)) comp=as.matrix(comp)
  cc=if(is.numeric(comp) && (!is.null(comp.data) || is.null(dim(comp)))) 1 else if(is.character(comp)){
    comp=tolower(comp)
    2
  }else 0
  # group and order
  agc = c('c', 'list', 'cbind', 'data.frame')
  if(!missing(group) && !(is.null(colnames(data)) && rx == length(opt$group) - 1))
    group = if(length(opt$group) > 1 && as.character(opt$group[1]) %in% agc
    && !grepl('[$[]',as.character(opt$group[1]))) lapply(opt$group[-1],gv,data) else{
      if(!is.null(data) && is.character(opt$group) && length(opt$group) < nrow(data)){
        if(!all(opt$group %in% colnames(data)))
          stop('group appears to be column names, but were not found in data')
        group = data[, opt$group]
        if(!is.list(group)) group = if(is.matrix(group)) as.data.frame(group) else list(group)
      }else{
        group=gv(opt$group,data)
        if(is.factor(group)) group=as.character(group) else if(is.matrix(group))
          group = as.data.frame(group,row.names=FALSE)
        if(is.null(ncol(group))) list(group) else lapply(group,as.character)
      }
    }
  if(!missing(comp.group) || (!is.null(comp.data) && !missing(group))){
    cg=opt[[if(missing(comp.group)) 'group' else 'comp.group']]
    if(!is.null(cg)){
      cg=if(!is.null(comp.data) && length(cg)>1
        && as.character(cg[1]) %in% agc && !grepl('[$[]',as.character(cg[1]))){
        lapply(as.character(cg[-1]),gv,comp.data)
      }else if(is.character(cg)){
        if(cg %in% colnames(comp.data)) list(comp.data[, cg]) else
          stop('groups not found in comp.data')
      }else{
        list(gv(cg, comp.data))
      }
      if(is.list(cg) && length(cg) == 1 && !is.null(nrow(cg[[1]]))) cg = as.data.frame(cg[[1]])
      if(cc!=1) if(NROW(comp)!=length(cg[[1]]) || NROW(input)!=length(group[[1]]))
        stop('data and comp.data mismatch',call.=FALSE)
      if(all.levels){
        comp.group = cg
      }else{
        comp.group=do.call(paste,cg)
        if(length(group)>1){
          group=do.call(paste,group)
          if(!is.null(comp.data) && any(ck<-!(ckg<-unique(group))%in%unique(comp.group)))
            if(all(ck)) stop('group and comp.group had no levels in common') else{
              warning('levels not found in comp.group: ',paste(ckg[ck],collapse=', '),call.=FALSE)
              group=group[ck<-group%in%ckg[!ck]]
              input=input[ck,,drop=FALSE]
            }
        }
      }
    }
  }
  if(!missing(group)) if(length(if(is.list(group)) group[[1]] else group)!=rx)
    stop('length(group) != nrow(input)')
  if(!missing(order)){
    order=gv(opt$order,data)
    if(!is.null(order)) if(length(order)==rx){
      input=input[order,]
      group=lapply(group,'[',order)
    }else warning('length(order) != nrow(input), so order was not applied', call. = FALSE) else
      warning('failed to apply order', call. = FALSE)
  }
  if(is.character(input)) input = matrix(as.numeric(input), rx)
  if(is.data.frame(input) && any(ckvc <- !vapply(seq_len(cx), function(col)
    class(input[, col])[1], '') %in% c('numeric', 'integer'))){
    if(all(ckvc)){
      for(col in seq_along(ckvc)) input[, col] = as.numeric(input[, col])
    }else{
      input = input[, !ckvc]
      warning('some input variables were not of numeric or integer class, so they were removed')
    }
  }
  dtm = Matrix(if(is.data.frame(input)) as.matrix(input) else input, sparse = TRUE)
  if(do.wmc) input=wmc(input)
  if(is.null(nrow(input))) input=t(as.matrix(input))
  if(cc==2 && (length(comp)>1 || any(grepl(' ',comp,fixed=TRUE)))){
    comp=do.call(lma_dtm,c(list(comp),dsp$p))
    cc=1
  }
  # if comp appears to be a dtm, unifying input and comp
  if(cc==1 && !is.null(names(comp))) comp=t(as.matrix(comp))
  cr=nrow(comp)
  cn=colnames(comp)
  if(!is.null(cn)){
    cc = 1
    nn = cn[!cn %in% colnames(input)]
    if(length(nn) != 0) input = cbind(input, matrix(0, nrow(input), length(nn), dimnames = list(NULL, nn)))
    input = rbind(matrix(0, cr, ncol(input), dimnames = list(NULL, colnames(input))), input)
    input[seq_len(cr), cn] = comp
    comp = seq_len(cr)
  }
  if(drop){
    if(sum(su <- colSums(input, na.rm = TRUE) != 0) != 0) input = input[, su, drop = FALSE] else
      stop('input is all 0s after processing')
  }
  nc=ncol(input)
  # finalizing comp
  if(cc==1 || opt$comp=='text'){
    comp.data=input[comp,,drop=FALSE]
    if(!missing(comp.group) && !all.levels){
      if(anyDuplicated(comp.group)){
        comp.data = t(vapply(split(as.data.frame(comp.data), comp.group), colMeans,
          numeric(ncol(comp.data))))
        rownames(comp.data) = comp.group = unique(comp.group)
        opt$comp = paste(opt$comp, opt$group, 'group means')
      }else if(nrow(comp.data) == length(comp.group)) rownames(comp.data) = comp.group
    }else if(nrow(comp.data) == 1) comp.data = structure(as.numeric(comp.data[1,]),
      names = colnames(comp.data))
    input=input[-comp,,drop=FALSE]
  }else if(cc==2){
    ckp=FALSE
    if(grepl('^pa|^se',comp)){
      opt$comp=if(grepl('^pa',comp)) 'pairwise' else 'sequential'
    }else if(any(!is.na(p<-pmatch(comp,rownames(lsm_profiles))))){
      opt$comp=rownames(lsm_profiles)[p]
      ckp=TRUE
      comp.data=lsm_profiles[p,,drop=FALSE]
    }else if(grepl('^au',comp)){
      p = colMeans(input, na.rm=TRUE)
      p = which.max(lma_simets(lsm_profiles, p, 'pearson'))
      opt$comp=paste('auto:',names(p))
      ckp=TRUE
      comp.data=lsm_profiles[p,,drop=FALSE]
    }else opt$comp=substitute(comp)
    if(ckp){
      if(any(ckp<-!(cn<-colnames(input))%in%(bn<-colnames(comp.data)))){
        if(all(ckp)) stop('input and comp have no columns in common')
        if('articles' %in% cn && !'articles' %in% bn) bn[bn == 'article'] = 'articles'
        if('preps' %in% cn && !'preps' %in% bn) bn[bn == 'prep'] = 'preps'
        colnames(comp.data)=bn
        if(any(ckp<-!cn%in%bn)){
          warning('input columns were not found in comp: ',paste(cn[ckp],collapse=', '), call. = FALSE)
          comp.data=comp.data[,cn[!ckp],drop=FALSE]
        }
      }else comp.data=comp.data[,cn,drop=FALSE]
    }
  }else if(!is.null(comp.data)){
    cn=colnames(input)
    cns=cn[ck<-cn%in%colnames(comp.data)]
    if(!any(ck)) stop('input and comp have no columns in common') else if(any(!ck)){
      warning('input columns were not found in comp: ',paste(cn[!ck],collapse=', '), call. = FALSE)
      input=input[,cns]
    }
    comp.data=comp.data[,cns,drop=FALSE]
  }
  compmeanck=opt$comp=='mean'
  sim=speaker=NULL
  if(!is.null(group)){
    if(!is.null(comp.data) && NROW(comp.data)==1){
      group=NULL
      warning('group does not appear to be meaningful for this comparison, so it was ignored',
        call. = FALSE)
    }else if(!is.list(group)) group=list(group)
    gl = length(group)
    if(opt$comp == 'sequential'){
      speaker = group[[gl]]
      group = if(gl == 1) NULL else group[-gl]
      gl = length(group)
    }
    if(gl > 1 && !all.levels){
      group = list(do.call(paste, group))
      gl = 1
    }
    if(gl){
      sim = as.data.frame(group)
      colnames(sim) = paste0('g', seq_len(gl))
      for(m in inp$metric) sim[, m] = NA
      mets = seq_along(inp$metric) + gl
    }
    gs=as.character(unique(sim[,1]))
    if(gl == 1 && !is.null(comp.data) && !is.null(comp.group) && !any(gs %in% rownames(comp.data))){
      warning('no group levels were found in comp.data', call. = FALSE)
    }
  }else if(opt$comp == 'sequential' && is.null(speaker)) speaker = seq_len(nrow(input))
  #making comparisons
  sal=dsp$s
  ckf=is.function(comp)
  if(is.null(group)){
    if(!is.null(speaker)) sal$group=speaker
    if(!is.null(comp.data)){
      if(ckf){
        opt$comp=paste(opt$comp.data,opt$comp)
        sal$b=comp.data=if(is.null(nrow(comp.data))) comp.data else
          if(compmeanck) colMeans(comp.data,na.rm=TRUE) else apply(na.omit(comp.data),2,comp)
      }else sal$b=comp.data
    }else if(ckf) sal$b=comp.data=if(compmeanck) colMeans(input,na.rm=TRUE) else
      apply(na.omit(input),2,comp)
    if(!'b' %in% names(sal) && (is.numeric(comp) || !is.null(dim(comp)))) sal$b = comp
    sim=do.call(lma_simets,c(list(input),sal))
  }else{
    cks=!is.null(speaker)
    ckc=!is.null(comp.data)
    ckp=cc==2 && opt$comp=='pairwise'
    ckq=cc==2 && opt$comp=='sequential'
    if(gl==1){
      if(opt$comp!='pairwise'){
        if(opt$comp == 'sequential'){
          group = sim[, 1]
          sim = do.call(rbind, lapply(gs, function(g){
            su = which(group == g)
            s = speaker[su]
            r = if(length(su) < 2 || length(unique(s)) < 2){
              data.frame(group = g, structure(as.list(numeric(length(mets)) + 1),
                names = inp$metric), row.names = paste(su, collapse = ', '))
            }else{
              sal$group = s
              r = do.call(lma_simets, c(list(input[su,, drop = FALSE]), sal))
              rs = as.integer(unlist(strsplit(rownames(r), '[^0-9]+')))
              rownames(r) = strsplit(do.call(sprintf, c(
                paste(gsub('[0-9]+', '%i', rownames(r)), collapse = '|'), as.list(rs - 1 + su[1])
              )), '|', fixed = TRUE)[[1]]
              data.frame(group = g, r)
            }
          }))
        }else{
          ckmc=FALSE
          if(!ckc && ckf){
            ckmc=TRUE
            opt$comp=paste(opt$group,'group',opt$comp)
            comp.data=as.data.frame(matrix(NA,length(gs),nc,dimnames=list(gs,colnames(input))))
          }
          for(g in gs){
            su=sim[,1]==g
            if(ckc){
              if(nrow(cc<-comp.data[if(!is.null(comp.group)) comp.group==g else g,,drop=FALSE])==1)
                sal$b=cc else warning('comp.data has too few/many rows in group ',g, call. = FALSE)
            }else if(ckf) if(sum(su)>1) sal$b=if(compmeanck) colMeans(input[su,],na.rm=TRUE) else
              apply(na.omit(input[su,]),2,comp) else{
                if(ckmc) comp.data[g,] = input[su,]
                sim[su,mets]=1
                next
              }
            if(!is.null(sal$b) && ckmc) comp.data[g,]=sal$b
            if(sum(su)==1 && is.null(sal$b)){
              sim[su,mets]=1
              next
            }
            tm=do.call(lma_simets,c(list(input[su,,drop=FALSE]),sal))
            sim[su, mets] = tm
          }
        }
      }else{
        sal$symmetrical=if('symmetrical'%in%names(dsp$s)) dsp$s$symmetrical else TRUE
        sal$mean=if('mean'%in%names(dsp$s)) dsp$s$mean else TRUE
        if(sal$symmetrical && sal$mean){
          sim = vapply(seq_along(group[[1]]), function(i){
            su = group[[1]] == group[[1]][i]
            su[i] = FALSE
            sal$b = input[i,]
            r = if(sum(su) != 0) do.call(lma_simets, c(list(input[su,]), sal)) else
              numeric(length(sal$metric)) + 1
            if(is.null(nrow(r))) r else if(nrow(r) == 1) as.numeric(r) else
              if(ncol(r) != 1) colMeans(r, TRUE) else mean(r, na.rm = TRUE)
          }, numeric(length(sal$metric)))
          sim = data.frame(group[[1]], if(is.matrix(sim)) t(sim) else sim)
          colnames(sim) = c(opt$group, sal$metric)
        }else{
          sim=lapply(ug<-unique(group[[1]]),function(g){
            su=group[[1]]==g
            if(sum(su)!=1) do.call(lma_simets,c(list(input[su,]),sal)) else
              numeric(length(sal$metric)) + 1
          })
          if(!sal$symmetrical){
            sim=as.data.frame(
              if(sal$mean) ug else rep(ug,vapply(sim,function(gs)length(gs[[1]]),0)),
              do.call(rbind,if(sal$mean) sim else lapply(sim,as.data.frame))
            )
            colnames(sim)=c(opt$group,sal$metric)
          }else names(sim)=ug
        }
      }
    }else if(gl>1){
      for(i in seq_len(gl-1)) sim=cbind(sim,sim[,mets])
      sug=seq_len(gl)
      cn=paste0('g',sug)
      mn=length(inp$metric)
      mw=seq_len(mn)
      colnames(sim)[-sug]=paste0(rep(vapply(seq_along(cn),function(e)
        paste0(cn[seq_len(e)],collapse='_'),''),each=mn),'_',inp$metric)
      group = vapply(sug, function(g) do.call(paste, group[seq_len(g)]), character(nrow(sim)))
      if(!missing(comp.group)){
        comp.group = vapply(sug, function(g)
          do.call(paste, comp.group[seq_len(g)]), character(length(comp.group[[1]])))
      }
      if(!is.null(dsp$s$mean) && !dsp$s$mean){
        sal$symmetrical=TRUE
        sim=lapply(ug<-unique(sim[,1]),function(g){
          su=sim[,1]==g
          gsm=do.call(lma_simets,c(list(input[su,]),sal))
          gn=group[su,ncol(group)]
          for(m in names(gsm)) dimnames(gsm[[m]])=list(gn,gn)
          gsm
        })
        if(!is.null(dsp$s$symmetrical) && !dsp$s$symmetrical){
          sim=do.call(rbind,lapply(sim,function(ll){
            m=ll[[1]]
            su=lower.tri(m)
            as.data.frame(
              comp=outer(n<-colnames(m),n,function(a,b)paste0(a,' <-> ',b))[su],
              lapply(ll,function(m)m[su])
            )
          }))
        }else names(sim)=ug
      }else{
        sal$symmetrical=FALSE
        ssl = if(is.null(speaker)) TRUE else !is.na(speaker)
        for(g in unique(sim[, 1])){
          su = which(sim[, 1] == g & ssl)
          sg = group[su,, drop = FALSE]
          sx = input[su,, drop = FALSE]
          gck = ckc && !missing(comp.group)
          if(gck){
            gcsub = comp.group[, 1] == g
            if(!any(gcsub)){
              warning('the first comparison group has no levels in common with the first data group',
                call. = FALSE)
              gck = FALSE
            }
          }
          for(s in sug){
            usg = unique(sg[, s])
            if(length(usg) == 1){
              ssg = list(sx)
              names(ssg) = usg
            }else{
              ssg = lapply(usg, function(ss) sx[sg[, s] == ss,, drop = FALSE])
              names(ssg) = usg
              ssg = Filter(function(ss) nrow(ss) > 1, ssg)
            }
            if(length(ssg) != 0) for(ssn in names(ssg)){
              ssu = su[sg[, s] == ssn]
              lss = length(ssu)
              if(lss < 2) next
              if(cks) sal$group = speaker[ssu] else if(ckf)
                sal$b = if(compmeanck) colMeans(ssg[[ssn]], na.rm = TRUE) else
                  apply(na.omit(ssg[[ssn]]), 2, comp)
              if(!is.null(sal$b) && identical(sal$b, ssg[[ssn]])){
                sim[ssu, gl + mw + (mn * (s - 1))] = 1
                next
              }
              if(gck){
                gcsu = comp.group[, s] == ssn & gcsub
                if(!any(gcsu)) warning('no ', paste(usg, collapse = ', '),
                  ' level found in the comparison group(s)') else{
                  sal$b = comp.data[gcsu,, drop = FALSE]
                  if(nrow(sal$b) != 1) sal$b = colMeans(sal$b)
                }
              }
              ssim = do.call(lma_simets, c(list(ssg[[ssn]]), sal))
              if(ckp || ckq){
                if(ckp){
                  if(length(ssim[[1]])!=1) ssim=vapply(ssim,mean,0,na.rm=TRUE)
                }else if(nrow(ssim)>1) ssim=colMeans(ssim,na.rm=TRUE)
                if(lss!=1) ssim=vapply(ssim,rep,numeric(lss),lss)
              }
              csu = gl + mw + (mn * (s - 1))
              if(all(dim(ssim)==c(lss,length(csu)))) sim[ssu,csu]=ssim else warning(g,s)
            }
          }
        }
      }
    }
  }
  list(dtm=dtm,processed=input,comp.type=opt$comp,comp=comp.data,group=opt$group,sim=sim)
}

#' Document-Term Matrix Creation
#'
#' Creates a document-term matrix (dtm) from a set of texts.
#' @param text Texts to be processed. This can be a vector (such as a column in a data frame)
#'   or list.
#' @param exclude A character vector of words to be excluded. If \code{exclude} is a single string
#'   matching \code{'function'}, \code{lma_dict()} will be used.
#' @param context A character vector used to reformat text based on look- ahead/behind. For example,
#'   you might attempt to disambiguate \emph{like} by reformatting certain \emph{like}s
#'   (e.g., \code{context = c('(i) like*','(you) like*','(do) like')}, where words in parentheses are
#'   the context for the target word, and asterisks denote partial matching). This would be converted
#'   to regular expression (i.e., \code{'(?=i) like[ .,?!:;/]'}) which, if matched, would be
#'   replaced with a coded version of the word (e.g., \code{"Hey, i like that!"} would become
#'   \code{"Hey, i i-like- that!"}). This would probably only be useful for categorization, where a
#'   dictionary would only include one or another version of a word (e.g., the LIWC 2015 dictionary
#'   does something like this with \emph{like}, and LIWC 2007 did something like this with
#'   \emph{kind (of)}, both to try and clean up the posemo category).
#' @param numbers Logical: if \code{TRUE}, numbers are preserved.
#' @param punct Logical: if \code{TRUE}, punctuation is preserved.
#' @param urls Logical: if \code{FALSE}, attempts to replace all urls with "url".
#' @param emojis Logical: if \code{TRUE}, attempts to replace emojis (e.g., ":(" would be replaced
#'   with "repfrown").
#' @param to.lower Logical: if \code{FALSE}, words with different capitalization are treated as
#'   different terms.
#' @param word.break A regular expression string determining the way words are split. Default is
#'   \code{' +'} which breaks words at one or more blank spaces. You may also like to break by
#'   dashes or slashes (\code{'[ /-]+'}), depending on the text.
#' @param dc.min Numeric: excludes terms appearing in fewer than the set number of documents.
#'   Default is 0 (no limit).
#' @param dc.max Numeric: excludes terms appearing in more than the set number of documents. Default
#'   is Inf (no limit).
#' @param sparse Logical: if \code{FALSE}, a regular matrix is returned.
#' @param tokens.only Logical: if \code{TRUE}, returns a list rather than a matrix:
#'   \tabular{ll}{
#'     \code{tokens} \tab A vector of indices with terms as names. \cr
#'     \code{frequencies} \tab A vector of counts with terms as names. \cr
#'     \code{WC} \tab A vector of term counts for each document. \cr
#'     \code{indices} \tab A list with a vector of token indices for each document. \cr
#'   }
#' @note
#' This is a relatively simple way to make a dtm. To calculate the (more or less) standard forms of
#' LSM and LSS, a somewhat raw dtm should be fine, because both processes essentially use
#' dictionaries (obviating stemming) and weighting or categorization (largely obviating 'stop word'
#' removal). The exact effect of additional processing will depend on the dictionary/semantic space
#' and weighting scheme used (particularly for LSA). This function also does some processing which
#' may matter if you plan on categorizing using categories with look- ahead/behind. Otherwise,
#' other methods may be faster, more memory efficient, and/or more featureful.
#' @examples
#' text = c(
#'   "Why, hello there! How are you this evening?",
#'   "I am well, thank you for your inquiry!",
#'   "You are a most good at social interactions person!",
#'   "Why, thank you! You're not all bad yourself!"
#' )
#'
#' lma_dtm(text)
#' @export

lma_dtm = function(text, exclude = NULL, context = NULL, numbers = FALSE, punct = FALSE, urls = TRUE,
  emojis = FALSE, to.lower = TRUE, word.break = ' +', dc.min = 0, dc.max = Inf, sparse = TRUE,
  tokens.only = FALSE){
  if(is.list(text) && all(c('tokens', 'indices') %in% names(text))){
    m = do.call(rbind, lapply(seq_along(text$indices), function(i){
      inds = as.factor(text$indices[[i]])
      cbind(i, as.integer(levels(inds)), tabulate(inds))
    }))
    dtm = sparseMatrix(m[, 1], m[, 2], x = m[, 3], dimnames = list(NULL,
      if(is.character(text$tokens)) text$tokens else names(text$tokens)))
    if(!sparse) dtm = as.matrix(dtm)
    attr(dtm, 'colsums') = text$frequencies
    attr(dtm, 'type') = 'count'
    attr(dtm, 'WC') = text$WC
    attr(dtm, 'opts') = attr(text, 'opts')
    attr(dtm, 'time') = attr(text, 'time')
    return(dtm)
  }
  if(is.null(text)) stop(substitute(text),' not found')
  if(is.character(text) && all(file.exists(text))){
    text = if(length(text) != 1 || dir.exists(text)) read.segments(text) else readLines(text)
  }
  text = paste(' ', text, ' ')
  st = proc.time()[[3]]
  text = gsub('[\u05be\u1806\u2010\u2011\u2013\uFE58\uFE63\uFF0D]', '-', text)
  text = gsub('[\u2012\u2014\u2015\u2E3A\u2E3B]|--+', ' - ', text)
  text = gsub('[\u2032\u2035\u2018\u2019]', "'", text)
  text = gsub("[\u2033\u2036\u201C\u201D\u201F]|(?<=[^a-z0-9])'|'(?=[^a-z0-9])", '"', text, TRUE, TRUE)
  if(!urls){
    text = gsub(paste0('\\s[a-z]+://[^\\s]*|www\\.[^\\s]*|\\s[a-z_~-]+\\.[a-z_~-]{2,}[^\\s]*|\\s[a-z_~-]+\\.',
      '(?:io|com|net|org|gov|edu)\\s'), ' url ', text, TRUE, TRUE)
    text = gsub('(?<=[A-Z])\\.\\s', ' ', text, perl = TRUE)
  }
  text = gsub('[\\n\\t\\r]+', ' ', text, perl = TRUE)
  text = gsub('\\s(etc|st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof)\\.', ' \\1tempperiod', text)
  text = gsub('\\s\\.|\\.\\s',' . ', text)
  if(any(punct, emojis, !is.null(context))){
    special=lma_dict(special)[[1]]
    if(!missing(context) && length(context) == 1 && grepl('like', context, TRUE))
      context = special[['LIKE']]
    if(punct) text = gsub(special[['ELLIPSIS']], ' repellipsis ', text)
    if(emojis) for(type in c('SMILE', 'FROWN')) text = gsub(
      special[[type]], paste0(' rep', tolower(type), ' '), text, perl = TRUE
    )
    if(!missing(context)){
      if(!any(grepl('[?=]', context))){
        context = gsub('^\\(','(?<=', context)
        context = gsub('\\((?!\\?)','(?=', context, perl = TRUE)
        context = gsub('(?<![)*])$','\\\\b', context, perl = TRUE)
        context = gsub('\\*','\\\\w*', context, perl = TRUE)
      }
      context = structure(
        as.list(context),
        names = paste('', gsub('--+', '-', gsub('[\\s^[]|\\\\s]', '-',
          gsub("[^a-z0-9\\s\\\\']|\\\\[wbs]", '', context, TRUE, TRUE), perl = TRUE)), '')
      )
      for(rn in names(context)) text = gsub(context[[rn]], rn, text, perl = TRUE)
    }
  }
  if(to.lower) text = tolower(text)
  if(!is.null(exclude)){
    if(length(exclude) == 1 && grepl(exclude, 'function', TRUE)){
      exclude = unlist(lma_dict(), use.names = FALSE)
    }else if(is.list(exclude)) exclude = unlist(exclude, use.names = FALSE)
  }
  if(!numbers) text = gsub('[[:punct:]]*[0-9][0-9,.el-]*', ' ', text, TRUE, TRUE)
  text = gsub(paste0("([^a-z0-9.,':/?=#\\s-]|[:/?=#](?=\\s)|(?:(?<=\\s)[:/=-]|,)(?=[a-z])|(?<=[^a-z0-9])",
    "(,(?=[a-z0-9])|[.-](?=[a-z]))|[.,'-](?=[^0-9a-z]|[.,'-]))"), ' \\1 ', text, TRUE, TRUE)
  text = gsub("(\\s[a-z]+)/([a-z]+\\s)", ' \\1 / \\2 ', text, TRUE, TRUE)
  text = gsub("([a-z0-9.,'-].*[^a-z0-9])", ' \\1 ', text, TRUE, TRUE)
  text = gsub("(?<=[a-z])\\s['\u00E7\u00ED]\\s(?=[a-z])", "'", text, TRUE, TRUE)
  if(!punct){
    text = gsub("[^A-Za-z0-9'._-]+", ' ', text)
    text = gsub("(?=[a-z])\\.+|(?<=[^a-z0-9])['._-]+|'+\\s", ' ', text, TRUE, TRUE)
  }
  text = gsub('tempperiod', '.', text, fixed = TRUE)
  text = gsub('^\\s+|\\s(?=\\s)|\\s+$', '', text, perl = TRUE)
  text = strsplit(text, word.break)
  words = sort(unique(unlist(text)))
  words = words[!words == '']
  if(tokens.only){
    m = match_terms(
      text, words, !grepl('^[[:punct:]]$|^repellipsis$', words),
      c(length(text), length(words)), is.null(exclude), TRUE
    )
    names(m) = c('tokens', 'frequencies', 'WC', 'indices')
    m$tokens = m$tokens + 1
    m$tokens = sort(m$tokens)
    names(m$frequencies) = names(m$tokens)
    m$indices = unname(split(m$indices + 1, rep(seq_along(text), m$WC)))
  }else{
    if(!is.null(exclude)){
      if(is.list(exclude)) exclude = unlist(exclude, use.names = FALSE)
      if(!any(grepl('^', exclude, fixed = TRUE))) exclude = gsub('\\^\\*|\\*\\$', '', paste0('^', exclude, '$'))
      if(any(ck <- grepl('[[({]', exclude) + grepl('[})]|\\]', exclude) == 1))
        exclude[ck] = gsub('([([{}\\])])', '\\\\\\1', exclude[ck], perl = TRUE)
      words = grep(paste(exclude, collapse = '|'), words, value = TRUE, invert = TRUE)
    }
    msu = match_terms(
      text, words, !grepl('^[[:punct:]]$|^repellipsis$', words),
      c(length(text), length(words)), is.null(exclude), FALSE
    )
    m = if(sparse) as(msu[[1]], 'dgCMatrix') else as.matrix(msu[[1]])
    su = msu[[3]] > dc.min & msu[[3]] < dc.max
    names(msu[[3]]) = words
    if(any(!su)) m = m[, su]
    attr(m, 'WC') = unlist(msu[[2]], use.names = FALSE)
    attr(m, 'colsums') = msu[[3]]
    attr(m, 'type') = 'count'
    if(!missing(dc.min) || !missing(dc.max)) attr(m, 'info') =
      paste('a lim of', dc.min, 'and', dc.max, 'left', sum(su), 'of', length(words), 'unique terms')
  }
  attr(m, 'opts') = c(numbers = numbers, punct = punct, urls = urls, to.lower = to.lower)
  attr(m, 'time') = c(dtm = proc.time()[[3]] - st)
  m
}

#' Document-Term Matrix Weighting
#'
#' Weight a document-term matrix.
#' @param dtm a matrix with words as column names.
#' @param weight a string referring at least partially to one (or a combination; see note) of the
#'   available weighting methods:
#'
#'   \strong{Term weights} (applied uniquely to each cell)
#'   \tabular{lll}{
#'     \code{binary} \tab \code{(dtm > 0) * 1} \tab convert frequencies to 1s and 0s; remove
#'       differences in frequencies\cr
#'     \code{log} \tab \code{log(dtm + 1)} \tab log of frequencies\cr
#'     \code{sqrt} \tab \code{sqrt(dtm)} \tab square root of frequencies\cr
#'     \code{count} \tab \code{dtm} \tab unaltered; sometimes called term frequencies (tf)\cr
#'     \code{amplify} \tab \code{dtm ^ alpha} \tab amplify difference in frequencies\cr
#'   }
#'
#'   \strong{Document weights} (applied by column)
#'   \tabular{lll}{
#'     \code{dflog} \tab \code{log(colSums(dtm > 0))} \tab log of binary term sum\cr
#'     \code{entropy} \tab \code{1 - rowSums(x * log(x) / log(ncol(x)))} \tab where
#'       \code{x = t(dtm) / colSums(dtm > 0)};
#'       entropy of term-conditional term distribution\cr
#'     \code{ppois} \tab \code{1 - ppois(alpha, colSums(dtm) / nrow(dtm))} \tab Poisson-predicted
#'       term distribution\cr
#'     \code{dpois} \tab \code{1 - dpois(alpha, colSums(dtm) / nrow(dtm))} \tab Poisson-predicted
#'       term density\cr
#'     \code{dfmlog} \tab \code{log(diag(x[max.col(t(x)),]))} \tab log of maximum term
#'       frequency\cr
#'     \code{dfmax} \tab \code{diag(x[max.col(t(x)),])} \tab maximum term frequency\cr
#'     \code{df} \tab \code{colSums(dtm > 0)} \tab sum of binary term occurance across documents\cr
#'     \code{idf} \tab \code{log(nrow(dtm) / colSums(dtm > 0))} \tab inverse document frequency\cr
#'     \code{ridf} \tab \code{idf - log(poisson)} \tab residual inverse document frequency\cr
#'     \code{normal} \tab \code{1 / colSums(dtm ^ 2) ^ .5} \tab normalized document frequency\cr
#'   }
#'
#' Alternatively, \code{'pmi'} or \code{'ppmi'} will apply a pointwise mutual information weighting
#' scheme (with \code{'ppmi'} setting negative values to 0).
#' @param normalize logical: if \code{FALSE}, the dtm is not divided by document word-count before
#'   being weighted.
#' @param wc.complete if the dtm was made with \code{\link{lma_dtm}} (has a \code{'WC'}
#'   attribute), word counts for
#'   frequencies can be based on the raw count (default; \code{wc.complete = TRUE}). If
#'   \code{wc.complete = FALSE}, or the dtm does not have a \code{'WC'} attribute,
#'   \code{rowSums(dtm)} is used as word count.
#' @param log.base the base of logs, applied to any weight using \code{\link[base]{log}}.
#'   Default is 10.
#' @param alpha a scaling factor applied to document frequency as part of pointwise mutual
#'   information weighting, or amplify's power (\code{dtm ^ alpha}, which defaults to 1.1), or the
#'   specified quantile of the poisson distribution (\code{dpois(alpha,}
#'   \code{colSums(x, na.rm = TRUE) /} \code{nrow(x))}).
#' @param doc.only logical: if \code{TRUE}, only document weights are returned (a single value for
#'   each term).
#' @param percent logical; if \code{TRUE}, frequencies are multiplied by 100.
#' @note
#' Term weights works to adjust differences in counts within documents, with differences meaning
#' increasingly more from \code{binary} to \code{log} to \code{sqrt} to \code{count} to \code{amplify}.
#'
#' Document weights work to treat words differently based on their between-document or overall frequency.
#' When term frequencies are constant, \code{dpois}, \code{idf}, \code{ridf}, and \code{normal} give
#' less common words increasingly more weight, and \code{ppois}, \code{df}, \code{dflog}, and
#' \code{entropy} give less common words increasingly less weight.
#'
#' \code{weight} can either be a vector with two characters, corresponding to term weight and
#' document weight (e.g., \code{c('count', 'idf')}), or it can be a string with term and
#' document weights separated by any of \code{\*} (e.g., \code{'count \* idf'}).
#' \code{'tf'} is also acceptable for \code{'count'}, and \code{'tfidf'} will be parsed as
#' \code{c('count', 'idf')}, though this is a special case.
#'
#' For \code{weight}, term or document weights can be entered individually; term weights alone will
#' not apply any document weight, and document weights alone will apply a \code{'count'} term weight
#' (unless \code{doc.only = TRUE}, in which case a term-named vector of document weights is returned
#' instead of a weighted dtm).
#' @examples
#' # visualize term and document weights
#'
#' ## term weights
#' term_weights = c('binary', 'log', 'sqrt', 'count', 'amplify')
#' Weighted = sapply(term_weights, function(w) lma_weight(1:20, w, FALSE))
#' if(require(splot)) splot(Weighted ~ 1:20, labx = 'Raw Count', lines = 'co')
#'
#' ## document weights
#' doc_weights = c('df', 'dflog', 'dfmax', 'dfmlog', 'idf', 'ridf',
#'   'normal', 'dpois', 'ppois', 'entropy')
#' weight_range = function(w, value = 1){
#'   m = diag(20)
#'   m[upper.tri(m, TRUE)] = if(is.numeric(value)) value else unlist(lapply(
#'     1:20, function(v) rep(if(value == 'inverted') 21 - v else v, v)
#'   ))
#'   lma_weight(m, w, FALSE, doc.only = TRUE)
#' }
#'
#' if(require(splot)){
#'   category = rep(c('df', 'idf', 'normal', 'poisson', 'entropy'), c(4, 2, 1, 2, 1))
#'   op = list(
#'     laby = 'Relative (Scaled) Weight', labx = 'Document Frequency',
#'     leg = 'outside', colorby = list(quote(category), grade = TRUE),
#'     lines = 'connected', mv.scale = TRUE, note = FALSE
#'   )
#'   splot(
#'     sapply(doc_weights, weight_range) ~ 1:20,
#'     options = op, title = 'Same Term, Varying Document Frequencies',
#'     sud = 'All term frequencies are 1.'
#'   )
#'   splot(
#'     sapply(doc_weights, weight_range, value = 'sequence') ~ 1:20,
#'     options = op, title = 'Term as Document Frequencies',
#'     sud = 'Non-zero terms are the number of non-zero terms.'
#'   )
#'   splot(
#'     sapply(doc_weights, weight_range, value = 'inverted') ~ 1:20,
#'     options = op, title = 'Term Opposite of Document Frequencies',
#'     sud = 'Non-zero terms are the number of zero terms + 1.'
#'   )
#' }
#'
#' @export

lma_weight = function(dtm, weight = 'count', normalize = TRUE, wc.complete = TRUE,
  log.base = 10, alpha = 1, doc.only = FALSE, percent = FALSE){
  if(is.null(dim(dtm))) dtm = if(is.character(dtm) || is.factor(dtm)) lma_dtm(dtm) else matrix(dtm, 1)
  ck = attr(dtm, 'type')
  if(!is.null(ck) && length(ck) == 3 && (ck[1] == 'TRUE' || ck[2] != 'count' || ck[3] != 'NA')){
    message('the entered dtm appears to already be weighted (', paste(ck[2:3], collapse = '*'),
      '), so it will not be altered')
    return(dtm)
  }
  weight = tolower(weight)
  if(missing(normalize) && any(grepl('pmi', weight))) normalize = FALSE
  if(normalize){
    wc = attr(dtm, 'WC')
    if(is.null(wc) || !wc.complete || nrow(dtm) != length(wc)) wc = rowSums(dtm, na.rm = TRUE)
    adj = if(percent) 100 else 1
    if(.hasSlot(dtm, 'x')){
      wc = wc[dtm@i + 1]
      su = wc != 0
      dtm@x[su] = dtm@x[su] / wc[su] * adj
    }else{
      su = wc != 0
      dtm[su,] = dtm[su,] / wc[su] * adj
    }
  }
  nr = nrow(dtm)
  if(any(grepl('pmi', weight))){
    tw = dw = 'pmi'
    if(missing(log.base)) log.base = 2
    twc = sum(dtm, na.rm = TRUE)
    pc = matrix(colSums(dtm ^ alpha, na.rm = TRUE) / twc ^ alpha, 1)
    dtm = dtm / twc
    dtm = dtm / rowSums(dtm, na.rm = TRUE) %*% pc
    if(.hasSlot(dtm, 'x')){
      dtm@x = log(dtm@x, base = log.base)
      dtm@x[!is.finite(dtm@x)] = 0
    }else{
      dtm = log(dtm, base = log.base)
      dtm[!is.finite(dtm)] = 0
    }
    if(any(grepl('pp', weight))){
      tw = dw = 'ppmi'
      dtm[dtm < 0] = 0
    }
  }else{
    term = function(x, type) switch(type,
      binary = (x > 0) * 1,
      log = log(x + 1, base = log.base),
      sqrt = sqrt(x),
      count = x,
      amplify = x ^ alpha
    )
    doc = function(x, type){
      d = switch(type,
        df = colSums(x > 0, na.rm = TRUE),
        dflog = log(colSums(x > 0, na.rm = TRUE), base = log.base),
        dfmax = diag(x[max.col(t(x)),]),
        dfmlog = log(diag(x[max.col(t(x)),]), base = log.base),
        idf = log(nrow(x) / colSums(x > 0, na.rm = TRUE), base = log.base),
        normal = sqrt(1 / colSums(x ^ 2, na.rm = TRUE)),
        dpois = 1 - dpois(alpha, colSums(x, na.rm = TRUE) / nrow(x)),
        ppois = 1 - ppois(alpha, colSums(x, na.rm = TRUE) / nrow(x)),
        ridf = doc(x, 'idf') - log(doc(x, 'dpois'), base = log.base),
        entropy = {
          x = t(x) / colSums(x > 0, na.rm = TRUE)
          1 - rowSums(x * log(x, base = log.base) /
              log(ncol(x), base = log.base), na.rm = TRUE)
        }
      )
      d[!is.finite(d)] = 0
      d
    }
    if(length(weight) == 1){
      weight = strsplit(weight, ' *[:\\*_/; ,-] *')[[1]]
      if(length(weight) == 1 && weight == 'tfidf') weight = c('count', 'idf')
    }
    if(grepl('^(?:t|na|non|f)', weight[1])) weight[1] = 'count'
    tws = c('binary', 'log', 'sqrt', 'count', 'amplify')
    tw = grep(substr(weight[1], 0, 4), tws, value = TRUE)[1]
    pdw = TRUE
    dws = c('df', 'dflog', 'dfmax', 'dfmlog', 'idf', 'normal', 'dpois', 'ppois', 'ridf', 'entropy')
    if(is.na(tw)){
      tw = grep(substr(weight[1], 0, 4), dws, value = TRUE)[1]
      if(!is.na(tw)){
        pdw=FALSE
        if(!doc.only){
          dw = tw
          tw = 'count'
        }else return(doc(dtm, tw))
      }else stop(paste(weight), ' is not a recognized weight', call. = FALSE)
    }
    if(pdw) dw = if(length(weight) > 1) grep(substr(weight[2], 0, 4), dws, value = TRUE)[1] else 'none'
    if(missing(alpha) && tw == 'amplify') alpha = 1.1
    dtm = if(dw == 'none') term(dtm, tw) else term(dtm, tw) * rep(doc(dtm, dw), each = nr)
  }
  attr(dtm, 'type') = c(normalized = normalize, term = tw, document = dw)
  dtm
}

#' Latent Semantic Space (Embeddings) Operations
#'
#' Map a document-term matrix onto a latent semantic space, extract terms from a
#' latent semantic space (if \code{dtm} is a character vector, or \code{map.space =} \code{FALSE}),
#' or perform a singular value decomposition on a document-term matrix (if \code{dtm} is a matrix
#' and \code{space} is missing).
#' @param dtm A matrix with terms as column names, or a character vector of terms to be extracted
#'   from a specified space. If this is of length 1 and \code{space} is missing, it will be treated
#'   as \code{space}.
#' @param space A matrix with terms as rownames. If missing, this will be the right singular vectors
#'   of a singular value decomposition of \code{dtm}. If a character, a file matching the character
#'   will be searched for in \code{dir} (e.g., \code{space = 'google'}). If a file is not found and
#'   the character matches one of the \href{https://osf.io/489he/wiki/home}{available spaces}, you
#'   will be given the option to download it, as handled by \code{\link{download.lsspace}}.
#'   If \code{dtm} is missing, the entire space will be loaded and returned.
#' @param map.space Logical: if \code{FALSE}, the original vectors of \code{space} for terms
#'   found in \code{dtm} are returned. Otherwise \code{dtm} \code{\%*\%} \code{space} is returned,
#'   excluding uncommon columns of \code{dtm} and rows of \code{space}.
#' @param fill.missing Logical: if \code{TRUE} and terms are being extracted from a space, includes
#'   terms not found in the space as rows of 0s, such that the returned matrix will have a row
#'   for every requested term.
#' @param term.map A matrix with \code{space} as a column name, terms as row names, and indices of
#'   the terms in the given space as values, or a numeric vector of indices with terms as names, or
#'   a character vector or terms corresponding to rows of the space. This is used instead of reading
#'   in an "_terms.txt" file corresponding to a \code{space} entered as a character (the name of a
#'   space file).
#' @param dim.cutoff If a \code{space} is calculated, this will be used to decide on the number of
#'   dimensions to be retained: \code{cumsum(d) / sum(d) < dim.cutoff}, where \code{d} is a vector
#'   of singular values of \code{dtm} (i.e., \code{svd(dtm)$d}). The default is \code{.5}; lower
#'   cutoffs result in fewer dimensions.
#' @param keep.dim Logical: if \code{TRUE}, and a space is being calculated from the input, a matrix
#'   in the same dimensions as \code{dtm} is returned. Otherwise, a matrix with terms as rows and
#'   dimensions as columns is returned.
#' @param use.scan Logical: if \code{TRUE}, reads in the rows of \code{space} with \code{\link{scan}}.
#' @param dir Path to a folder containing spaces. Default is \code{getOption('lingmatch.lspace.dir')};
#'   change with \code{options(lingmatch.lspace.dir} \code{= 'desired/path')}.
#' @note
#' A traditional latent semantic space is a selection of right singular vectors from the singular
#' value decomposition of a dtm (\code{svd(dtm)$v[, 1:k]}, where \code{k} is the selected number of
#' dimensions, decided here by \code{cutoff}).
#'
#' Mapping a new dtm into a latent semantic space consists of multiplying common terms:
#' \code{dtm[, ct] \%*\% space[ct,]}, where \code{ct} \code{=} \code{colnames(dtm)[colnames(dtm)}
#' \code{\%in\%} \code{rownames(space)]} -- the terms common between the dtm and the space. This
#' results in a matrix with documents as rows, and dimensions as columns, replacing terms.
#' @family Latent Semantic Space functions
#' @examples
#'
#' text = c(
#'   "Hey, I like kittens. I think all kinds of cats really are just the best pet ever.",
#'   "Oh year? Well I really like cars. All the wheels and the turbos... I think that's the best
#'     ever.",
#'   "You know what? Poo on you. Cats, dogs, rabbits -- you know, living creatures... to think
#'     you'd care about anything else!",
#'   "You can stick to your opinion. You can be wrong if you want. You know what life's about?
#'     Supercharging, diesel guzzling, exhaust spewing, piston moving ignitions."
#' )
#'
#' dtm = lma_dtm(text)
#'
#' # calculate a latent semantic space from the example text
#' lss = lma_lspace(dtm)
#'
#' # show that document similarities between the truncated and full space are the same
#' spaces = list(
#'   full = lma_lspace(dtm, keep.dim = TRUE),
#'   truncated = lma_lspace(dtm, lss)
#' )
#' sapply(spaces, lma_simets, metric = 'cosine')
#'
#' \dontrun{
#'
#' # map to a pretrained space
#' ddm = lma_lspace(dtm, '100k')
#'
#' # load the matching subset of the space
#' # without mapping
#' lss_100k_part = lma_lspace(colnames(dtm), '100k')
#'
#' ## or
#' lss_100k_part = lma_lspace(dtm, '100k', map.space = FALSE)
#'
#' # load the full space
#' lss_100k = lma_lspace('100k')
#'
#' ## or
#' lss_100k = lma_lspace(space = '100k')
#'
#' }
#' @export

lma_lspace = function(dtm = '', space, map.space = TRUE, fill.missing = FALSE, term.map = NULL,
  dim.cutoff = .5, keep.dim = FALSE, use.scan = FALSE, dir = getOption('lingmatch.lspace.dir')){
  dir = path.expand(dir)
  if(is.character(dtm) || is.factor(dtm)){
    if(length(dtm) > 1 && missing(space)){
      dtm = lma_dtm(dtm)
    }else if(length(dtm) == 1 && dtm != ''){
      if(missing(use.scan)) use.scan = TRUE
      space = dtm
      dtm = ''
    }
  }
  if(is.data.frame(dtm)) dtm = as.matrix(dtm)
  if(missing(space)){
    nr = nrow(dtm)
    if(is.null(nr)) stop('enter a matrix for dtm, or specify a space')
    nc = ncol(dtm)
    md = min(nr, nc)
    s = svd(dtm)
    s$v = t(s$v)
    k = cumsum(s$d) / sum(s$d)
    if(dim.cutoff > 1) dim.cutoff = 1
    k = seq_len(if(any(k < dim.cutoff)) which(k >= dim.cutoff)[1] else 1)
    if(keep.dim){
      dtm[] = s$u[, k] %*% (if(length(k) == 1) matrix(s$d[k]) else diag(s$d[k])) %*% s$v[k,]
    }else{
      cn = colnames(dtm)
      dtm = t(s$v[k,, drop = FALSE])
      rownames(dtm) = cn
    }
  }else{
    terms = if(is.null(colnames(dtm))){
      map.space = FALSE
      dtm
    }else colnames(dtm)
    if(is.character(space)){
      if(space == 'default') space = '100k'
      name = sub('\\..*$', '', space)[1]
      spaces = list.files(dir)
      ts = grep(space, spaces, fixed = TRUE, value = TRUE)
      if(!length(ts)){
        ts = rownames(select.lsspace(name)$selected)[1]
        if(!is.na(ts) && grepl('^$|^[yt1]|^ent', readline(paste0(
          'would you like to download the ', ts, ' space? (press Enter for yes): ')))){
          download.lsspace(ts, dir = dir)
          ts = paste0(ts, '.dat')
        }else stop('space (', space, ') not found in dir (', dir, ')', call. = FALSE)
      }
      if(grepl('[bgx]z[ip2]*$', ts[1])) use.scan = TRUE
      space_path = paste0(dir, '/', ts[1])
      name = sub('\\..*$', '', ts[1])
      if(name %in% colnames(term.map)) term.map = term.map[term.map[, name] != 0, name]
      rex = function(inds, f){
        nc = length(strsplit(readLines(f, 1), '\\s+')[[1]])
        l = length(inds)
        all = all(seq_len(l) == inds)
        r = matrix(0, l, nc)
        i = 1
        con = file(f, 'r')
        on.exit(close(con))
        while(i <= l){
          if(all){
            n = l
          }else{
            n = 1
            while(i + n < l && inds[i + n - 1] == inds[i + n] - 1) n = n + 1
          }
          r[seq_len(n) + i - 1,] = matrix(scan(
            con, n = n * nc, quiet = TRUE, skip = (if(i == 1) inds[i] else
              inds[i] - inds[i - 1]) - 1, quote = '', comment.char = '', na.strings = ''
          ), n, nc, byrow = TRUE)
          i = i + n
        }
        r
      }
      if(!is.null(term.map)){
        if(is.character(term.map)) term.map = structure(seq_along(term.map), names = term.map)
        su = which(names(term.map) %in% terms)
        inds = as.numeric(sort(if(length(terms) == 1 && terms == '') term.map else term.map[su]))
        if(length(inds)){
          space = if(use.scan) rex(inds, space_path) else t(extract_indices(inds, space_path))
          rownames(space) = ts = names(term.map)[inds]
        }else stop('no matching terms in space ', space)
      }else{
        if(!file.exists(paste0(dir, '/', name, '_terms.txt'))){
          if(file.exists(paste0(dir, '/lma_term_map.rda'))){
            lma_term_map = NULL
            load(paste0(dir, '/lma_term_map.rda'))
            if(!is.null(lma_term_map) && !is.null(colnames(lma_term_map)) && name %in% colnames(lma_term_map)){
              space_terms = names(lma_term_map[lma_term_map[, name] != 0, name])
            }else stop(
              'could not find terms file (', space, '_terms.txt) in space (', dir, '),',
              'nor retrieve terms from them term map (lma_term_map.rda).'
            )
          }else stop('terms file (', space, '_terms.txt) not found in dir (', dir, ')')
        }else space_terms = readLines(paste0(dir, '/', name, '_terms.txt'))
        su = if(length(terms) == 1 && terms == ''){
          terms = space_terms
          !logical(length(space_terms))
        }else space_terms %in% terms
        if(sum(su) < length(terms)){
          lsterms = tolower(space_terms)
          su2 = !duplicated(lsterms) & lsterms %in% terms[!terms %in% space_terms[su]]
          if(any(su2)){
            space_terms[su2] = lsterms[su2]
            su = su | su2
          }
        }
        if(sum(su)){
          space = if(use.scan) rex(which(su), space_path) else t(extract_indices(which(su), space_path))
          rownames(space) = space_terms[su]
          ts = terms[terms %in% rownames(space)]
          space = space[ts,, drop = FALSE]
        }else stop('no matching terms in space ', space)
      }
    }else{
      if(is.data.frame(space)) space = as.matrix(space)
      name = deparse(substitute(space))
      su = terms %in% rownames(space)
      if(sum(su)){
        ts = terms[su]
        space = space[ts,, drop = FALSE]
      }else if(sum(su <- terms %in% colnames(space))){
        ts = terms[su]
        space = t(space[, ts, drop = FALSE])
      }else stop('no matching terms in provided space')
    }
    if(fill.missing){
      su = which(!terms %in% rownames(space))
      if(length(su)){
        space = rbind(space, matrix(0, length(su), ncol(space), dimnames = list(terms[su])))
        space = space[terms,]
      }
      ts = rownames(space)
    }
    attr(space, 'space') = name
    if(map.space){
      rep = length(ts) / ncol(dtm)
      if(rep < .2) warning(paste0(
        'only ', round(rep * 100, 2), '% of dtm terms appear in the provided space; ',
        'you might consider using a different source or cleaning/partial matching terms'
      ), call. = FALSE)
      dtm = dtm[, ts, drop = FALSE] %*% space
      attr(dtm, 'space') = name
    }else return(space)
  }
  dtm
}

#' Document-Term Matrix Categorization
#'
#' Reduces the dimensions of a document-term matrix by dictionary-based categorization.
#' @param dtm A matrix with words as column names.
#' @param dict The name of a provided dictionary
#'   (\href{https://osf.io/y6g5b/wiki/home}{osf.io/y6g5b/wiki}) or of a file found in
#'   \code{dir}, or a \code{list} object with named character vectors as word lists,
#'   or the path to a file to be read in by \code{\link{read.dic}}.
#' @param term.weights A \code{list} object with named numeric vectors lining up with the character
#'   vectors in \code{dict}, used to weight the terms in each \code{dict} vector. If a category in
#'   \code{dict} is not specified in \code{term.weights}, or the \code{dict} and \code{term.weights}
#'   vectors aren't the same length, the weight for that category will be 1.
#' @param bias A list or named vector specifying a constant to add to the named category. If an
#'   '_intercept' is included in a category, if will be removed from the category, and the associated
#'   \code{weight} will be used as the \code{bias} for that category.
#' @param escape Logical indicating whether the terms in \code{dict} should not be treated as plain
#'   text (including asterisk wild cards). If \code{TRUE}, regular expression related characters are
#'   escaped. Set to \code{TRUE} if you get PCRE compilation errors.
#' @param partial Logical; if \code{TRUE} terms are partially matched (not padded by ^ and $).
#' @param glob Logical; if \code{TRUE} (default), will convert initial and terminal asterisks to
#'   partial matches.
#' @param term.filter A regular expression string used to format the text of each term (passed to
#'   \code{gsub}). For example, if terms are part-of-speech tagged (e.g.,
#'   \code{'a_DT'}), \code{'_.*'} would remove the tag.
#' @param term.break If a category has more than \code{term.break} characters, it will be processed
#'   in chunks. Reduce from 20000 if you get a PCRE compilation error.
#' @param dir Path to a folder in which to look for \code{dict}; defaults to
#'   \code{getOption('lingmatch.dict.dir')}.
#' @seealso For applying pattern-based dictionaries (to raw text) see \code{\link{lma_patcat}}.
#' @family Dictionary functions
#' @examples
#' # Score texts with the NRC Affect Intensity Lexicon
#' \dontrun{
#'
#' dict = readLines('https://saifmohammad.com/WebDocs/NRC-AffectIntensity-Lexicon.txt')
#' dict = read.table(
#'   text = dict[-seq_len(grep('term\tscore', dict, fixed = TRUE)[[1]])],
#'   col.names = c('term', 'weight', 'category')
#' )
#'
#' text = c(
#'   angry = paste(
#'     'We are outraged by their hateful brutality,',
#'     'and by the way they terrorize us with their hatred.'
#'   ),
#'   fearful = 'The horrific torture of that terrorist was tantamount to the terrorism of terrorists.',
#'   joyous = 'I am jubilant to be celebrating the bliss of this happiest happiness.',
#'   sad = 'They are nearly suicidal in their mourning after the tragic and heartbreaking holocaust.'
#' )
#'
#' emotion_scores = lma_termcat(
#'   text, split(dict$term, dict$category), split(dict$weight, dict$category)
#' )
#' if(require('splot')) splot(emotion_scores ~ names(text), leg = 'out')
#' }
#' @export

lma_termcat=function(dtm, dict, term.weights = NULL, bias = NULL, escape = TRUE, partial = FALSE,
  glob = TRUE, term.filter = NULL, term.break = 2e4, dir = getOption('lingmatch.dict.dir')){
  st=proc.time()[[3]]
  if(missing(dict)) dict = lma_dict(1:9)
  if(is.character(dict) && length(dict) == 1 && !grepl('[^a-z]', dict, TRUE)){
    name = sub('\\.[^.]*$', '', dict)[1]
    dicts = list.files(dir, full.names = TRUE)
    ts = dicts[grepl(dict, sub('^.*/', '', dicts), fixed = TRUE)][1]
    if(is.na(ts)){
      ts = rownames(select.dict(paste0('^', name))$selected)[1]
      if(!is.na(ts) && grepl('^$|^[yt1]|^ent', readline(paste0(
        'would you like to download the ', ts, ' dictionary? (press Enter for yes): ')))){
        ts = download.dict(ts, dir = dir)
      }else if(grepl('\\.[a-z]{2,4}$', dict)) stop('dictionary (', dict,
        ') not found in dir (', dir, ')', call. = FALSE)
    }
    if(!is.na(ts)) dict = ts
  }
  if(!is.null(ncol(dict))){
    if(!is.null(term.weights)){
      if(is.character(term.weights) && any(su <- term.weights %in% colnames(dict))){
        term.weights = dict[, term.weights[su], drop = FALSE]
      }
      if(!is.null(ncol(term.weights))) term.weights = term.weights[, vapply(seq_len(ncol(term.weights)),
        function(col) is.numeric(term.weights[, col]), TRUE)]
    }else if(any(su <- vapply(seq_len(ncol(dict)), function(col) is.numeric(dict[, col]), TRUE))){
      term.weights = dict[, su, drop = FALSE]
    }
    if(!is.null(rownames(dict)) && any(grepl('^[a-z]', rownames(dict), TRUE))){
      dict = rownames(dict)
    }else{
      su = vapply(seq_len(ncol(dict)), function(col) !is.numeric(dict[, col]), TRUE)
      if(!any(su)) stop('no terms found in dictionary')
      dict = if(sum(su) > 1){
        su = vapply(seq_len(ncol(dict)), function(col) anyDuplicated(dict[, col]) == 0, TRUE)
        if(any(su)) dict[, which(su)[1]] else dict[, 1]
      }else dict[, su]
    }
  }
  if(!is.null(ncol(term.weights))){
    term.weights = term.weights[, vapply(seq_len(ncol(term.weights)),
      function(col) is.numeric(term.weights[, col]), TRUE), drop = FALSE]
    if(!ncol(term.weights)) stop('no numeric columns in term.weights')
    if(is.null(colnames(term.weights))) colnames(term.weights) = paste0('cat', seq_len(ncol(term.weights)))
    if(!is.data.frame(term.weights)) term.weights = as.data.frame(term.weights)
  }
  if(!is.list(dict)) dict = if(length(dict) == 1 && is.character(dict) && (file.exists(dict) ||
      dict %in% rownames(select.dict()$info)))
    read.dic(dict) else list(dict)
  if(is.null(names(dict))) names(dict) = seq_along(dict)
  if(is.null(term.weights)){
    if(is.numeric(dict[[1]]) && !is.null(names(dict[[1]]))){
      term.weights = dict
      dict = lapply(dict, names)
    }else term.weights = lapply(dict, function(cat) rep(1, length(cat)))
  }else if(is.null(nrow(term.weights))){
    if(!is.list(term.weights)) term.weights = list(term.weights)
    dlen = length(dict)
    if(is.null(names(term.weights)))
      names(term.weights) = if(length(term.weights) == dlen) names(dict) else seq_along(term.weights)
    if(length(term.weights) > dlen && dlen == 1 && all(vapply(term.weights, length, 0) == length(dict[[1]])))
      dict = lapply(term.weights, function(ws) dict[[1]])
  }
  dict = lapply(dict, function(cat) if(!is.character(cat))
    if(is.null(names(cat))) as.character(cat) else names(cat) else cat)
  if(!is.null(bias) && is.null(names(bias)))
    names(bias) = if(length(bias) == length(dict)) names(dict) else seq_along(bias)
  for(n in names(dict)) if(!n %in% names(bias) && any(ii <- !is.na(dict[[n]]) & dict[[n]] == '_intercept')){
    dict[[n]] = dict[[n]][!ii]
    bias[n] = term.weights[[n]][ii]
    term.weights[[n]] = term.weights[[n]][!ii]
  }
  if(is.character(dtm) || is.factor(dtm)) dtm=lma_dtm(dtm)
  ats = attributes(dtm)[c('opts', 'WC', 'type')]
  ats = ats[!vapply(ats, is.null, TRUE)]
  atsn = names(ats)
  cls = structure(numeric(length(dict)), names = names(dict))
  for(cat in seq_along(dict)){
    ccls = tryCatch(nchar(dict[[cat]]), error = function(e) NULL)
    if(is.null(ccls)){
      warning('dict appears to be miss-encoded, so results may not be as expected;\n',
        'might try reading the dictionary in with encoding = "ISO-8859-1"')
      dict[[cat]] = iconv(dict[[cat]], sub = '#')
      ccls = nchar(dict[[cat]])
    }
    cls[cat] = sum(ccls)
  }
  odict = dict
  formatdict = function(dict){
    lab = lapply(dict, function(l) grepl('[{([]', l) + grepl('[])}]', l) == 1)
    lab = lab[names(lab)[vapply(lab, any, TRUE)]]
    if(!partial){
      s = '^'
      e = '$'
    }else s = e = ''
    rec = '([][)(}{*.^$+?\\|\\\\])'
    if(length(lab)){
      for(l in names(lab)) dict[[l]][lab[[l]]] = gsub('([][)(}{])', '\\\\\\1', dict[[l]][lab[[l]]])
      rec = '([*.^$+?\\|])'
    }
    res = if(escape) lapply(dict, function(l)
      paste0(s, gsub(rec, '\\\\\\1', l, perl = TRUE), e, collapse = '|')
    ) else lapply(dict, function(l) paste(paste0(s, gsub('([+*])[+*]+', '\\\\\\1+', l), e), collapse='|'))
    if(glob) lapply(res, function(l) gsub(paste0(
      if(s == '^') '\\' else '', s, if(escape) '\\\\' else '', '\\*|', if(escape) '\\\\' else '', '\\*', if(e == '$')
        '\\' else '', e
    ), '', l)) else res
  }
  getweights = function(terms, cat){
    if(!cat %in% names(term.weights)) term.weights[[cat]] = rep(1, cls[[cat]])
    if(is.null(names(term.weights[[cat]])) && length(term.weights[[cat]]) == length(odict[[cat]]))
      names(term.weights[[cat]]) = odict[[cat]]
    if(any(mcn <- !terms %in% names(term.weights[[cat]])))
      term.weights[[cat]][terms[mcn]] = 1
    term.weights[[cat]][terms]
  }
  ws = if(is.null(term.filter)) colnames(dtm) else gsub(term.filter, '', colnames(dtm), perl = TRUE)
  if('opts' %in% atsn && !ats$opts['to.lower']) ws = tolower(ws)
  boundries = FALSE
  for(l in dict){
    if(!boundries) boundries = !any(grepl('^\\*|\\*$', l)) && any(grepl('^\\^|\\$$', l))
    if(missing(partial) && boundries) partial = TRUE
    if(missing(glob) && (any(grepl('([][}{.^$+?\\|\\\\])', l)) || any(grepl('\\w\\*\\w', l)))) glob = FALSE
    if(missing(escape) && (boundries || any(grepl('[.])][+*]|[.+*]\\?|\\[\\^', l))) &&
      !any(grepl('[({[][^])}]*$|^[^({[]*[])}]', l))) escape = FALSE
  }
  if(any(cls > term.break)){
    br = function(l, e = term.break){
      f = ceiling(cls[[l]] / e)
      l = length(dict[[l]])
      e = ceiling(l / f)
      o = lapply(seq_len(f), function(i) seq_len(e) + e * (i - 1))
      o[[f]] = o[[f]][o[[f]] <= l]
      o
    }
    if(is.null(ncol(term.weights))){
      op = matrix(0, nrow(dtm), length(dict), dimnames = list(rownames(dtm), names(dict)))
      for(cat in names(dict)){
        matches = if(cls[[cat]] > term.break){
          unique(unlist(lapply(br(cat), function(s)
            grep(formatdict(list(dict[[cat]][s]))[[1]], ws, perl = TRUE))))
        }else grep(formatdict(list(dict[[cat]])), ws, perl = TRUE)
        op[, cat] = if(length(matches)){
          weights = getweights(colnames(dtm)[matches], cat)
          colSums(t(dtm[, matches, drop = FALSE]) * weights, na.rm = TRUE)
        }else numeric(nrow(dtm))
      }
    }else{
      op = matrix(0, nrow(dtm), ncol(term.weights), dimnames = list(rownames(dtm), colnames(term.weights)))
      matches = unique(unlist(lapply(br(names(dict)[[1]]), function(s)
        grep(formatdict(list(dict[[1]][s]))[[1]], ws, perl = TRUE))))
      for(cat in colnames(term.weights)){
        op[, cat] = if(length(matches)){
          colSums(t(dtm[, matches, drop = FALSE]) * term.weights[matches, cat], na.rm = TRUE)
        }else numeric(ncol(dtm))
      }
    }
  }else{
    dict = formatdict(dict)
    op = if(!is.null(term.weights)){
      if(is.null(ncol(term.weights))){
        vapply(names(dict), function(cat){
          su = dtm[, grep(dict[[cat]], ws, perl = TRUE), drop = FALSE]
          weights = getweights(colnames(su), cat)
          if(!ncol(su)) numeric(nrow(su)) else colSums(t(su) * weights, na.rm = TRUE)
        }, numeric(nrow(dtm)))
      }else{
        ssu = grep(dict[[1]], ws, perl = TRUE)
        su = dtm[, ssu, drop = FALSE]
        vapply(colnames(term.weights), function(cat){
          weights = term.weights[ssu, cat]
          if(!ncol(su)) numeric(nrow(su)) else colSums(t(su) * weights, na.rm = TRUE)
        }, numeric(nrow(dtm)))
      }
    }else{
      vapply(names(dict), function(cat) rowSums(dtm[, grep(dict[[cat]], ws, perl = TRUE),
        drop = FALSE], na.rm = TRUE), numeric(nrow(dtm)))
    }
  }
  if(!is.null(bias)) for(n in names(bias)) if(n %in% colnames(op)) op[, n] = op[, n] + bias[[n]]
  attr(op,'WC')=if('WC'%in%atsn) ats$WC else rowSums(dtm,na.rm=TRUE)
  attr(op, 'time') = c(attr(dtm, 'time'), termcat = proc.time()[[3]] - st)
  if('type'%in%atsn) attr(op,'type')=ats$type
  op
}

match_metric = function(x){
  mets = c('jaccard', 'euclidean', 'canberra', 'cosine', 'pearson')
  sel = if(is.null(x) || length(x) == 1 && grepl(tolower(substr(x, 1, 1)), 'a', fixed = TRUE))
    mets else if(is.function(x)){
      stop('only internal metrics are available: ', paste(mets, collapse = ', '), call. = FALSE)
    }else{
      if(is.null(x)) 'cosine' else if(is.numeric(x)) mets[x] else{
        su = grepl('cor', x)
        if(any(su)) x[su] = 'pearson'
        unique(unlist(lapply(substr(x, 1, 3), grep, mets, fixed = TRUE, value = TRUE)))
      }
    }
  list(all = mets, selected = sel, dummy = as.integer(mets %in% sel))
}

#' Calculate similarity between vectors
#'
#' @param a vector or matrix. If a vector, \code{b} must also be provided. If a matrix and \code{b}
#'   is missing, each row will be compared. If a matrix and \code{b} is not missing, each row will
#'   be compared with \code{b} or each row of \code{b}.
#' @param b vector or matrix to be compared with \code{a} or rows of \code{a}.
#' @param metric a character or vector of characters at least partially matching one of the
#'   available metric names (or 'all' to explicitly include all metrics),
#'   or a number or vector of numbers indicating the metric by index:
#'   \tabular{ll}{
#'     \code{jaccard} \tab \code{sum(a & b) / sum(a | b)} \cr
#'     \code{euclidean} \tab \code{1 / (1 + sqrt(sum((a - b) ^ 2)))} \cr
#'     \code{canberra} \tab \code{mean(1 - abs(a - b) / (a + b))} \cr
#'     \code{cosine} \tab \code{sum(a * b) / sqrt(sum(a ^ 2 * sum(b ^ 2)))} \cr
#'     \code{pearson} \tab \code{(mean(a * b) - (mean(a) * mean(b))) / sqrt(mean(a ^ 2) - mean(a) ^ 2) /
#'       sqrt(mean(b ^ 2) - mean(b) ^ 2)} \cr
#'   }
#' @param group if \code{b} is missing and \code{a} has multiple rows, this will be used to make
#'   comparisons between rows of \code{a}, as modified by \code{agg} and \code{agg.mean}.
#' @param lag Amount to adjust the \code{b} index; either rows if \code{b} has multiple rows (e.g.,
#'   for \code{lag = 1}, \code{a[1,]} is compared with \code{b[2,]}), or values otherwise (e.g.,
#'   for \code{lag = 1}, \code{a[1]} is compared with \code{b[2]})
#' @param agg logical; if \code{FALSE}, only the boundary rows between groups will be compared, see
#'   example.
#' @param agg.mean logical; if \code{FALSE} aggregated rows are summed instead of averaged.
#' @param pairwise logical; if \code{FALSE} and \code{a} and \code{b} are matrices with the same number of
#'   rows, only paired rows are compared. Otherwise (and if only \code{a} is supplied), all pairwise
#'   comparisons are made.
#' @param symmetrical logical; if \code{TRUE} and pairwise comparisons between \code{a} rows were made,
#'   the results in the lower triangle are copied to the upper triangle.
#' @param mean logical; if \code{TRUE}, a single mean for each metric is returned per row of \code{a}.
#' @param return.list logical; if \code{TRUE}, a list-like object will always be returned, with an entry
#'   for each metric, even when only one metric is requested.
#' @details
#' Use \code{\link[RcppParallel]{setThreadOptions}} to change parallelization options; e.g., run
#' RcppParallel::setThreadOptions(4) before a call to lma_simets to set the number of CPU
#' threads to 4.
#' @return Output varies based on the dimensions of \code{a} and \code{b}:
#'   \tabular{ll}{
#'     \strong{output} \tab \strong{input} \cr
#'     vector with a value per metric \tab Only when \code{a} and \code{b} are both vectors.\cr
#'     vector with a value per row \tab Any time a single value is expected per row: \code{a} or \code{b} is a vector,
#'       \code{a} and \code{b} are matrices with the same number of rows and \code{pairwise = FALSE}, a group is
#'       specified, or \code{mean = TRUE}, and only one metric is requested.\cr
#'     data.frame with a column per metric \tab When multiple metrics are requested in the previous case.\cr
#'     sparse matrix \tab Pairwise comparisons within an \code{a} matrix or between
#'       an \code{a} and \code{b} matrix, when only 1 metric is requested.\cr
#'     list with a sparse matrix per metric \tab When multiple metrics are requested in the previous case.\cr
#'   }
#' @examples
#' text = c(
#'   'words of speaker A', 'more words from speaker A',
#'   'words from speaker B', 'more words from speaker B'
#' )
#' (dtm = lma_dtm(text))
#'
#' # compare each entry
#' lma_simets(dtm)
#'
#' # compare each entry with the mean of all entries
#' lma_simets(dtm, colMeans(dtm))
#'
#' # compare by group (corresponding to speakers and turns in this case)
#' speaker = c('A', 'A', 'B', 'B')
#'
#' ## by default, consecutive rows from the same group are averaged:
#' lma_simets(dtm, group = speaker)
#'
#' ## with agg = FALSE, only the rows at the boundary between
#' ## groups (rows 2 and 3 in this case) are used:
#' lma_simets(dtm, group = speaker, agg = FALSE)
#' @export

lma_simets=function(a, b = NULL, metric = NULL, group = NULL, lag = 0, agg = TRUE, agg.mean = TRUE,
  pairwise = TRUE, symmetrical = FALSE, mean = FALSE, return.list = FALSE){
  cf = NULL
  mets = c('jaccard', 'euclidean', 'canberra', 'cosine', 'pearson')
  if(missing(metric) && length(b) == 1 && !grepl(' ', b) &&
    any(grepl(tolower(substr(b, 1, 3)), mets, fixed = TRUE))){
    metric = b
    b = NULL
  }
  met = match_metric(metric)
  st = proc.time()[[3]]
  slots = c('i', 'p', 'x', 'Dim')
  if((is.character(a) || is.factor(a)) && any(grepl('[a-zA-Z]', a))) a = lma_dtm(a) else
    if(is.data.frame(a)) a = Matrix(as.matrix(a), sparse = TRUE)
  if(is.null(b) && !missing(lag) && is.null(dim(a))) b = a
  if(is.null(b)){
    n = dim(a)[1]
    if(is.null(n) || n < 2) stop('a must have more than 1 row when b is not provided', call. = FALSE)
    if(is.null(group)){
      if(!all(slots %in% slotNames(a))) a = as(a, 'dgCMatrix')
      res = calculate_similarities(a, NULL, 2, met$dummy)
      for(i in seq_along(res)) attr(res[[i]], 'metric') = met$selected[i]
    }else{
      if(length(group) != n) stop('group is not the same length as a or columns in a')
      ager = if(agg.mean) colMeans else colSums
      l = length(group)
      chunks = NULL
      i = 1
      while(i < l){
        st = i
        g = group[i]
        while(i < l && g == group[i + 1]) i = i + 1
        chunks = c(chunks, list(seq(st, i)))
        i = i + 1
      }
      if(!any(chunks[[length(chunks)]] == l)) chunks = c(chunks, list(l))
      rows = character(length(chunks) - 1)
      res = as.data.frame(matrix(0, length(chunks) - 1, sum(met$dummy), dimnames = list(NULL, met$selected)))
      for(i in seq_len(length(chunks) - 1)){
        s = chunks[[i]]
        sa = if(agg) s else s[length(s)]
        ta = ager(a[sa,, drop = FALSE])
        s = chunks[[i + 1]]
        sb = if(agg) s else s[1]
        tb = ager(a[sb,, drop = FALSE])
        res[i,] = vector_similarity(ta, tb, met$dummy)
        rows[i] = paste(paste(sa, collapse = ', '), '<->', paste(sb, collapse = ', '))
      }
      rownames(res) = rows
    }
  }else{
    if((is.character(b) || is.factor(b)) && any(grepl('[a-zA-Z]', b))) b = lma_dtm(b) else
      if(is.data.frame(b)) b = Matrix(as.matrix(b), sparse = TRUE)
    bn = if(is.null(dim(b))) length(b) else dim(b)[1]
    if(lag && abs(lag) >= bn) lag = if(lag < 0) -bn + 1 else bn - 1
    res = if((is.null(dim(a)) || any(dim(a) == 1)) && (length(a) == bn)){
      b = as.numeric(b)
      if(lag) b = if(lag < 0) c(b[-seq_len(-lag)], numeric(-lag)) else c(numeric(lag), b)[seq_len(bn)]
      vector_similarity(as.numeric(a), b, met$dummy)
    }else{
      if(is.null(dim(a))) a = Matrix(a, 1, dimnames = list(NULL, names(a)), sparse = TRUE)
      if(!all(slots %in% slotNames(a))) a = as(a, 'dgCMatrix')
      if(is.null(dim(b))) b = Matrix(b, 1, dimnames = list(NULL, names(b)), sparse = TRUE)
      if(!all(slots %in% slotNames(b))) b = as(b, 'dgCMatrix')
      d = c(dim(a), dim(b))
      if(d[2] != d[4]){
        ns = colnames(a)
        if(!is.null(ns)){
          ns = ns[ns %in% colnames(b)]
          if(length(ns)){
            a = a[, ns, drop = FALSE]
            b = b[, ns, drop = FALSE]
          }
        }
        d = c(dim(a), dim(b))
        if(d[2] != d[4])
          stop('a and b have a different number of columns, which could not be aligned by name')
      }
      if(lag){
        b = if(lag > 0) rbind(Matrix(0, lag, d[4], sparse = TRUE), b[-(seq_len(lag) + d[3] - lag),]) else
          rbind(b[-seq_len(-lag),], Matrix(0, -lag, d[4], sparse = TRUE))
      }
      calculate_similarities(a, b, if(((missing(pairwise) || !pairwise) && d[1] == d[3]) ||
          d[3] == 1) 1 else 3, met$dummy)
    }
  }
  if('list' %in% class(res)){
    pairwise = 'dtCMatrix' %in% class(res[[1]])
    if((pairwise && symmetrical) || mean) for(i in seq_along(res)){
      if(pairwise && (symmetrical || mean)) res[[i]] = forceSymmetric(res[[i]], 'L')
      if(mean) res[[i]] = if(is.null(nrow(res[[i]]))) mean(res[[i]], na.rm = TRUE) else rowMeans(res[[i]], TRUE)
    }
    if(is.null(dim(res[[1]]))){
      rn = if(!is.na(nd <- which(d == length(res[[1]]))[1]) && !is.null(rownames(if(nd == 1) a else b)))
        rownames(if(nd == 1) a else b) else NULL
      if(length(met$selected) == 1){
        if(length(rn) == length(res[[1]])) names(res[[1]]) = rn
      }else{
        attr(res, 'row.names') = if(length(rn) == length(res[[1]])) rn else seq_along(res[[1]])
        attr(res, 'class') = 'data.frame'
      }
    }
    if(!return.list && length(met$selected) == 1) res = res[[1]]
  }
  attr(res, 'time') = c(simets = proc.time()[[3]] - st)
  res
}
