#' Read/write LIWC dictionary files
#'
#' Read in or write Linguistic Inquiry and Word Count dictionary (.dic) files.
#' @param path Path to a .dic file.
#' @param cats A character vector of category names to be returned. All categories are returned by default.
#' @export

read.dic=function(path,cats){
  di=tryCatch(
    readLines(if(missing(path))file.choose() else path,warn=FALSE)
    ,error=function(e)stop('failed to read path: ',e$message,call.=FALSE)
  )
  lst=grep('%',di)
  if(length(lst)>1){
    di=di[-seq_len(lst[1])]
    lst=lst[2]-2
  }else stop('file is not in the expected format')
  ci=lapply(di[seq_len(lst)],function(l)strsplit(l,'[ \t][^A-z]*')[[1]])
  names(ci)=vapply(ci,'[[','',2)
  if(missing(cats)) cats=names(ci)
  ci=lapply(ci[names(ci)%in%cats],'[[',1)
  if(ckp<-any(grepl('(',di,fixed=TRUE))){
    di=gsub(' +\\(','_(',di)
    di=gsub('\\) +',')_',di)
  }
  di=strsplit(di[seq_along(di)[-(1:3)]],'[ \t][^0-9]*')
  di=di[vapply(di,length,0)>1]
  names(di)=vapply(di,'[','',1)
  di=lapply(di,'[',-1)
  if(ckp) names(di)=gsub('_(?=\\()|(?<=\\))_',' ',names(di),perl=TRUE)
  wl=list()
  for(w in names(di)){
    ck=ci%in%di[[w]]
    if(any(ck)){
      cm=names(ci[ck])
      for(c in cm) wl[[c]]=c(wl[[c]],w)
    }
  }
  wl
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
#' # save it as a .dic file: write.dic(dict,'murder')
#' # read it back in as a list: read.dic('murder.dic')
#'
#' # read in the Moral Foundations or LUSI dictionaries from urls
#' moral_dict = read.dic('http://bit.ly/MoralFoundations')
#' lusi_dict = read.dic('http://bit.ly/lusi_dict')
#'
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
#' Read in and optionaly segment all texts within a folder.
#'
#' @param path Path to a folder containing files, or a vector of paths to files.
#' @param segment Specifies how the text of each file should be segmented. If a number, texts will be broken into
#'   that many segments, each with a roughly equal number of words. If a character, texts will be broken at that character;
#'   for example, a string matching \code{join} will split texts on returns.
#' @param subdir If \code{TRUE} files in folders in \code{path} will also be included.
#' @param ext The extention of the files you want to read in. '.txt' by default.
#' @param fixed If \code{FALSE}, and \code{segment} is a character, \code{segment} will be treated as a regular expression.
#' @param segment.size If specified, \code{segment} will be ignored, and texts will be broken into segments countaining roughtly
#'   \code{segment.size} number of words.
#' @param bysentence If \code{TRUE}, and \code{segment} is a number or \code{wordcount} is specified, sentences will be kept
#'   together, rather than being broken across segments.
#' @param reader The function used to read files. File paths are always passed as the first argument.
#'   \code{\link[base]{readLines}} by default.
#' @param readarg A list of additional arguments to pass to \code{reader}, starting in the second position.
#'
#' @export

read.folder=function(path=NULL,segment=NULL,subdir=FALSE,ext='.txt',fixed=TRUE,
  segment.size=NULL,bysentence=FALSE,reader=readLines,readarg=list(warn=FALSE)){
  if(missing(path)){
    path=choose.dir()
    if(is.na(path)) return()
  }
  fs=if(length(path)==1 && dir.exists(path)) fs=list.files(path,ext,recursive=subdir,full.names=TRUE) else path
  fs=data.frame(rbind(fs,gsub('^.*[\\/]+','',fs)),stringsAsFactors=FALSE)
  if(!missing(segment.size)) segment=NULL
  d=do.call(rbind,lapply(fs,function(f){
    txt=tryCatch(do.call(reader,c(f[1],readarg)),error=function(e)NULL)
    if(!is.null(txt)){
      txt=paste(txt,collapse='\r\n')
      if(is.character(segment)){
        txt=strsplit(txt,segment,fixed=fixed)[[1]]
      }else if(is.numeric(segment) || !is.null(segment.size)){
        txt=if(bysentence){
          txt=gsub('(?<=st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof)\\.','__PERIOD__',txt,TRUE,TRUE)
          txt=strsplit(gsub('(?<=[.?!][ ")}\\]])','__BREAK__',txt,perl=TRUE),'__BREAK__',fixed=TRUE)[[1]]
          lapply(txt,function(t)strsplit(sub('__PERIOD__','.',t,fixed=TRUE),' [^A-z0-9]+ | +|\r\n',perl=TRUE)[[1]])
        }else strsplit(txt,' [^A-z0-9]+ | +|\r\n',perl=TRUE)[[1]]
        ns=length(txt)
        sls=vapply(txt,function(s)sum(s!=''),0)
        wc=sum(sls)
        segment=if(is.null(segment.size)) round(wc/segment+.49) else segment.size
        op=c()
        cl=s=0
        ind=1
        for(i in seq_len(ns)) if(i<ns && cl+sls[i]<segment) cl=cl+sls[i] else{
          cl=0
          op[ind]=paste(unlist(txt[(s+1):i]),collapse=' ')
          s=i
          ind=ind+1
        }
        txt=op[!grepl('^[ ]*$',op)]
      }
      data.frame(file=f[2],segment=seq_along(txt),text=txt)
    }
  }))
  rownames(d)=seq_len(nrow(d))
  d
}

#' Download Latent Semantic Spaces
#'
#' Downloads the specified semantic space.
#'
#' Spaces are slightly altered and reprocessed versions of those available at
#' \url{http://www.lingexp.uni-tuebingen.de/z2/LSAspaces/}.
#'
#' Spaces can (sometimes) be downloaded directly from \url{https://www.myweb.ttu.edu/miserman/lsspaces/}.
#' @param space name of the space you wish to download. Options include \code{'default'} and \code{'tasa'}.
#' @param dir desired path to the downloaded space; default is '~/Documents/Latent Semantic Spaces'.
#' @export
#' @importFrom utils download.file unzip

download.lsspace=function(space='default',dir='~/Documents/Latent Semantic Spaces'){
  z=grepl('^l|^s|^z',space,TRUE)
  if(!grepl('.',space,fixed=TRUE)) space=paste0(space,'.zip') else if(grepl('.sqlite',space,fixed=TRUE))
    space=sub('.sqlite','.zip',space,fixed=TRUE)
  space=match.arg(space,c('default.zip','tasa.zip'))
  if(!dir.exists(dir)) dir.create(dir) else if(space%in%list.files(dir)){
    if(z && !(uzf<-sub('.zip','.sqlite',space,fixed=TRUE))%in%list.files(dir)){
      unzip(paste0(dir,'/',space),exdir=dir)
      return(message(space,' uncompressed as ',uzf,' to ',dir))
    }else stop(space,' already exists in ',dir)
  }
  td=try(download.file(url<-paste0('https://www.myweb.ttu.edu/miserman/lsspaces/',space),paste0(dir,'/',space)),TRUE)
  if(inherits(td,'try-error')) stop(if(grepl('Permission',td)) 'R does not have permission to save the file;' else
    sub('^.* : ','',td),'\ndownload it directly from ',url,call.=FALSE)
  if(z) unzip(paste0(dir,'/',space),exdir=dir)
  message(space,' downloaded to ',dir)
}

#' Categorize raw texts using a pattern-based dictionary
#'
#' @param text A vector of raw text to be categorized.
#' @param dict At least a vector of terms (patterns), usually a matrix-like object with columns for terms,
#'   categories, and weights.
#' @param term,category,weight Strings specifying the relevant column names in \code{dict}.
#' @param to.lower Logical indicating whether \code{text} should be convered to lower case.
#' @param to.percent Logical indicating whether term-counts should be devided by document-counts before
#'   being weighted.
#' @param bias A constant to add to each category after weighting and summing. Can be a vector with names
#'   corresponding to the unique values in \code{dict[,category]}, but is usually extracted from dict based
#'   on the intercept included in each category (defined by \code{intname}).
#' @param intname The term representing the intercept (bias) of a category, to be extracted from \code{dict}
#'   and used as \code{bias}.
#' @param dtm Logical; if \code{TRUE}, only a document-term matrix will be returned, rather than the
#'   weighted, summed, and adjusted category value.
#' @examples
#' # example text
#' text = c(
#'   "Oh, what youth was! What I had and gave away. What I took and spent and saw. What I lost. And now? Ruin.",
#'   "God, are you so bored?! You just want what's gone from us all? I miss the you that was too. I love that you.",
#'   "Tomorrow! Tomorrow--nay, even tonight--you wait, as I am about to change. Soon I will off to revert. Please wait."
#' )
#'
#' # read in the temporal orientation lexicon from the World Well-Being Project
#' tempori = read.csv('https://wwbp.org/downloads/public_data/temporalOrientationLexicon.csv')
#'
#' lma_patcat(text,tempori)
#'
#' @export

lma_patcat=function(text,dict,term='term',category='category',weight='weight',
  to.lower=TRUE,to.percent=TRUE,bias=NULL,intname='_intercept',dtm=FALSE){
  text=as.character(text)
  if(to.lower) text=tolower(text)
  if(is.null(colnames(dict))){
    if(is.list(dict)) dict=data.frame(
      term=unlist(dict,use.names=FALSE),
      category=unlist(lapply(names(dict),function(n)rep(n,length(dict[[n]]))))
    ) else dict=data.frame(term=dict)
    term='term'
    category='category'
  }
  if(!weight%in%names(dict)) dict[,weight]=1
  if(any(bs<-dict[,term]==intname)){
    bias=dict[bs,,drop=FALSE]
    bias=if(sum(bs)!=1 && category%in%names(bias)){
      rownames(bias)=bias[,category]
      t(bias[,weight,drop=FALSE])[1,]
    }else bias[1,weight]
    dict=dict[!bs,]
  }
  r=vapply(as.character(dict[,term]),function(p)
    vapply(strsplit(text,p,fixed=TRUE),length,0)-1,numeric(length(text)))
  if(to.percent){
    rs=rowSums(r)
    if(any(rs!=0)) r[rs>0,]=r[rs>0,]/rs[rs>0]
  }
  r=t(r)
  if(dtm) return(r)
  r=r*dict[,weight]
  r=if(category%in%names(dict))
    do.call(rbind,lapply(split(as.data.frame(r),dict[,category]),colSums)) else colSums(r)
  if(!is.null(bias)) if(length(bias)==1) r=r+bias else if(!is.null(rownames(r))){
    r[names(bias),]=r[names(bias),]+bias
    r=t(r)
  }
  r
}

#' English function word category lists
#'
#' Returns a list of function words based on the Linguistic Inquiry and Word Count 2015 dictionary.
#' @param ... Numbers or letters corresponding to category names: ppron, ipron, article,
#' adverb, conj, prep, auxverb, negate, quant, interrog, number, or special.
#' @param as.regex Logical: if \code{FALSE}, lists are returned without regular expression.
#' @note
#' The \code{special} category is not returned unless specifically requested. It is a list of regular expression
#' strings attempting to capture special things like ellipses and emojis. If \code{special} is part of the returned list,
#' \code{as.regex} is set to \code{TRUE}.
#'
#' The \code{special} list is always used by both \code{\link{lma_dtm}} and \code{\link{lma_termcat}}. When creating a dtm,
#' \code{special} is used to clean the original imput (so that, by default, the punctuation involved in ellipses and emojis
#' are treated as different -- as ellipses and emojis rather than as periods and parens and colons and such). When categorizing
#' a dtm, the input dictionary is passed by the special lists to be sure the terms in the dtm match up with the dictionary
#' (so, for example, ": (" would be replaced with "FROWN" in both the text and dictionary).
#' @examples
#' #return the full dictionary (excluding special)
#' lma_dict()
#'
#' #return the standard 7 category lsm categories
#' lma_dict(1:7)
#'
#' #return just a few categories without regular expression
#' lma_dict(neg, int, aux, as.regex=FALSE)
#'
#' #return special specifically
#' lma_dict(special)
#' @export

lma_dict=function(...,as.regex=TRUE){
  cats=as.character(substitute(list(...)))[-1]
  dict=list(
    ppron=c("^he$","^he'd$","^he's$","^her$","^hers$","^herself$","^hes$","^him$","^himself$","^his$","^hissel","^i$","^ic$","^i'd$",
      "^i'd've$","^i'll$","^i'm$","^i've$","^id$","^idc$","^idgaf$","^idk$","^idontknow$","^idve$","^iirc$","^ikr$","^ily","^im$","^ima$",
      "^imean$","^imma$","^ive$","^let's$","^lets$","^me$","^methinks$","^mine$","^my$","^myself$","^oic$","^oneself$","^our$","^ours$",
      "^ourselves$","^she$","^she'd$","^she'll$","^she's$","^shes$","^thee$","^their","^them$","^themself$","^themselves$","^they$",
      "^they'd$","^they'll$","^they've$","^theyd$","^theyll$","^theyve$","^thine$","^thou$","^thoust$","^thy$","^thyself$","^u$","^ur$",
      "^us$","^we$","^we'd$","^we'll$","^we're$","^we've$","^weve$","^y'all$","^y'all's$","^ya$","^ya'll","^yall$","^yalls$","^ye$","^yinz",
      "^you$","^you'd$","^you'll$","^you're$","^you've$","^youd$","^youll$","^your$","^youre$","^yours$","^yourself$","^yourselves$","^youve$"),
    ipron=c("^another$","^anybod","^anymore$","^anyone","^anything$","^deez$","^everybod","^everyday$","^everyone",
      "^everything","^it$","^it'd$","^it'll$","^it's$","^itd$","^itll$","^its$","^itself$","^nobod","^other$","^others$","^somebod",
      "^someone","^something","^stuff$","^that$","^that'd$","^that'll$","^that's$","^thatd$","^thatll$","^thats$","^these$",
      "^thing","^this$","^those$","^what$","^what'd$","^what'll$","^what's$","^whatd$","^whatever$","^whatll$","^whats$","^which$",
      "^whichever$","^who$","^who'd$","^who'll$","^who's$","^whod$","^whoever$","^wholl$","^whom$","^whomever$","^whos$","^whose$",
      "^whosever$","^whoso"),
    article=c("^a$","^an$","^the$"),
    adverb=c("^about$","^absolutely$","^actually$","^again$","^almost$","^already$","^also$","^anyway","^anywhere$","^apparently$",
      "^around$","^awhile$","^back$","^barely$","^basically$","^beyond$","^briefly$","^clearly$","^commonly$","^completely$",
      "^constantly$","^continually$","^definitely$","^especially$","^essentially$","^even$","^eventually$","^ever$",
      "^everywhere","^exclusively$","^extremely$","^finally$","^fortunately$","^frequently$","^fully$","^generally$","^hardly$",
      "^hence$","^henceforth$","^here$","^here's$","^herein$","^heres$","^hereto","^hopefully$","^how$","^how'd$","^how're$","^how's$",
      "^howd$","^however$","^howre$","^hows$","^immediately$","^indeed$","^instead$","^jus$","^just$","^juz$","^lately$","^maybe$",
      "^meanwhile$","^mostly$","^namely$","^nearly$","^never$","^nevertheless$","^nonetheless$","^notwithstanding$","^now$",
      "^often$","^only$","^originally$","^particularly$","^perhaps$","^practically$","^presently$","^primarily$","^principally$",
      "^probab","^prolly$","^rarely$","^rather$","^really$","^regularly$","^relatively$","^respectively$","^seldomly$",
      "^seriously$","^shortly$","^simply$","^so$","^somehow$","^somewhat$","^somewhere$","^soon$","^sooo","^specifically$","^still$",
      "^subsequently$","^such$","^suddenly$","^supposedly$","^there$","^there's$","^thereafter$","^therefor","^theres$","^tho$",
      "^tho'","^though$","^thus","^too$","^totally$","^truly$","^typically$","^ultimately$","^uncommonly$","^usually$","^vastly$",
      "^very$","^virtually$","^well$","^when$","^when'","^whence$","^whenever$","^where$","^where'd$","^whereby$","^wherefore$",
      "^wherein$","^whereof$","^wherever$","^whither$","^wholly$","^why$","^why'","^whyever$","^yet$"),
    conj=c("^also$","^altho$","^although$","^and$","^as$","^bc$","^because$","^but$","^cos$","^coz$","^cuz$","^how$","^how'd$","^how're$",
      "^how's$","^howd$","^however$","^howre$","^hows$","^if$","^nevertheless$","^nor$","^or$","^otherwise$","^plus$","^so$","^then$",
      "^tho$","^tho'","^though$","^til$","^till$","^unless$","^until$","^when$","^when'","^whenever$","^whereas$","^wherefore$",
      "^wherever$","^whether$","^while$","^whilst$"),
    prep=c("^about$","^above$","^abt$","^across$","^after$","^against$","^ahead$","^along$","^amid$","^amidst$","^among","^around$",
      "^as$","^at$","^atop$","^away$","^before$","^behind$","^below$","^beneath$","^beside$","^besides$","^between$","^beyond$","^by$",
      "^despite$","^down$","^during$","^except$","^excluding$","^for$","^from$","^hereafter$","^in$","^including$","^inside$",
      "^insides$","^into$","^like$","^minus$","^near$","^of$","^off$","^on$","^onto$","^out$","^outside$","^over$","^plus$","^regarding$",
      "^respecting$","^sans$","^since$","^than$","^through","^thru$","^til$","^till$","^to$","^toward","^under$","^underneath$",
      "^unless$","^unlike$","^until$","^unto$","^up$","^upon$","^versus$","^via$","^vs$","^with$","^within$","^without$"),
    auxverb=c("^ain't$","^aint$","^am$","^are$","^aren't$","^arent$","^be$","^become$","^becomes$","^becoming$","^been$","^being$","^can$",
      "^cannot$","^could$","^could've$","^couldn't$","^couldnt$","^couldve$","^did$","^didn't$","^didnt$","^do$","^does$","^doesn't$",
      "^doesnt$","^doing$","^don't$","^done$","^dont$","^gunna$","^had$","^hadn't$","^hadnt$","^has$","^hasn't$","^hasnt$","^have$",
      "^haven't$","^havent$","^having$","^he'd$","^he's$","^hes$","^i'd$","^i'll$","^i'm$","^i've$","^id$","^im$","^is$","^isn't$","^isnt$",
      "^it'd$","^it'll$","^it's$","^itd$","^itll$","^ive$","^let$","^may$","^might$","^might've$","^mightve$","^must$","^must'nt$",
      "^must've$","^mustn't$","^mustnt$","^mustve$","^ought$","^ought'nt$","^ought've$","^oughta$","^oughtn't$","^oughtnt$",
      "^oughtve$","^shall$","^shan't$","^shant$","^she'd$","^she'll$","^she's$","^shes$","^should$","^should'nt$","^should've$",
      "^shouldn't$","^shouldnt$","^shouldve$","^that'd$","^that'll$","^that's$","^thatd$","^thatll$","^thats$","^there's$","^theres$",
      "^they'd$","^they'll$","^they're$","^they've$","^theyd$","^theyll$","^theyre$","^theyve$","^tryna$","^unable$","^wanna$","^was$",
      "^wasn't$","^wasnt$","^we'd$","^we'll$","^we've$","^were$","^weren't$","^werent$","^weve$","^what's$","^whats$","^who'd$","^who'll$",
      "^whod$","^wholl$","^will$","^won't$","^wont$","^would$","^would've$","^wouldn't$","^wouldnt$","^wouldve$","^you'd$","^you'll$",
      "^you're$","^you've$","^youd$","^youll$","^youre$","^youve$"),
    negate=c("^ain't$","^aint$","^aren't$","^arent$","^can't$","^cannot$","^cant$","^couldn't$","^couldnt$","^didn't$","^didnt$",
      "^doesn't$","^doesnt$","^don't$","^dont$","^hadn't$","^hadnt$","^hasn't$","^hasnt$","^haven't$","^havent$","^idk$","^isn't$",
      "^isnt$","^must'nt$","^mustn't$","^mustnt$","^nah","^need'nt$","^needn't$","^neednt$","^negat","^neither$","^never$","^no$",
      "^nobod","^noes$","^none$","^nope$","^nor$","^not$","^nothing$","^nowhere$","^np$","^ought'nt$","^oughtn't$","^oughtnt$",
      "^shan't$","^shant$","^should'nt$","^shouldn't$","^shouldnt$","^uh-uh$","^wasn't$","^wasnt$","^weren't$","^werent$","^without$",
      "^won't$","^wont$","^wouldn't$","^wouldnt$"),
    quant=c("^add$","^added$","^adding$","^adds$","^all$","^allot$","^alot$","^amount$","^amounts$","^another$","^any$","^approximat",
      "^average$","^bit$","^bits$","^both$","^bunch$","^chapter$","^couple$","^doubl","^each$","^either$","^entire","^equal",
      "^every$","^extra$","^few$","^fewer$","^fewest$","^group","^inequal","^least$","^less$","^lot$","^lotof$","^lots$","^lotsa$",
      "^lotta$","^majority$","^many$","^mo$","^mo'","^more$","^most$","^much$","^mucho$","^multiple$","^nada$","^none$","^part$","^partly$",
      "^percent","^piece$","^pieces$","^plenty$","^remaining$","^sampl","^scarce$","^scarcer$","^scarcest$","^section$",
      "^segment","^series$","^several","^single$","^singles$","^singly$","^some$","^somewhat$","^ton$","^tons$","^total$",
      "^triple","^tripling$","^variety$","^various$","^whole$"),
    interrog=c("^how$","^how'd$","^how're$","^how's$","^howd$","^howre$","^hows$","^wat$","^wattt","^what$","^what'd$","^what'll$",
      "^what're$","^what's$","^whatd$","^whatever$","^whatll$","^whatre$","^whatt","^when$","^when'","^whence$","^whenever$",
      "^where$","^where'd$","^where's$","^wherefore$","^wherever$","^whether$","^which$","^whichever$","^whither$","^who$","^who'd$",
      "^who'll$","^who's$","^whoever$","^wholl$","^whom$","^whomever$","^whos$","^whose$","^whosever$","^whoso","^why$","^why'",
      "^whyever$","^wut$"),
    number=c("^billion","^doubl","^dozen","^eight","^eleven$","^fift","^first$","^firstly$","^firsts$","^five$","^four",
      "^half$","^hundred","^infinit","^million","^nine","^once$","^one$","^quarter","^second$","^seven","^single$","^six",
      "^ten$","^tenth$","^third$","^thirt","^thousand","^three$","^trillion","^twel","^twent","^twice$","^two$","^zero$",
      "^zillion"),
    special=list(
      ELLIPSIS='\\.{3,}|\\. +\\. +[. ]+',
      SMILE='[([{q][ -<.,]+[;:8]|[([{q][;:8]|[;:8][ ->.,][p3)}]|[:;8][ ->.,]\\]|[:;8][p3)}]|[:;8]\\]',
      FROWN='\\][ -<.,]+[;:8]|[)}/\\>][ -<.,]+[;:8]|\\][:;8]|[)}/\\>][;:8]|[;:8][ ->.,][([{/\\<]|[:;8][([{/\\<]',
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
    dict[cats]
  }else lapply(dict[cats],function(l)gsub('\\^|\\$','',sub('(?<=[^$])$','*',l,perl=TRUE)))
}
