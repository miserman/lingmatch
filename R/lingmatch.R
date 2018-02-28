#' Linguistic Matching and Accommodation
#'
#' Offers a variety of methods to assess linguistic matching or accommodation, where \emph{matching}
#' is general similarity (sometimes called \emph{homophily}), and \emph{accomodation} is some form
#' of conditional similarity (accounting for some base rate or precedent; sometimes called
#' \emph{alignment}).
#'
#' There are a great many points of decision in the assessment of linguistic similarity and/or
#' accomodation, partly inherited from the great many point of decision inherent in the numerical
#' representation of language. Two general types of matching are implemented here as sets of
#' defaults: Language/Linguistic Style Matching (LSM; Niederhoffer & Pennebaker, 2002; Ireland &
#' Pennebaker, 2010), and Latent Semantic Analysis/Similarity (LSA; Landauer & Dumais, 1997;
#' Babcock, Ta, & Ickes, 2014). See the \code{type} argument for specifics.
#'
#' @param x Texts to be compared; a vector, document-term matrix (dtm; with terms as column names),
#'   or path to a file (.txt or .csv, with texts separated by one or more lines/rows).
#' @param comp Defines the comparison to be made:
#' \itemize{
#'   \item If a function, this will be applied to \code{x} within each group (overall if there is
#'     no group; i.e., \code{apply(x,2,comp)}; e.g., \code{comp = mean} would compare each text to
#'     the mean profile of its group.)
#'   \item If a character with a length of 1 and no spaces, if it partially matches one of
#'     \code{lsm_profiles}'s rownames, that row will be used as the comparison; if it partially
#'     matches \code{'auto'}, the highest correlating \code{lsm_profiles} row will be used; if it
#'     partially matches \code{'pairwise'}, each text will be compared to one another; if it
#'     partially matches \code{'sequential'}, the last variable in \code{group} will be treated as
#'     a speaker ID (see the grouping and comparisons section).
#'   \item If a character vector, this will be processed in the same way as \code{x}.
#'   \item If a vector, either of the same length as \code{x} has rows and logical or factor-like
#'     (having  n levels < length), or a numeric range or logical of length less than \code{nrow(x)}
#'     , this will be used to select a subset of
#'     \code{x} (e.g., \code{comp = 1:10} would treat the first 10 rows of x as the comparison;
#'     \code{comp = type=='prompt'} would make a logical vector identifying prompts, assuming
#'     "type" was the name of a column in \code{data}, or a variable in the global environment,
#'     and the value "prompt" marked the prompts).
#'   \item If a matrix-like object (having multiple rows and columns), this will be treated as a
#'     sort of dtm, assuming there are common column names between \code{x} and \code{comp} (e.g.,
#'     if you had prompt and response texts that were already processed separately).
#' }
#' @param data A matrix-like object as a reference for column names, if variables are refereed to in
#'   other arguments (e.g., \code{lingmatch(text, data=data)} would be the same as
#'   \code{lingmatch(data$text)}.
#' @param group A logical or factor-like vector the same length as \code{nrow(x)}, used to defined
#'   groups.
#' @param ... Passes arguments to \code{\link{lma_dtm}}, \code{\link{lma_weight}},
#'   \code{\link{lma_lspace}}, and/or \code{\link{lma_termcat}}, depending on \code{x} and
#'   \code{comp}.
#' @param comp.data A matrix-like object as a reference to \code{comp} variables.
#' @param comp.group The Column name of the grouping variable(s) in \code{comp.data}; if
#'   \code{group} contains references to column names, and \code{comp.group} is not specified,
#'   \code{group} variables will be looked for in \code{comp.data}.
#' @param order A numeric vector the same length as \code{nrow(x)} indicating the order of the
#'   texts and grouping variables if the type of comparison is sequential. Only necessary if the
#'   texts are not already ordered as desired.
#' @param drop logical; if \code{FALSE}, columns with a sum of 0 are retained.
#' @param all.levels logical; if \code{FALSE}, multiple groups are combined. See the Grouping and
#'   Comparisons section.
#' @param type A character at least partially matching 'lsm' or 'lsa'; applies default settings
#'   alighning with the standard calculations of each type:
#'   \tabular{ll}{
#'     LSM \tab \code{lingmatch(text, weight='count', dict=lma_dict(1:9), metric='canberra')}\cr
#'     LSA \tab \code{lingmatch(text, weight='tfidf', space='internal', metric='cosine')}\cr
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
#'     to apply to both \code{x} and \code{comp} (either both in \code{data}, or separately
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
#'   essays, correspondence, and poetry. \emph{Journal of personality and social psychology, 99},
#'   549.
#'
#' Landauer, T. K., & Dumais, S. T. (1997). A solution to Plato's problem: The latent semantic
#'   analysis theory of acquisition, induction, and representation of knowledge.
#'   \emph{Psychological review, 104}, 211.
#'
#' Niederhoffer, K. G., & Pennebaker, J. W. (2002). Linguistic style matching in social interaction.
#'   \emph{Journal of Language and Social Psychology, 21}, 337-360.
#'
#' @export
#' @importFrom Matrix Matrix as.matrix
#' @importFrom stats na.omit cor dpois

lingmatch=function(x,comp=mean,data=NULL,group=NULL,...,comp.data=NULL,comp.group=NULL,order=NULL,
  drop=TRUE,all.levels=TRUE,type='lsm'){
  inp=as.list(substitute(list(...)))[-1]
  #setting up a default type if specified
  if(!missing(type) && !is.null(type)){
    type=if(grepl('lsm|lang|ling|style|match',type,TRUE)) 'lsm' else 'lsa'
    ni=names(inp)
    if(type=='lsm' && !'dict'%in%ni) inp$dict=lma_dict(1:9)
    if(!'weight'%in%ni) inp$weight=if(type=='lsm'){inp$percent=TRUE;'count'}else c('count','idf')
    if(!'metric'%in%ni) inp$metric=if(type=='lsm') 'canberra' else 'cosine'
    if(type=='lsa' && !'space'%in%ni) inp$space='default'
  }
  mets=c('euclidean','canberra','cosine','pearson','spearman','kendall','jaccard','kld')
  inp$metric=if(!is.null(inp$metric)) match.arg(as.character(inp$metric),mets,TRUE) else 'cosine'
  vs=c('x','comp','group','order','data','comp.data','comp.group')
  opt=as.list(match.call(expand.dots=FALSE))[vs]
  names(opt)=vs
  # organizing options for preprocessing
  dsp=lapply(c('lma_dtm','lma_weight','lma_lspace','lma_termcat','lma_simets'),function(f){
    a=names(as.list(args(f)))
    a=a[2:(length(a)-1)]
    inp[a[a%in%names(inp)]]
  })
  names(dsp)=c('p','w','m','c','s')
  # fetches input from data or environment
  gv=function(a,data=NULL){
    ta=a
    if(is.character(ta)){
      if(!is.null(data) && ta%in%colnames(data)) return(unlist(data[,ta])) else
      if(length(ta)==1 || !any(grepl(' ',ta,fixed=TRUE))) ta=parse(text=a)
    }
    ta=tryCatch(eval(ta,data,parent.frame(2)),error=function(e)NULL)
    if(length(ta)==0) ta=tryCatch(eval(a,globalenv()),error=function(e)NULL)
    if(is.null(ta)) ta=tryCatch(eval(ta,data),error=function(e)NULL)
    if(is.null(ta)) stop('could not find ',a,call.=FALSE)
    ta
  }
  gd=function(a,data=NULL){
    r=if(is.character(a) && length(a)==1 && grepl('\\.txt$|\\.csv$',a,TRUE)){
      r=readLines(a,warn=FALSE)
      r[r!='']
    }else if(is.character(a)) a else gv(a,data)
    if(is.factor(r)) r=as.character(r)
    r
  }
  # weight, map, and/or categorize
  wmc=function(a){
    if(length(dsp$w)!=0) a=do.call(lma_weight,c(list(a),dsp$w))
    if(length(dsp$m)!=0) a=do.call(lma_lspace,c(list(a),dsp$m))
    if(length(dsp$c)!=0) a=do.call(lma_termcat,c(list(a),dsp$c))
    a
  }
  # initial data parsing
  # x
  if(missing(x)) opt$x=file.choose()
  if(!missing(data)) x=gd(opt$x,data)
  rx=NROW(x)
  cx=NCOL(x)
  # comp
  if(!missing(comp)){
    comp=gd(opt$comp,if(missing(comp.data)) data else comp.data)
    if(is.logical(comp)) comp=which(comp)
    if(missing(comp.data) && !is.null(colnames(comp))) comp.data=comp
  }else opt$comp='mean'
  if(is.factor(x)) x=as.character(x)
  if(is.factor(comp)) comp=as.character(comp) else if(is.data.frame(comp))
    comp=comp[,!vapply(comp,function(c)is.factor(c)||is.character(c),TRUE)]
  do.wmc=TRUE
  if(class(x)%in%c('matrix','data.frame') && is.null(attr(x,'Type'))){
    dn=if('dict'%in%names(inp)) eval(inp$dict) else names(lma_dict(1:9))
    if(is.list(dn)) dn=names(dn)
    cn=colnames(x)
    if(any(!(ck<-dn%in%cn))){
      if('preps'%in%cn) colnames(x)=sub('preps','prep',cn,fixed=TRUE)
      if('articles'%in%cn) colnames(x)=sub('articles','article',cn,fixed=TRUE)
      ck=dn%in%colnames(x)
    }
    if(sum(ck)>6){
      if(missing(data)) data=x
      if(any(!ck)) dn=dn[ck]
      x=x[,dn]
      do.wmc=FALSE
      if(!missing(comp)){
        if(class(comp)%in%c('matrix','data.frame')){
          if(missing(group) && missing(comp.group)){
            comp=mean
            opt[c('comp','comp.data')]=c('mean',opt$comp)
          }else{
            if(all(dn%in%colnames(comp))) comp=comp[,dn]
          }
        }else{
          if(is.character(comp) && (length(comp)>1 || grepl(' ',comp,fixed=TRUE)))
            comp=wmc(do.call(lma_dtm,c(list(comp),dsp$p)))
        }
      }
    }
  }
  # if x looks like text, seeing if other text can be added, then converting to a dtm
  if(!is.matrix(x) && (is.character(x) || is.factor(x))){
    if(is.character(comp) && (length(comp)>1 ||  grepl(' ',comp,fixed=TRUE))){
      x=c(comp,x)
      comp=seq_along(comp)
      opt$comp='text'
    }
    x=do.call(lma_dtm,c(list(x),dsp$p))
  }
  if(is.data.frame(comp)) comp=as.matrix(comp)
  cc=if(is.numeric(comp)) 1 else if(is.character(comp)){comp=tolower(comp);2}else 0
  # group and order
  if(!missing(group)) group=if(length(opt$group)>1 && !grepl('\\$|\\[',as.character(opt$group[1])))
    lapply(as.character(opt$group)[-1],gv,data) else{
      group=gv(opt$group,data)
      if(is.factor(group)) group=as.character(group) else if(is.matrix(group)) group=as.data.frame(group,row.names=FALSE)
      if(is.null(ncol(group))) list(group) else lapply(group,as.character)
    }
  if(!missing(comp.group) || !missing(comp.data)){
    cg=opt[[if(missing(comp.group)) 'group' else 'comp.group']]
    if(!is.null(cg)){
      cg=if(length(cg)>1 && !grepl('\\$|\\[',as.character(cg[1]))) lapply(as.character(cg[-1]),gv,
        comp.data) else list(gv(cg,comp.data))
      if(cc!=1) if(NROW(comp)!=length(cg[[1]]) || NROW(x)!=length(group[[1]]))
        stop('data and comp.data mismatch',call.=FALSE)
      comp.group=do.call(paste,cg)
      if(!is.null(comp.data)) rownames(comp.data)=comp.group
      if(length(group)>1){
        group=do.call(paste,group)
        if(!is.null(comp.data) && any(ck<-!(ckg<-unique(group))%in%unique(comp.group))) if(all(ck))
          stop('group and comp.group had no levels in common') else{
            warning('levels not found in comp.group: ',paste(ckg[ck],collapse=', '),call.=FALSE)
            group=group[ck<-group%in%ckg[!ck]]
            x=x[ck,,drop=FALSE]
          }
      }
    }
  }
  if(!missing(group)) if(length(if(is.list(group)) group[[1]] else group)!=rx) stop('length(group) != nrow(x)')
  if(!missing(order)){
    order=as.character(gv(opt$order,data))
    if(!is.null(order)) if(length(order)==rx){
      x=x[order,]
      group=lapply(group,'[',order)
    }else warning('length(order) != nrow(x), so order was not applied') else
      warning('failed to apply order')
  }
  if(cc==2 && (length(comp)>1 || any(grepl(' ',comp,fixed=TRUE)))){
    comp=do.call(lma_dtm,c(list(comp),dsp$p))
    cc=1
  }
  # if comp appears to be a dtm, unifying x and comp
  if(cc==1 && !is.null(names(comp))) comp=t(as.matrix(comp))
  cr=nrow(comp)
  cn=colnames(comp)
  if(cc==1 && !is.null(cr) && !is.null(cn) && any(vapply(dsp[-1],length,0)>0)){
    nn=cn[!cn%in%colnames(x)]
    if(length(nn)!=0) x=cbind(x,matrix(0,nrow(x),length(nn),dimnames=list(c(),nn)))
    x=rbind(matrix(0,cr,ncol(x),dimnames=list(c(),colnames(x))),x)
    x[seq_len(cr),cn]=comp
    comp=seq_len(cr)
  }
  if(is.character(x)) x=apply(x,2,as.numeric)
  if(is.matrix(comp) && is.list(comp)) comp=apply(comp,2,as.numeric)
  dtm=Matrix(as.matrix(x),sparse=TRUE)
  if(do.wmc) x=wmc(x)
  if(is.null(nrow(x))) x=t(as.matrix(x))
  if(drop){
    if(sum(su<-colSums(x,na.rm=TRUE)!=0)!=0) x=x[,su,drop=FALSE] else stop('x is all 0s after processing')
  }
  nc=ncol(x)
  # finalizing comp
  if(cc==1 || opt$comp=='text'){
    comp.data=x[comp,,drop=FALSE]
    x=x[-comp,,drop=FALSE]
  }else if(cc==2){
    ckp=FALSE
    if(grepl('^pa|^se',comp)){
      opt$comp=if(grepl('^pa',comp)) 'pairwise' else 'sequential'
    }else if(any(!is.na(p<-pmatch(comp,rownames(lsm_profiles))))){
      opt$comp=rownames(lsm_profiles)[p]
      ckp=TRUE
      comp.data=lsm_profiles[p,,drop=FALSE]
    }else if(grepl('^au',comp)){
      p=colMeans(x,na.rm=TRUE)
      p=which.max(apply(lsm_profiles,1,function(r)cor(r,p)))
      opt$comp=paste('auto:',names(p))
      ckp=TRUE
      comp.data=lsm_profiles[p,,drop=FALSE]
    }else opt$comp=deparse(substitute(comp))
    if(ckp){
      if(any(ckp<-!(cn<-colnames(x))%in%(bn<-colnames(comp.data)))){
        if(all(ckp)) stop('x and comp have no columns in common')
        if('articles'%in%cn) bn[bn=='article']='articles'
        if('preps'%in%cn) bn[bn=='prep']='preps'
        colnames(comp.data)=bn
        if(any(ckp<-!cn%in%bn)){
          warning('x columns were not found in comp: ',paste(cn[ckp],collapse=', '))
          comp.data=comp.data[,cn[!ckp],drop=FALSE]
        }
      }else comp.data=comp.data[,cn,drop=FALSE]
    }
  }else if(!is.null(comp.data)){
    cn=colnames(x)
    cns=cn[ck<-cn%in%colnames(comp.data)]
    if(!any(ck)) stop('x and comp have no columns in common') else if(any(!ck)){
      warning('x columns were not found in comp: ',paste(cn[!ck],collapse=', '))
      x=x[,cns]
    }
    comp.data=comp.data[,cns,drop=FALSE]
  }
  if(is.matrix(comp.data) && is.list(comp.data)) comp.data=apply(comp.data,2,as.numeric)
  compmeanck=opt$comp=='mean'
  sim=speaker=NULL
  if(!is.null(group)){
    if(!is.null(comp.data) && NROW(comp.data)==1){
      group=NULL
      warning('group does not appear to be meaningful for this comparison, so it was ignored')
    }else if(!is.list(group)) group=list(group)
    gl=length(group)
    if(opt$comp=='sequential'){
      speaker=group[[gl]]
      group=group[-gl]
      gl=length(group)
    }
    if(gl>1 && !all.levels){
      group=list(do.call(paste,group))
      gl=1
    }
    if(gl!=0){
      sim=as.data.frame(group)
      colnames(sim)=paste0('g',seq_len(gl))
      for(m in inp$metric) sim[,m]=NA
      mets=seq_along(inp$metric)+gl
    }
  }
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
      }else if(is.null(nrcd<-nrow(comp.data)) || (nrcd==1 || nrcd==rx)) sal$b=comp.data else
        warning('a group must be specified when comp has more than one row')
    }else if(ckf) sal$b=comp.data=if(compmeanck) colMeans(x,na.rm=TRUE) else apply(na.omit(x),2,comp)
    sim=do.call(lma_simets,c(list(x),sal))
  }else{
    cks=!is.null(speaker)
    ckc=!is.null(comp.data)
    ckp=cc==2 && opt$comp=='pairwise'
    ckq=cc==2 && opt$comp=='sequential'
    if(gl==1){
      if(opt$comp!='pairwise'){
        ckmc=FALSE
        gs=unique(sim[,1])
        if(is.null(comp.data) && ckf){
          ckmc=TRUE
          opt$comp=paste(deparse(opt$group),'group',opt$comp)
          comp.data=data.frame(matrix(NA,length(gs),nc,dimnames=list(gs,colnames(x))))
        }
        for(g in gs){
          su=sim[,1]==g
          if(cks){sal$group=speaker[su];sal$mean=TRUE}else if(ckc){
            if(nrow(cc<-comp.data[if(!is.null(comp.group)) comp.group==g else g,,drop=FALSE])==1)
              sal$b=cc else warning('comp.data has too few/many rows in group ',g)
          }else if(ckf) if(sum(su)>1) sal$b=if(compmeanck) colMeans(x[su,],na.rm=TRUE) else
            apply(na.omit(x[su,]),2,comp) else{sim[su,mets]=1;next}
          if(!is.null(sal$b) && ckmc) comp.data[g,]=sal$b
          if((sum(su)==1 && is.null(sal$b))){sim[su,mets]=1;next}
          tm=do.call(lma_simets,c(list(x[su,,drop=FALSE]),sal))
          sim[su,mets]=if(cks) c(NA,tm) else tm
        }
      }else{
        sal$square=if('square'%in%names(dsp$s)) dsp$s$square else TRUE
        sal$mean=if('mean'%in%names(dsp$s)) dsp$s$mean else TRUE
        if(sal$square && sal$mean){
          sim=vapply(seq_along(group[[1]]),function(i){
            su=group[[1]]==group[[1]][i]
            su[i]=FALSE
            sal$b=x[i,]
            r=if(sum(su)!=0) do.call(lma_simets,c(list(x[su,,drop=FALSE]),sal)) else
              vapply(sal$metric,function(m)NA,0)
            if(is.null(nrow(r)) || nrow(r)==1) r else if(ncol(r)!=1) colMeans(r) else mean(r)
          },numeric(length(sal$metric)))
          sim=data.frame(group[[1]],if(is.matrix(sim)) t(sim) else sim)
          colnames(sim)=c(opt$group,sal$metric)
        }else{
          sim=lapply(ug<-unique(group[[1]]),function(g){
            su=group[[1]]==g
            if(sum(su)!=1) do.call(lma_simets,c(list(x[su,,drop=FALSE]),sal)) else
              vapply(sal$metric,function(m)NA,0)
          })
          if(!sal$square){
            sim=data.frame(
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
      group=vapply(sug,function(g)do.call(paste,group[seq_len(g)]),character(nrow(sim)))
      if(!is.null(comp.group) && NCOL(comp.group)==gl)
        comp.group=vapply(sug,function(g)do.call(paste,comp.group[seq_len(g)]),
          character(nrow(comp)))
      if(!is.null(dsp$s$mean) && !dsp$s$mean){
        sal$square=TRUE
        sim=lapply(ug<-unique(sim[,1]),function(g){
          su=sim[,1]==g
          gsm=do.call(lma_simets,c(list(x[su,]),sal))
          gn=group[su,ncol(group)]
          for(m in names(gsm)) dimnames(gsm[[m]])=list(gn,gn)
          gsm
        })
        if(!is.null(dsp$s$square) && !dsp$s$square){
          sim=do.call(rbind,lapply(sim,function(ll){
            m=ll[[1]]
            su=lower.tri(m)
            data.frame(
              comp=outer(n<-colnames(m),n,function(a,b)paste0(a,' <-> ',b))[su],
              lapply(ll,function(m)m[su])
            )
          }))
        }else names(sim)=ug
      }else{
        sal$square=FALSE
        ssl=if(is.null(speaker)) TRUE else !is.na(speaker)
        for(g in unique(sim[,1])){
          su=which(sim[,1]==g & ssl)
          sg=group[su,,drop=FALSE]
          sx=x[su,,drop=FALSE]
          for(s in sug){
            usg=unique(sg[,s])
            if(length(usg)==1){
              ssg=list(sx)
              names(ssg)=usg
            }else{
              ssg=lapply(usg,function(ss)sx[sg[,s]==ss,,drop=FALSE])
              names(ssg)=usg
              ssg=Filter(function(ss)nrow(ss)>1,ssg)
            }
            if(length(ssg)!=0) for(ssn in names(ssg)){
              ssu=su[sg[,s]==ssn]
              lss=length(ssu)
              if(lss<2) next
              if(cks) sal$group=speaker[ssu] else if(ckf)
                sal$b=if(compmeanck) colMeans(ssg[[ssn]],na.rm=TRUE) else
                  apply(na.omit(ssg[[ssn]]),2,comp)
              if(!is.null(sal$b) && identical(sal$b,ssg[[ssn]])){sim[ssu,gl+mw+(mn*(s-1))]=1;next}
              ssim=do.call(lma_simets,c(list(ssg[[ssn]]),sal))
              if(ckp || ckq){
                if(ckp){
                  if(length(ssim[[1]])!=1) ssim=vapply(ssim,mean,0,na.rm=TRUE)
                }else if(nrow(ssim)>1) ssim=colMeans(ssim,na.rm=TRUE)
                if(lss!=1) ssim=vapply(ssim,rep,numeric(lss),lss)
              }
              csu=gl+mw+(mn*(s-1))
              if(all(dim(ssim)==c(lss,length(csu)))) sim[ssu,csu]=ssim else warning(g,s)
            }
          }
        }
      }
    }
  }
  list(dtm=dtm,processed=x,comp.type=opt$comp,comp=comp.data,group=opt$group,sim=sim)
}

#' Document-Term Matrix Creation
#'
#' Creates a document-term matrix (dtm) from a set of texts.
#' @param text Texts to be processed. This can be a vector (such as a column in a data frame)
#'   or list.
#' @param exclude A character vector of words to be excluded. If \code{exclude} is a single string
#'   matching \code{'function'}, \code{lma_dict()} will be used.
#' @param context A character vector used to reformat text based on look- ahead/behind. For example,
#'   you might (hopelessly) attempt to disambiguate \emph{like} by reformatting certain \emph{like}s
#'   (e.g., \code{context=c('(i) like*','(you) like*','(do) like')}, where words in parentheses are
#'   the
#'   context for the target word, and asterisks denote partial matching). This would be converted
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
#'   with "FROWN").
#' @param to.lower Logical: if \code{FALSE}, words with different capitalization are treated as
#'   different terms.
#' @param word.break A regular expression string determining the way words are split. Default is
#'   \code{' +'} which breaks words at one or more blank spaces. You may also like to break by
#'   dashes or slashes (\code{' +|/|-'}), depending on the text.
#' @param dc.min Numeric: excludes terms appearing in fewer than the set number of documents.
#'   Default is 0 (no limit).
#' @param dc.max Numeric: excludes terms appearing in more than the set number of documents. Default
#'   is Inf (no limit).
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
#' @importFrom fastmatch fmatch

lma_dtm=function(text,exclude=NULL,context=NULL,numbers=FALSE,punct=FALSE,urls=TRUE,
  emojis=FALSE,to.lower=TRUE,word.break=' +',dc.min=0,dc.max=Inf){
  if(is.null(text)) stop(substitute(text),' not found')
  text=as.character(text)
  st=proc.time()[3]
  if(!urls){
    text=gsub('http[^ ]*|www[^ ]*| [a-z]+\\.[a-z]{2,}[./][^ ]*',' url ',text,TRUE)
    text=gsub('(?<=[A-Z])\\. ',' ',text,perl=TRUE)
  }
  text=gsub('(?<=st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof)\\. |^|$',' ',text,TRUE,perl=TRUE)
  text=gsub('\\.(?![A-z0-9])',' . ',text,perl=TRUE)
  if(to.lower) text=tolower(text)
  if(!missing(exclude)){
    if(length(exclude)==1 && grepl(exclude,'function',TRUE)){
      exclude=unlist(lma_dict(),use.names=FALSE)
    }else if(is.list(exclude)) exclude=unlist(exclude,use.names=FALSE)
  }
  if(any(punct,emojis,!missing(context))){
    special=lma_dict(special)[[1]]
    if(!missing(context) && length(context)==1 && grepl('like',context,TRUE))
      context=special[['LIKE']]
    if(punct) text=gsub(special[['ELLIPSIS']],' ELLIPSIS ',text)
    if(emojis) for(type in c('SMILE','FROWN')) text=gsub(special[[type]],paste('',type,''),text)
    if(!missing(context)){
      if(!any(grepl('[?=]',context))){
        context=gsub('^\\(','(?<=',context)
        context=gsub('\\((?!\\?)','(?=',context,perl=TRUE)
        context=gsub('(?<![)*])$','[ .,?!:;/"\']',context,perl=TRUE)
        context=gsub('\\*','[^ /-]*',context,perl=TRUE)
      }
      context=structure(
        as.list(context),
        names=paste('',gsub('--+','-',gsub('[ ^[]|\\]','-',gsub('[^A-z0-9 ]','',context))),'')
      )
      for(rn in names(context)) text=gsub(context[[rn]],rn,text,perl=TRUE)
    }
  }
  text=gsub(',(?=[0-9])','',text,perl=TRUE)
  if(!numbers) text=gsub('[^A-z ]*[0-9],*',' ',text)
  text=gsub('\\t|\\r|\\n|(?=[^A-z.0-9])|(?=[\\^`\\\\[\\]])|(?<=[\\^`\\\\[\\]])|(?<=[^A-z.0-9])',
    ' ',text,perl=TRUE)
  text=gsub("(?<=[A-z]) ['\u00E7\u00ED] (?=[A-z])","'",text,perl=TRUE)
  text=gsub('(?<=[A-z]) / (?=[A-z])','/',text,perl=TRUE)
  if(!punct) text=gsub('[[:punct:]] +',' ',text)
  text=gsub('^ +| (?= )| +$','',text,perl=TRUE)
  text=strsplit(text,word.break)
  wc=if(punct) vapply(text,function(l)sum(!grepl('^[[:punct:]]$|^ELLIPSIS$',l)),numeric(1)) else
    vapply(text,length,numeric(1))
  words=sort(unique(unlist(text)))
  words=words[!words=='']
  if(!missing(exclude)) words=grep(paste(exclude,collapse='|'),words,value=TRUE,invert=TRUE)
  m=matrix(0L,length(text),length(words),dimnames=list(c(),words))
  cseq=function(x){
    x=sort(x)
    l=length(x)
    v=c=unique(x)
    i=1
    for(u in seq_along(v)){
      n=i
      while(i<l && x[i]==x[i+1]) i=i+1
      c[u]=i-n+1
      i=i+1
    }
    list(values=v,counts=as.integer(c))
  }
  for(t in seq_along(text)){
    wm=fmatch(text[[t]],words)
    wm=cseq(wm)
    m[t,wm$value]=wm$count
  }
  su=colSums(m>0,na.rm=TRUE)
  su=su>dc.min & su<dc.max
  m=if(any(!su)) m[,su] else m
  attr(m,'WC')=unlist(wc,use.names=FALSE)
  attr(m,'type')='count'
  if(!missing(dc.min) || !missing(dc.max))
    attr(m,'info')=paste('a lim of',dc.min,'and',dc.max,'left',sum(su),'of',length(words),
      'unique terms')
  attr(m,'opts')=c(numbers=numbers,punct=punct,urls=urls,to.lower=to.lower)
  attr(m,'time')=c(dtm=unname(proc.time()[3]-st))
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
#'     \code{log} \tab \code{log(dtm + 1)} \tab logarithm of frequencies\cr
#'     \code{sqrt} \tab \code{dtm^.5} \tab square root of frequencies\cr
#'     \code{count} \tab \code{dtm} \tab unaltered; sometimes called term frequencies (tf)\cr
#'     \code{amplify} \tab \code{dtm^1.1} \tab amplify difference in frequencies\cr
#'   }
#'
#'   \strong{Document weights} (applied by column)
#'   \tabular{lll}{
#'     \code{entropy} \tab \code{1 - rowSums(x * log(x) / log(ncol(x)))} \tab where
#'       \code{x = t(dtm) / colSums(dtm > 0)};
#'       entropy of term-conditional document distribution; gives common words more weight\cr
#'     \code{dfmax} \tab \code{log(apply(dtm, 2, max))} \tab logarithm of maximum document
#'       frequency\cr
#'     \code{df} \tab \code{log(colSums(dtm > 0))} \tab logarithm of binary document sum\cr
#'     \code{poisson} \tab \code{1 - dpois(0, colSums(dtm) / nrow(dtm))} \tab Poisson-predicted
#'       erm distribution\cr
#'     \code{ridf} \tab \code{idf - log(poisson)} \tab residual inverse document frequency;
#'       gives uncommon words more weight\cr
#'     \code{normal} \tab \code{1 / colSums(dtm^2)^.5} \tab normalized document frequency\cr
#'     \code{idf} \tab \code{log(nrow(dtm) / colSums(dtm > 0))} \tab inverse document frequency\cr
#'   }
#'
#' @param to.freq logical: if \code{FALSE}, the dtm is not divided by document word count before
#'   being weighted.
#' @param freq.complete if the dtm was made with \code{\link{lma_dtm}} (has a \code{'WC'}
#'   attribute), word counts for
#'   frequencies can be based on the raw count (default; \code{freq.complete = TRUE}). If
#'   \code{freq.complete = FALSE}, or the dtm does not have a \code{'WC'} attribute,
#'   \code{rowSums(dtm)} is used as word count.
#' @param log.base the base of logarithms, applied to any weight using \code{\link[base]{log}}.
#'   Default is 10.
#' @param doc.only logical: if \code{TRUE}, only document weights are returned (a single value for
#'   each term).
#' @param percent logical; if \code{TRUE}, frequencies are multiplied by 100.
#' @note
#' Term weights works to dampen differences in word count, with differences meaning increasingly less
#' from \code{amplify} to \code{count} to \code{sqrt} to \code{log} to \code{binary}.
#'
#' Document weights work to treat words differently based on their frequency. \code{entropy},
#' \code{dfmax}, \code{df}, and \code{poisson} (most to least intense over document frequencies of ~15)
#' give more frequent words more weight, whereas \code{ridf}, \code{normal}, and \code{idf}, give less
#' frequent words more weight.
#'
#' \code{weight} can either be a vector with two characters, corresponding to term weight and
#' document weight (e.g., \code{weight = c('count','idf')}), or it can be a string with term and
#' document weights separated by any of \code{*-_, :;/\\\\} (e.g., \code{weight = 'count * idf'}).
#' \code{'tf'} is also acceptable for \code{'count'}, and \code{'tfidf'} will be parsed as
#' \code{c('count','idf')}, though this is a special case.
#'
#' For \code{weight}, term or document weights can be entered individually; term weights alone will
#' not apply any document weight, and document weights alone will apply a \code{'count'} term weight
#' (unless \code{doc.only = TRUE}, in which case a term-named vector of document weights is returned
#' instead of a weighted dtm).
#' @examples
#' # visualize term and document weights
#' document_frequency = seq_len(20)
#' op = list(y='term_frequency~document_frequency',line='connected',leg='outside')
#'
#' # term weights
#' term_weights = c('binary','log','sqrt','count','amplify')
#' term_frequency = sapply(term_weights,function(w)lma_weight(matrix(document_frequency,1),w,FALSE))
#' if(require(splot)) splot(myl=c(0,25),options=op)
#'
#' # document weights
#' doc_weights = c('df','dfmax','idf','normal','poisson','ridf','entropy')
#' term_frequency = sapply(doc_weights,function(w)
#'   lma_weight(sapply(document_frequency,function(i)sample(0:i,5000,TRUE)),w,FALSE,doc.only=TRUE)
#' )
#' if(require(splot)) splot(myl=c(-3,3),mv.scale=TRUE,options=op)
#'
#' @export

lma_weight=function(dtm,weight='count',to.freq=TRUE,freq.complete=TRUE,log.base=10,doc.only=FALSE,percent=FALSE){
  ck=attr(dtm,'type')
  if(!is.null(ck) && length(ck)==3 && (ck[1]=='TRUE' || ck[2]!='count' || ck[3]!='NA')){
    message('the entered dtm appears to already be weighted (',paste(ck[2:3],collapse='*'),
      '), so it will not be altered')
    return(dtm)
  }
  dtm=if(to.freq){
    wc=attr(dtm,'WC')
    dtm=as.matrix(dtm)
    if(is.null(wc) || !freq.complete) wc=rowSums(dtm,na.rm=TRUE)
    su=dtm!=0 & !is.na(dtm)
    dtm=t(vapply(seq_along(wc),function(r){
      d=dtm[r,]
      if(any(su<-(!is.na(d) & d!=0))) d[su]=d[su]/wc[r]*if(percent) 100 else 1
      d
    },numeric(ncol(dtm))))
  }else as.matrix(dtm)
  term=function(x,type) switch(type,
    binary=(x>0)*1,
    log=log(x+1,base=log.base),
    sqrt=x^.5,
    count=x,
    amplify=x^1.1
  )
  doc=function(x,type) switch(type,
    df=log(colSums(x>0,na.rm=TRUE),base=log.base),
    dfmax=log(apply(x,2,max),base=log.base),
    idf=log(nrow(x)/colSums(x>0,na.rm=TRUE),base=log.base),
    normal=1/colSums(x^2,na.rm=TRUE)^.5,
    poisson=1-dpois(0,colSums(x,na.rm=TRUE)/nrow(x)),
    ridf=doc(x,'idf')-log(doc(x,'poisson'),base=log.base),
    entropy={x=t(x)/colSums(x>0,na.rm=TRUE);1-rowSums(x*log(x,base=log.base)/log(ncol(x),base=log.base),
      na.rm=TRUE)}
  )
  if(length(weight)==1){
    weight=strsplit(weight,' *[-:\\*_/; ,] *')[[1]]
    if(length(weight)==1 && weight=='tfidf') weight=c('count','idf')
  }
  if(grepl('^t|^na|^non|^f',weight[1],TRUE)) weight[1]='count'
  tw=tryCatch(match.arg(weight[1],c('binary','log','sqrt','count','amplify')),error=function(e)NULL)
  pdw=TRUE
  dws=c('df','dfmax','idf','normal','poisson','ridf','entropy')
  if(is.null(tw)){
    tw=tryCatch(match.arg(weight[1],dws),error=function(e)NULL)
    if(!is.null(tw)){
      pdw=FALSE
      if(!doc.only){
        dw=tw
        tw='count'
      }else return(doc(dtm,tw))
    }else stop(paste(weight),' is not a recognized weight',call.=FALSE)
  }
  if(pdw) dw=if(length(weight)>1) match.arg(weight[2],dws) else 'none'
  dtm=if(dw=='none') term(dtm,tw) else t(t(term(dtm,tw))*doc(dtm,dw))
  attr(dtm,'type')=c(freq=to.freq,term=tw,document=dw)
  dtm
}

#' Latent Semantic Space Operations
#'
#' Calculate and reduce the singular value decomposition of a dtm (i.e., create a latent semantic
#' space), or map a dtm onto an existing latent semantic space.
#' @param dtm A matrix with terms as column names.
#' @param space A matrix of right singular vectors (a latent semantic space), with terms as
#'   rownames. If missing, this will be calculated from the \code{dtm}. If a character, a file
#'   matching the character will be searched for in \code{path} (e.g., \code{space = 'default'}).
#'   If the file has a .sqlite extension, only a subset will be loaded into RAM; this is slightly
#'   slower than using a full, preloaded .rda space, but faster than loading and unloading a complete
#'   .rda space, and less RAM intensive in all cases.
#' @param path Path to a folder containing spaces. Default is '~/Documents/Latent Semantic Spaces'.
#' @param dim.cutoff If a \code{space} is calculated, this will be used to decide on the number of
#'   dimensions to be retained: \code{cumsum(d) / sum(d) < dim.cutoff}, where \code{d} is a vector
#'   of singular values of \code{dtm} (i.e., \code{svd(dtm)$d}). The default is \code{.5}; lower
#'   cutoffs result in fewer dimensions.
#' @param keep.dim Logical: if \code{TRUE} and a space is being calculated from the input, a matrix
#'   in the same dimensions as \code{dtm} is returned. Otherwise, a matrix with terms as rows and
#'   svd dimensions as columns is returned. The default, truncated matrix
#' @note
#' A general latent semantic space is a selection of right singular vectors from the singular value
#' decomposition of a dtm (\code{svd(dtm)$v[,1:k]}, where \code{k} is the selected number of
#' dimensions, decided here by \code{cutoff}).
#'
#' Mapping a new dtm into a latent semantic space consists of multiplying common terms:
#' \code{dtm[,ct] \%*\% space[ct,]}, where \code{ct = colnames(dtm)[colnames(dtm) \%in\%
#' rownames(space)]} -- the terms common between the dtm and the space. This results in a matrix
#' with documents as rows, and svd dimensions as columns, replacing terms.
#' @examples
#'
#' text = c(
#'   "Hey, I like kittens. I think all kinds of cats really are just the best pet ever.",
#'   "Oh year? Well I really like cars. All the wheels and the turbos... I think that's the best
#'     ever.",
#'   "You know what? Poo on you. Cats, dogs, rabbits -- you know, living creatures... to think
#'      you'd care about anything else!",
#'   "You can stick to your opinion. You can be wrong if you want. You know what life's about?
#'     Supercharging, diesel guzzling, exhaust spewing, piston moving ignitions."
#' )
#'
#' dtm = lma_dtm(text)
#'
#' #using a space from a file might look something like this:
#' # lma_space(dtm,'default.zip','C:/user/name/downloads')
#'
#' #calculate a latent semantic space from the example text
#' lss = lma_lspace(dtm)
#'
#' #show that document similarities between the truncated and full space are the same
#' spaces = list(
#'   full = lma_lspace(dtm,keep.dim=TRUE),
#'   truncated = lma_lspace(dtm,lss)
#' )
#' sapply(spaces,lma_simets,metric='cosine')
#'
#' @export
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect

lma_lspace=function(dtm,space,path='~/Documents/Latent Semantic Spaces',
  dim.cutoff=.5,keep.dim=FALSE){
  if(missing(space)){
    nr=nrow(dtm)
    nc=ncol(dtm)
    md=min(nr,nc)
    s=svd(dtm)
    s$v=t(s$v)
    k=cumsum(s$d)/sum(s$d)
    k=seq_len(if(any(k<dim.cutoff)) which(k>=dim.cutoff)[1] else 1)
    if(keep.dim) dtm[]=s$u[,k]%*%diag(s$d[k])%*%s$v[k,] else{
      cn=colnames(dtm)
      dtm=t(s$v[k,])
      rownames(dtm)=cn
    }
  }else{
    if(is.character(space)){
      if(!missing(path)) path=sub('/$','',path)
      if(length(fls<-list.files(path))==0 || !any(grepl(space,fls))){
        message('spaces not found in ',path)
        if(grepl('^y',readline('would you like to download the space? (y/n): '),TRUE)){
          download.lsspace(space,dir=path)
          fls=list.files(path)
        }else stop('no spaces available; load and specify a space,
            or download one with download.lsspace()',call.=FALSE)
      }
      file=grep(space,fls,value=TRUE)
      if(length(file)==0) file=NA
      if(length(file)>1) file=if(any(ck<-grepl('sqlite|zip',fls))) file[ck][1] else file[1]
      ck=grepl('.rda',file,fixed=TRUE)
      if(!ck && ((nck<-is.na(file)) || grepl('.zip',file,fixed=TRUE))){
        if(any(fck<-grepl(sub('\\..*','.sqlite',file),fls,fixed=TRUE))){
          file=fls[fck]
        }else{
          if(nck) if(!(file<-sub('\\..*','.zip',space))%in%fls)
            stop('could not find ',space,' in ',path)
          unzip(paste0(path,'/',file),exdir=path)
          file=sub('.zip','.sqlite',file,fixed=TRUE)
        }
      }
      space=load(paste0(path,'/',if(ck) file else sub('.sqlite','_dict.rda',file,fixed=TRUE)))
      lss_dict=eval(parse(text=space))
      if(is.data.frame(space)) lss_dict=rownames(space)
      dtm=dtm[,vapply(data.frame(dtm),function(col)!any(is.na(col)),TRUE)]
      ts=fmatch(colnames(dtm),lss_dict,nomatch='')
      ts=ts[!is.na(ts)]
      if(length(ts)==0) stop('found no terms in common with the loaded space')
      if(!ck){
        tryCatch({
          db=dbConnect(SQLite(),dbname=paste0(path,'/',file))
          on.exit(dbDisconnect(db))
          space=dbGetQuery(db,paste0('SELECT * FROM en where _INDEX IN (',
            paste(ts,collapse=','),')'))
        },error=function(e)stop('failed to query space: ',e$message,call.=FALSE))
        space=as.matrix(space[,-1])
        rownames(space)=lss_dict[ts]
      }else space=as.matrix(space[ts,-1])
      ts=rownames(space)
    }else{
      ts=colnames(dtm)[colnames(dtm)%in%rownames(space)]
      space=as.matrix(space[ts,])
    }
    rep=length(ts)/ncol(dtm)
    if(rep<.2) warning(paste0('
      only ',round(rep*100,2),'% of dtm terms appear in the provided space;
      you might consider using a different source or cleaning/partial matching terms
    '))
    dtm=dtm[,ts]%*%space
  }
  dtm
}

#' Document-Term Matrix Categorization
#'
#' Reduces the dimensions of a document-term matrix by dictionary-based categorization.
#' @param dtm A matrix with words as column names.
#' @param dict A \code{list} object with named character vectors as word lists.
#' @param term.weights A \code{list} object with named numeric vectors lining up with the character vectors
#'   in \code{dict}, used to weight the terms in each \code{dict} vector. If a category in \code{dict}
#'   is not specified in \code{term.weights}, or the \code{dict} and \code{term.weights} vectors aren't the same
#'   length, the weight for that category will be 1.
#' @param bias A list or named vector specifying a constant to add to the named category. If an '_intercept' is
#'   included in a category, if will be removed from the category, and the associated \code{weight} will be used
#'   as the \code{bias} for that category.
#' @param escape Logical indicating whether the terms in \code{dict} should not be treated as plain text
#'   (including asterisk wild cards). If \code{TRUE}, regular expression related characters are escaped. Set to
#'   \code{TRUE} if you get PCRE compilation errors.
#' @param term.filter A regular expression string used to format the text of each term (passed to
#'   \code{gsub}). For example, if terms are part-of-speech tagged (e.g.,
#'   \code{'a_DT'}), \code{filter='_.*'} would remove the tag.
#' @param term.break A limit used to break up longer categories. Reduce from 3900 if you get a PCRE compilation
#'   error.
#' @export

lma_termcat=function(dtm,dict,term.weights=list(),bias=NULL,escape=FALSE,term.filter=NULL,term.break=3900){
  st=proc.time()[3]
  if(missing(dict)) dict=lma_dict(1:9)
  if(!is.list(dict)){
    if(!missing(term.weights) && !is.list(term.weights)) term.weights=list(cat=term.weights) else{
      if(length(term.weights)==1) names(term.weights)='cat' else{
        if(any(l<-lapply(term.weights,length)==length(dict))){
          term.weights=term.weights[l][1]
          names(term.weights)='cat'
        }else{
          if(!missing(term.weights)) warning('no weights line up with the dict category, so they will be ignored')
          term.weights=list(cat=rep(1,length(dict)))
        }
      }
    }
    dict=list(cat=dict)
  }
  for(n in names(dict)) if(!n%in%names(bias) && any(ii<-dict[[n]]=='_intercept')){
    dict[[n]]=dict[[n]][!ii]
    bias[n]=weight[[n]][ii]
    weight[[n]]=weight[[n]][!ii]
  }
  dict=lapply(dict,as.character)
  ats=attributes(dtm)[c('opts','WC','orientation','type')]
  ats=ats[!vapply(ats,is.null,TRUE)]
  atsn=names(ats)
  if(any((l<-vapply(dict,length,0))>term.break)){
    br=function(l,e=term.break){
      l=length(l)
      f=round(l/e+.49)
      o=lapply(seq_len(f),function(i)seq_len(e)+e*(i-1))
      o[[f]]=o[[f]][o[[f]]<=l]
      o
    }
    ag=list(dtm,bias=bias,escape=escape,term.filter=term.filter,term.break=term.break)
    if(!missing(term.weights)) ag$term.weights=term.weights
    op=vapply(names(dict),function(n){
      if(l[n]>term.break) Reduce('+',lapply(br(dict[[n]]),function(s) do.call(lma_termcat,
        c(ag,dict=dict[[n]][s],if(missing(term.weights)) term.weights=term.weights[[n]][s])
      ))) else do.call(lma_termcat,c(ag,dict=dict[cat],if(missing(term.weights)) term.weights=term.weights[cat]))
    },numeric(nrow(dtm)))
  }else{
    ord=dict
    lab=lapply(dict,function(l)grep('(',l,fixed=TRUE))
    lab=lab[vapply(lab,length,0)!=0]
    if(length(lab)!=0){
      special=lma_dict(special)[[1]][c('SMILE','FROWN')]
      for(l in names(lab)){
        for(en in c('SMILE','FROWN')) dict[[l]][lab[[l]]]=gsub(special[[en]],en,dict[[l]][lab[[l]]])
        sul=grep('(',dict[[l]],fixed=TRUE)
        dict[[l]][sul]=gsub("'|^-+|-(?=-)|\\*-+",'',
          gsub('$','-',gsub('[^A-z0-9*\']','-',dict[[l]][sul])),perl=TRUE)
        dict[[l]]=dict[[l]][!grepl('[(/:;]|\\)',dict[[l]])]
      }
    }
    dict=if(!escape) lapply(ord,paste,collapse='|') else lapply(dict,function(l) if(length(l)!=1)
      gsub('\\*\\$','',paste(paste0('^',gsub('(?=[*.^$({[\\]})+?-])','\\\\',l,perl=TRUE),'$',collapse='|'))) else l)
    ws=if(is.null(term.filter)) colnames(dtm) else gsub(term.filter,'',colnames(dtm),perl=TRUE)
    if('opts'%in%atsn && !ats$opts['to.lower']) ws=tolower(ws)
    op=if(!missing(term.weights)){
      ord=lapply(ord,function(cat)gsub('\\^|\\$','',cat))
      for(n in names(dict)){
        l=length(ord[[n]])
        if(!n%in%names(term.weights)) term.weights[[n]]=rep(1,l) else if(length(term.weights[[n]])!=l){
          warning('weights do not line up with terms for the ',n,' category, so they were ignored',call.=FALSE)
          term.weights[[n]]=rep(1,l)
        }
        names(term.weights[[n]])=ord[[n]]
      }
      vapply(names(dict),function(c){
        su=dtm[,grep(dict[[c]],ws,perl=TRUE),drop=FALSE]
        colSums(t(su)*term.weights[[c]][colnames(su)],na.rm=TRUE)
      },numeric(nrow(dtm)))
    }else{
      vapply(names(dict),function(c)rowSums(dtm[,grep(dict[[c]],ws,perl=TRUE),drop=FALSE],na.rm=TRUE),numeric(nrow(dtm)))
    }
  }
  if(!is.null(bias)) for(n in names(bias)) if(n%in%names(op)) op[,n]=op[,n]+bias[[n]]
  attr(op,'WC')=if('WC'%in%atsn) ats$WC else rowSums(dtm,na.rm=TRUE)
  attr(op,'time')=c(attr(dtm,'time'),termcat=unname(proc.time()[3]-st))
  if('orientation'%in%atsn) attr(op,'orientation')=ats$orientation
  if('type'%in%atsn) attr(op,'type')=ats$type
  op
}

#' Similarity, Distance, and/or Accommodation Metrics
#'
#' @param a vector or matrix. If a vector, \code{b} must also be provided. If a matrix and \code{b}
#'   is missing, each row will be compared. If a matrix and \code{b} is not missing, each row will
#'   be compared with \code{b}.
#' @param b vector to be compared with \code{a} or rows of \code{a}, or a matrix with the same
#'   number of rows as \code{a}, in which case each row of \code{a} and \code{b} will be compared.
#' @param metric a function (see details), or a character at least partially matching one of the
#'   available metrics:
#'   \tabular{ll}{
#'     \code{euclidean} \tab \code{1 / (1 + sum((a - b)^2)^.5)} \cr
#'     \code{canberra} \tab \code{mean(as.numeric(1 - abs(a - b) / (a + b + .0001)))} \cr
#'     \code{cosine} \tab \code{sum(a * b) / sum(a^2 * sum(b^2))^.5} \cr
#'     \code{pearson} \tab \code{cor(a, b, method='pearson')} \cr
#'     \code{kendall} \tab \code{cor(a, b, method='kendall')} \cr
#'     \code{spearman} \tab \code{cor(a, b, method='spearman')} \cr
#'     \code{jaccard} \tab \code{sum(a & b) / sum(a | b)} \cr
#'     \code{kld} \tab \code{p = (p<-table(a,b)/100)/sum(p); q = rowSums(p) \%*\% matrix(colSums(p),1);
#'       sum(p*log(p/q))}\cr
#'   }
#' @param metric.arg a list of arguments to be passed to \code{metric} if \code{metric} is a
#'   function.
#' @param group if \code{b} is missing and \code{a} has multiple rows, this will be used to make
#'   comparisons between rows of \code{a}, as modified by \code{agg} and \code{add.mean}.
#' @param agg logical; if \code{FALSE}, only the boundary rows between groups will be compared, see
#'   example.
#' @param agg.mean logical; if \code{FALSE}, consecutive rows of the same group will be summed.
#' @param square logical; if \code{FALSE}, only the lower triangle is returned from a pairwise
#'   comparison.
#' @param mean logical; if \code{TRUE}, a single mean for each metric is returned.
#' @param sample If \code{a} is a matrix and \code{b} is not specified (a pariwise comparison is to
#'   be made), this determines whether every comparison or a sample should be calculated. If
#'   \code{sample} is a number smaller than \code{nrow(a)}, each row will be compared against a
#'   random sample of other rows, not including the given row.
#' @param ncores sets the number of CPU cores to be used during pairwise comparisons. If not
#'   specified, multiple cores will only be used if \code{nrow(a)} is greater than 1000, in which
#'   case the number of detected cores - 2 will be used.
#' @details
#' When a function, \code{metric} is called in places of built-in metrics. Two arguments are always
#' passed to it, generally corresponding to rows of \code{a} in the first position, and \code{b} in
#' the second. If \code{b} is missing and \code{a} has more than one row, other rows of \code{a}
#' will be in the second position.
#'
#' If \code{metric} is a function with compatible first and second position arguments, this can be
#' entered directly (e.g., \code{metric = energy::dcor}). Otherwise, \code{a} and \code{b} might
#' have to be reformatted (e.g., \code{metric = function(a,b) entropy::mi.empirical(table(a,b))}).
#'
#' The function entered as \code{metric} must always return a single numerical value.
#' @examples
#' text = c(
#'   'words of speaker A','more words from speaker A',
#'   'words from speaker B','more words from speaker B'
#' )
#' speaker = c('A','A','B','B')
#'
#' (dtm = lma_dtm(text))
#'
#' # by default, consecutive rows from the same group are averaged:
#' lma_simets(dtm, group=speaker)
#'
#' # with agg = FALSE, only the rows at the boundary between
#' # groups (rows 2 and 3 in this case) are used:
#' lma_simets(dtm, group=speaker, agg=FALSE)
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar% registerDoSEQ

lma_simets=function(a,b=NULL,metric,metric.arg=list(),group=NULL,agg=TRUE,agg.mean=TRUE,square=TRUE,
  mean=FALSE,sample=200,ncores=detectCores()-2){
  cf=NULL
  comp=function(a,b,metric) switch(metric,
    euclidean=1/(1+sum((a-b)^2)^.5),
    canberra=mean(as.numeric(1-abs(a-b)/(a+b+.0001))),
    cosine=sum(a*b)/sum(a^2*sum(b^2))^.5,
    pearson=cor(a,b,method='pearson'),
    kendall=cor(a,b,method='kendall'),
    spearman=cor(a,b,method='spearman'),
    jaccard=sum(a&b)/sum(a|b),
    kld={p=(p<-table(a,b)/100)/sum(p);q=rowSums(p,na.rm=TRUE)%*%matrix(colSums(p,na.rm=TRUE),1);
      sum(p*log(p/q+.0001))},
    custom=do.call(cf,c(list(a,b),metric.arg))
  )
  mets=c('euclidean','canberra','cosine','pearson','spearman','kendall','jaccard','kld')
  metric=if(missing(metric)) mets else if(is.function(metric)){
    cf=metric
    'custom'
  }else{
    if(is.null(metric)) metric='cosine'
    mets[if(is.numeric(metric)) metric else pmatch(metric,mets)]
  }
  if('kld'%in%metric){
    a=a!=0
    if(!is.null(b)) b=b!=0
  }
  st=proc.time()[3]
  if(is.null(b)){
    n=NROW(a)
    if(n<2) stop('a must have more than 1 row when b is not provided',call.=FALSE)
    if(!is.numeric(a)) a=if(n>1) apply(a,2,as.numeric) else as.numeric(a)
    if(is.null(group)){
      rand=is.numeric(sample) && n>sample
      p=ncores>1 && (!missing(ncores) || n>1000)
      if(p){
        clust=makeCluster(ncores)
        registerDoParallel(clust)
      }else registerDoSEQ()
      res=if(rand){
        if(missing(mean)) mean=TRUE
        ms=length(metric)
        res=matrix(numeric((if(mean) n else n*sample)*ms),ncol=ms,dimnames=list(c(),metric))
        for(m in metric) res[,m]=foreach(i=seq_len(n),.combine=c) %dopar% {
          r=vapply(sample(seq_len(n)[-i],sample),function(b)comp(a[i,],a[b,],m),0)
          if(mean) mean(r) else r
        }
        res
      }else{
        if(missing(mean) && !missing(square) && !square) mean=TRUE
        su=diag(n)
        m=vapply(metric,function(met)list(met=su),list(0))
        su=lower.tri(su)
        for(met in metric) m[[met]][su]=foreach(i=seq_len(n),.combine=c) %dopar%
          vapply(seq_len(n-i)+i,function(r)comp(a[i,],a[r,],met),0)
        if(square){
          u=upper.tri(m[[1]])
          res=lapply(m,function(i){i[u]=t(i)[u];i})
          if(mean) vapply(res,function(i)(colSums(i)-1)/(ncol(i)-1),numeric(n)) else res
        }else vapply(m,function(i)i[su],numeric((n-1)*n/2))
      }
      if(p) stopCluster(clust)
    }else{
      if(length(group)!=n) stop('length(group) != NROW(a)')
      cblock=function(i=1,d=1){
        f=d>0
        e=if(f) l else 1
        if(f && i>e) return(list(b=group[e],s=e,e=e))
        if(!f && i>l) i=l
        s=i
        b=group[i]
        while((if(f) i<e else i>e) && group[i]==group[i+d]){i=i+d; b=c(b,group[i])}
        list(v=b,s=seq.int(s,i),e=i)
      }
      l=length(group)
      rows=c()
      res=NULL
      i=1
      while(i<l){
        be=cblock(i)
        cu=cblock(be$e+1)
        af=cblock(cu$e+1)
        ic=cu$e+1<=l
        af$e=i=af$s[1]
        cu$e=af$s[1]
        r=lapply(list(a=be,b=cu,c=af),function(r){
          r=a[if(((l<-length(r$s))>1 && agg) || l==1) r$s else r$e,,drop=FALSE]
          if(length(l>1)) if(agg.mean) colMeans(r,na.rm=TRUE) else colSums(r,na.rm=TRUE) else r
        })
        res=rbind(res,vapply(metric,function(met)c(comp(r$a,r$b,met),if(ic)comp(r$b,r$c,met)),if(ic)
          c(0,0) else 0))
        rows=c(rows,
          paste(paste(if(agg) be$s else be$e,collapse=', '),'<->',
            paste(if(agg) cu$s else cu$e,collapse=', ')),
          if(ic) paste(paste(if(agg) cu$s else cu$e,collapse=', '),'<->',paste(if(agg) af$s else
            af$e,collapse=', '))
        )
      }
      rownames(res)=rows
    }
  }else{
    nrb=nrow(b)
    if(!is.numeric(b)) b=if(is.null(nrb)) as.numeric(b) else if(nrb==1) as.matrix(b) else
      apply(b,2,as.numeric)
    if(!is.null(an<-colnames(a)) && identical(an,bn<-colnames(b))){
      if(sum(sn<-an%in%bn)==0) stop('a and b have no columns in common')
      b=b[,an<-an[sn],drop=FALSE]
      a=a[,an,drop=FALSE]
    }
    res=if(!is.null(n<-nrow(a)) && n>1){
      if(!is.null(nrb) && nrb==n){
        vapply(metric,function(m)vapply(seq_len(n),function(r)comp(a[r,],b[r,],m),0),numeric(n))
      }else vapply(metric,function(m)vapply(seq_len(n),function(r)comp(a[r,],b,m),0),numeric(n))
    }else vapply(metric,function(m)comp(a,b,m),0)
  }
  if(mean && !square) res=if(is.list(res)) lapply(res,mean,na.rm=TRUE) else colMeans(res,na.rm=TRUE)
  attr(res,'time')=c(simets=unname(proc.time()[3]-st))
  res
}
