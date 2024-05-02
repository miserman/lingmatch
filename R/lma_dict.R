#' English Function Word Category and Special Character Lists
#'
#' Returns a list of function words based on the Linguistic Inquiry and Word Count 2015 dictionary
#' (in terms of category names -- words were selected independently), or a list of special characters and patterns.
#' @param ... Numbers or letters corresponding to category names: ppron, ipron, article,
#' adverb, conj, prep, auxverb, negate, quant, interrog, number, interjection, or special.
#' @param as.regex Logical: if \code{FALSE}, lists are returned without regular expression.
#' @param as.function Logical or a function: if specified and \code{as.regex} is \code{TRUE}, the selected dictionary
#' will be collapsed to a regex string (terms separated by \code{|}), and a function for matching characters to that
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
#' @seealso To score texts with these categories, use \code{\link{lma_termcat}()}.
#' @return A list with a vector of terms for each category, or (when \code{as.function = TRUE}) a function which
#' accepts an initial "terms" argument (a character vector), and any additional arguments determined by function
#' entered as \code{as.function} (\code{\link{grepl}} by default).
#' @examples
#' # return the full dictionary (excluding special)
#' lma_dict()
#'
#' # return the standard 7 category lsm categories
#' lma_dict(1:7)
#'
#' # return just a few categories without regular expression
#' lma_dict(neg, ppron, aux, as.regex = FALSE)
#'
#' # return special specifically
#' lma_dict(special)
#'
#' # returning a function
#' is.ppron <- lma_dict(ppron, as.function = TRUE)
#' is.ppron(c("i", "am", "you", "were"))
#'
#' in.lsmcat <- lma_dict(1:7, as.function = TRUE)
#' in.lsmcat(c("a", "frog", "for", "me"))
#'
#' ## use as a stopword filter
#' is.stopword <- lma_dict(as.function = TRUE)
#' dtm <- lma_dtm("Most of these words might not be all that relevant.")
#' dtm[, !is.stopword(colnames(dtm))]
#'
#' ## use to replace special characters
#' clean <- lma_dict(special, as.function = gsub)
#' clean(c(
#'   "\u201Ccurly quotes\u201D", "na\u00EFve", "typographer\u2019s apostrophe",
#'   "en\u2013dash", "em\u2014dash"
#' ))
#' @export

lma_dict <- function(..., as.regex = TRUE, as.function = FALSE) {
  cats <- as.character(substitute(...()))
  dict <- list(
    ppron = c(
      "^dae$", "^dem$", "^eir$", "^eirself$", "^em$", "^he$", "^he'", "^her$", "^hers$", "^herself$", "^hes$",
      "^him$", "^himself$", "^hir$", "^hirs$", "^hirself$", "^his$", "^hisself$", "^i$", "^i'", "^id$", "^idc$",
      "^idgaf$", "^idk$", "^idontknow$", "^idve$", "^iirc$", "^iknow$", "^ikr$", "^ill$", "^ily$", "^im$", "^ima$",
      "^imean$", "^imma$", "^ive$", "^lets$", "^let's$", "^me$", "^methinks$", "^mine$", "^my$", "^myself$", "^omfg$",
      "^omg$", "^oneself$", "^our$", "^ours", "^she$", "^she'", "^shes$", "^thee$", "^their$", "^their'", "^theirs",
      "^them$", "^thems", "^they$", "^they'", "^theyd$", "^theyll$", "^theyve$", "^thine$", "^thou$", "^thoust$",
      "^thy$", "^thyself$", "^u$", "^u'", "^ud$", "^ull$", "^ur$", "^ure$", "^us$", "^we$", "^we'", "^weve$", "^y'",
      "^ya'", "^yall", "^yins$", "^yinz$", "^you$", "^you'", "^youd$", "^youll$", "^your$", "^youre$", "^yours$",
      "^yourself$", "^yourselves$", "^youve$", "^zer$", "^zir$", "^zirs$", "^zirself$", "^zis$"
    ),
    ipron = c(
      "^another$", "^anybo", "^anyone", "^anything", "^dat$", "^de+z$", "^dis$", "^everyb", "^everyone",
      "^everything", "^few$", "^it$", "^it'$", "^it'", "^itd$", "^itll$", "^its$", "^itself$", "^many$", "^nobod",
      "^nothing$", "^other$", "^others$", "^same$", "^somebo", "^somebody'", "^someone", "^something", "^stuff$",
      "^that$", "^that'", "^thatd$", "^thatll$", "^thats$", "^these$", "^these'", "^thesed$", "^thesell$", "^thesere$",
      "^thing", "^this$", "^this'", "^thisd$", "^thisll$", "^those$", "^those'", "^thosed$", "^thosell$", "^thosere$",
      "^what$", "^what'", "^whatd$", "^whatever$", "^whatll$", "^whats$", "^which", "^who$", "^who'", "^whod$",
      "^whoever$", "^wholl$", "^whom$", "^whomever$", "^whos$", "^whose$", "^whosever$", "^whosoever$"
    ),
    article = c("^a$", "^an$", "^da$", "^teh$", "^the$"),
    adverb = c(
      "^absolutely$", "^actively$", "^actually$", "^afk$", "^again$", "^ago$", "^ahead$", "^almost$",
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
      "^whyd$", "^whys$", "^widely$", "^wither$", "^yet$"
    ),
    conj = c(
      "^also$", "^altho$", "^although$", "^and$", "^b/c$", "^bc$", "^because$", "^besides$", "^both$", "^but$",
      "^'cause$", "^cos$", "^cuz$", "^either$", "^else$", "^except$", "^for$", "^how$", "^how'", "^howd$", "^howll$",
      "^hows$", "^if$", "^neither$", "^nor$", "^or$", "^than$", "^tho$", "^though$", "^unless$", "^unlike$", "^versus$",
      "^vs$", "^when$", "^when'", "^whenever$", "^whereas$", "^whether$", "^while$", "^whilst$"
    ),
    prep = c(
      "^about$", "^above$", "^abt$", "^across$", "^acrost$", "^afk$", "^after$", "^against$", "^along$", "^amid",
      "^among", "^around$", "^as$", "^at$", "^atop$", "^before$", "^behind$", "^beneath$", "^beside$", "^betwe",
      "^beyond$", "^by$", "^despite$", "^down$", "^during$", "^excluding$", "^from$", "^here$", "^here'", "^heres$",
      "^in$", "^including$", "^inside$", "^into$", "^minus$", "^near$", "^now$", "^of$", "^off$", "^on$", "^onto$",
      "^out$", "^outside$", "^over$", "^plus$", "^regarding$", "^sans$", "^since$", "^then$", "^there$", "^there'",
      "^thered$", "^therell$", "^theres$", "^through$", "^throughout$", "^thru$", "^til$", "^till$", "^to$", "^toward",
      "^under$", "^underneath$", "^until$", "^untill$", "^unto$", "^up$", "^upon$", "^via$", "^with$", "^within$",
      "^without$", "^worth$"
    ),
    auxverb = c(
      "^am$", "^are$", "^arent$", "^aren't$", "^be$", "^been$", "^bein$", "^being$", "^brb$", "^can$",
      "^could$", "^could'", "^couldnt$", "^couldn't$", "^couldve$", "^did$", "^didnt$", "^didn't$", "^do$", "^does$",
      "^doesnt$", "^doesn't$", "^doing$", "^dont$", "^don't$", "^had$", "^hadnt$", "^hadn't$", "^has$", "^hasnt$",
      "^hasn't$", "^have$", "^havent$", "^haven't$", "^having$", "^is$", "^isnt$", "^isn't$", "^may$", "^might$",
      "^might'", "^mightnt$", "^mightn't$", "^mightve$", "^must$", "^mustnt$", "^mustn't$", "^mustve$", "^ought",
      "^shant$", "^shan't$", "^sha'nt$", "^shall$", "^should$", "^shouldnt$", "^shouldn't$", "^shouldve$", "^was$",
      "^wasnt$", "^wasn't$", "^were$", "^werent$", "^weren't$", "^will$", "^would$", "^would'", "^wouldnt", "^wouldn't",
      "^wouldve$"
    ),
    negate = c(
      "^ain't$", "^aint$", "^aren't$", "^arent$", "^can't$", "^cannot$", "^cant$", "^couldn't$", "^couldnt$",
      "^didn't$", "^didnt$", "^doesn't$", "^doesnt$", "^don't$", "^dont$", "^hadn't$", "^hadnt$", "^hasn't$", "^hasnt$",
      "^haven't$", "^havent$", "^idk$", "^isn't$", "^isnt$", "^must'nt$", "^mustn't$", "^mustnt$", "^nah", "^need'nt$",
      "^needn't$", "^neednt$", "^negat", "^neither$", "^never$", "^no$", "^nobod", "^noes$", "^none$", "^nope$",
      "^nor$", "^not$", "^nothing$", "^nowhere$", "^np$", "^ought'nt$", "^oughtn't$", "^oughtnt$", "^shant$",
      "^shan't$", "^sha'nt$", "^should'nt$", "^shouldn't$", "^shouldnt$", "^uh-uh$", "^wasn't$", "^wasnt$", "^weren't$",
      "^werent$", "^without$", "^won't$", "^wont$", "^wouldn't$", "^wouldnt$"
    ),
    quant = c(
      "^add$", "^added$", "^adding$", "^adds$", "^all$", "^allot$", "^alot$", "^amount$", "^amounts$",
      "^another$", "^any$", "^approximat", "^average$", "^bit$", "^bits$", "^both$", "^bunch$", "^chapter$", "^couple$",
      "^doubl", "^each$", "^either$", "^entire", "^equal", "^every$", "^extra$", "^few$", "^fewer$", "^fewest$",
      "^group", "^inequal", "^least$", "^less$", "^lot$", "^lotof$", "^lots$", "^lotsa$", "^lotta$", "^majority$",
      "^many$", "^mo$", "^mo'", "^more$", "^most$", "^much$", "^mucho$", "^multiple$", "^nada$", "^none$", "^part$",
      "^partly$", "^percent", "^piece$", "^pieces$", "^plenty$", "^remaining$", "^sampl", "^scarce$", "^scarcer$",
      "^scarcest$", "^section$", "^segment", "^series$", "^several", "^single$", "^singles$", "^singly$", "^some$",
      "^somewhat$", "^ton$", "^tons$", "^total$", "^triple", "^tripling$", "^variety$", "^various$", "^whole$"
    ),
    interrog = c(
      "^how$", "^how'd$", "^how're$", "^how's$", "^howd$", "^howre$", "^hows$", "^wat$", "^wattt", "^what$",
      "^what'd$", "^what'll$", "^what're$", "^what's$", "^whatd$", "^whatever$", "^whatll$", "^whatre$", "^whatt",
      "^when$", "^when'", "^whence$", "^whenever$", "^where$", "^where'd$", "^where's$", "^wherefore$", "^wherever$",
      "^whether$", "^which$", "^whichever$", "^whither$", "^who$", "^who'd$", "^who'll$", "^who's$", "^whoever$",
      "^wholl$", "^whom$", "^whomever$", "^whos$", "^whose$", "^whosever$", "^whoso", "^why$", "^why'", "^whyever$",
      "^wut$"
    ),
    number = c(
      "^billion", "^doubl", "^dozen", "^eight", "^eleven$", "^fift", "^first$", "^firstly$", "^firsts$",
      "^five$", "^four", "^half$", "^hundred", "^infinit", "^million", "^nine", "^once$", "^one$", "^quarter",
      "^second$", "^seven", "^single$", "^six", "^ten$", "^tenth$", "^third$", "^thirt", "^thousand", "^three$",
      "^trillion", "^twel", "^twent", "^twice$", "^two$", "^zero$", "^zillion"
    ),
    interjection = c(
      "^a+h+$", "^a+w+$", "^allas$", "^alright", "^anyhoo$", "^anyway[ysz]", "^bl[eh]+$", "^g+[eah]+$",
      "^h[ah]+$", "^h[hu]+$", "^h[mh]+$", "^l[ol]+$", "^m[hm]+$", "^meh$", "^o+h+$", "^o+k+$", "^okie", "^oo+f+$",
      "^soo+$", "^u[uh]+$", "^u+g+h+$", "^w[ow]+$", "^wee+ll+$", "^y[aes]+$", "^ya+h+$", "^yeah$", "^yus+$"
    ),
    special = list(
      ELLIPSIS = "\\.{3, }|\\. +\\. +[. ]+",
      SMILE = "\\s(?:[[{(<qd]+[\\s<-]*[;:8=]|[;:8=][\\s>-]*[]})>Dpb]+|[uUnwWmM^=+-]_[uUnwWmM^=+-])(?=\\s)",
      FROWN = "\\s(?:[]D)}>]+[\\s.,<-]*[;:8=]|[;:8=][\\s.,>-]*[[{(<]+|[Tt:;]_[Tt;:]|[uUtT;:][mMn][uUtT;:])(?=\\s)",
      LIKE = c(
        "(?<=could not) like\\b", "(?<=did not) like\\b", "(?<=did) like\\b", "(?<=didn't) like\\b",
        "(?<=do not) like\\b", "(?<=do) like\\b", "(?<=does not) like\\b", "(?<=does) like\\b", "(?<=doesn't) like\\b",
        "(?<=don't) like\\b", "(?<=i) like\\b", "(?<=should not) like\\b", "(?<=they) like\\b", "(?<=we) like\\b",
        "(?<=will not) like\\b", "(?<=will) like\\b", "(?<=won't) like\\b", "(?<=would not) like\\b",
        "(?<=you) like\\b"
      ),
      CHARACTERS = c(
        ` ` = "\\s",
        `'` = paste0(
          "[\u00B4\u2018\u2019\u201A\u201B\u2032\u2035\u02B9\u02BB\u02BE\u02BF\u02C8\u02CA\u02CB\u02F4",
          "\u0300\u0301\u030D\u0312\u0313\u0314\u0315\u031B\u0321\u0322\u0326\u0328\u0329\u0340\u0341\u0343\u0351",
          "\u0357]"
        ),
        `"` = "[\u201C\u201D\u201E\u201F\u2033\u2034\u2036\u2037\u2057\u02BA\u02DD\u02EE\u02F5\u02F6\u030B\u030F]",
        `...` = "\u2026",
        `-` = "[\u05BE\u1806\u2010\u2011\u2013\uFE58\uFE63\uFF0D]",
        ` - ` = "[\u2012\u2014\u2015\u2E3A\u2E3B]|--+",
        a = paste0(
          "[\u00C0\u00C1\u00C2\u00C3\u00C4\u00C5\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5\u0100\u0101\u0102",
          "\u0103\u0104\u105\u0200\u0201\u0202\u0203\u0226\u0227\u0245\u0250\u0251\u0252\u0255\u0363\u0386\u0391",
          "\u0410\u0430]"
        ),
        ae = "[\u00C6\u00E6\u0152\u0153\u0276]",
        b = paste0(
          "[\u00DF\u0180\u0181\u0182\u0183\u0184\u0185\u0186\u0187\u0188\u0189\u018A\u018B\u018C\u0243",
          "\u0253\u0299\u0411\u0412\u0431\u0432\u0462\u0463\u0494\u0495\u212C]"
        ),
        c = paste0(
          "[\u00C7\u00E7\u0106\u0107\u0108\u0109\u0186\u0187\u0188\u0254\u0297\u0368\u0421\u0441\u2102",
          "\u2103]"
        ),
        d = paste0(
          "[\u00D0\u00DE\u00FE\u010D\u010E\u010F\u0110\u0111\u0189\u0221\u0256\u0256\u0257\u0369\u0392",
          "\u0434\u0500\u2145\u2146]"
        ),
        e = paste0(
          "[\u00C8\u00C9\u00CA\u00CB\u00E8\u00E9\u00EA\u00EB\u0112\u0113\u0114\u0115\u0116\u0117\u0118",
          "\u0119\u011A\u011B\u018E\u018F\u0190\u0204\u0205\u0206\u0207\u0228\u0229\u0246\u0247\u0258\u0259\u0364",
          "\u0388\u0395\u0400\u0401\u0404\u0415\u0417\u0435\u0437\u0450\u0451\u0454\u0498\u0499\u2107\u2108\u2128",
          "\u212E\u212F\u2130\u2147]"
        ),
        f = "[\u0191\u0192\u0492\u0493\u2109\u2231\u2132\u214E]",
        g = "[\u011C\u011D\u011E\u011F\u0120\u0121\u0122\u0123\u0193\u0222\u0260\u0261\u0262\u210A\u2141]",
        h = "[\u0124\u0125\u0127\u0195\u0266\u0267\u0389\u0397\u0452\u210B\u210C\u210D\u210E\u210F]",
        i = paste0(
          "[\u00CC\u00CD\u00CE\u00CF\u00EC\u00ED\u00EE\u00EF\u0128\u0129\u012A\u012B\u012C\u012D\u012E\u012F",
          "\u0130\u0131\u0197\u019A\u0208\u0209\u0365\u0390\u0399\u0406\u0407\u0456\u0457]"
        ),
        j = "[\u0135\u0236\u0237\u0248\u0249\u0408\u0458\u2129\u2139\u2149]",
        k = "[\u0137\u0138\u0198\u0199\u212A]",
        l = "[\u0139\u013A\u013B\u013C\u013D\u013E\u013F\u0140\u0141\u0142\u0234]",
        m = "[\u0271\u0460\u2133]",
        n = paste0(
          "[\u00D1\u00F1\u0143\u0144\u0145\u0146\u0147\u0148\u0149\u014A\u014B\u0220\u0235\u0272\u0273",
          "\u0274\u0376\u0377\u0418\u0419\u0438\u0439\u2115\u2135]"
        ),
        h = "\u0149",
        o = paste0(
          "[\u00D2\u00D3\u00D4\u00D5\u00D6\u00D8\u00F0\u00F2\u00F3\u00F4\u00F5\u00F6\u00F8\u014C\u014D",
          "\u014E\u014F\u0150\u0151\u0150\u0151\u0230\u0231\u0275\u0298\u0366\u0398\u0424\u0444\u0472\u0473\u2134]"
        ),
        p = "[\u0420\u0440\u2117\u2118\u2119]",
        q = "[\u018D\u211A\u213A]",
        r = paste0(
          "[\u0154\u0155\u0156\u0157\u0158\u0159\u0211\u0212\u0213\u0279\u0280\u0281\u0433\u0453\u0490",
          "\u0491\u211B\u211C\u211D\u211F\u213E]"
        ),
        s = "[\u015A\u015C\u015D\u015E\u015F\u0160\u0161\u0160\u0161\u0218\u0219\u0405\u0455]",
        t = "[\u0162\u0163\u0164\u0165\u0166\u0167\u0371\u0373\u0422\u0442]",
        u = paste0(
          "[\u00D9\u00DA\u00DB\u00DC\u00F9\u00FA\u00FB\u00FC\u00FC\u0168\u0169\u016A\u016B\u016C\u016D",
          "\u016E\u016F\u0170\u0171\u0172\u0173\u01D3\u01D4\u01D5\u01D6\u01D7\u01D8\u01D9\u01DA\u01DB\u01DC\u0214",
          "\u0217\u0244\u0289\u0367\u0426\u0446]"
        ),
        v = "[\u0474\u0475\u0476\u0477]",
        w = "[\u0174\u0175\u0270\u0428\u0429\u0448\u0449\u0461]",
        y = "[\u00DD\u00FD\u00FF\u0176\u0177\u0178\u0232\u0233\u0423\u0427\u0443\u0447]",
        z = "[\u0179\u017A\u017B\u017C\u017E\u0224\u0225\u0240\u0290\u0291\u0396\u2124]",
        x = "[\u00D7\u0416\u0425\u0436\u0445\u0496\u0497]"
      ),
      SYMBOLS = c(
        `(cc)` = "\u00A9",
        number = "\u2116",
        sm = "\u2120",
        tel = "\u2121",
        `(tm)` = "\u2122",
        omega = "\u2126",
        alpha = "\u2127",
        fax = "\u213B",
        pi = "[\u213C\u213F]",
        sigma = "\u2140"
      )
    )
  )
  if (length(cats) == 0) cats <- names(dict)[-length(dict)]
  if (length(cats) == 1 && grepl("\\(|\\[", cats)) cats <- eval(parse(text = cats))
  if (any(grepl("[0-9]|seq", cats))) cats <- if (length(cats) > 1) as.numeric(cats) else eval(parse(text = cats))
  if (is.numeric(cats)) {
    cats <- cats[cats < length(dict)]
  } else if (any(!cats %in% names(dict))) cats <- grep(paste(paste0("^", cats), collapse = "|"), names(dict), value = TRUE)
  if (length(cats) == 0) {
    stop(
      "\n  enter numbers between 1 and ", length(dict) - 1,
      ", or letters matching a category:\n  ", paste(names(dict), collapse = ", ")
    )
  }
  if ("special" %in% cats) as.regex <- TRUE
  if (as.regex) {
    if (!missing(as.function)) {
      if ("special" %in% cats && is.function(as.function) && grepl("sub", substitute(as.function))) {
        dict <- c(dict$special$CHARACTERS, dict$special$SYMBOLS)
        fun <- as.function
        if (substitute(as.function) == "gsub") {
          charmap <- as.data.frame(unlist(lapply(as.list(dict), strsplit, "")), stringsAsFactors = FALSE)
          charmap <- data.frame(to = sub("[0-9]+", "", rownames(charmap)), from = charmap[[1]], stringsAsFactors = FALSE)
          charmap <- charmap[grepl("^\\w$", charmap$to) & !charmap$from %in% c("[", "]"), ]
          dict <- dict[!names(dict) %in% charmap$to]
          charmap <- list(to = paste(charmap$to, collapse = ""), from = paste(charmap$from, collapse = ""))
        } else {
          charmap <- NULL
        }
        function(terms, ...) {
          args <- list(...)
          args$x <- terms
          if (!is.null(charmap)) {
            args$x <- tryCatch(chartr(charmap$from, charmap$to, args$x), error = function(e) NULL)
            if (is.null(args$x)) {
              args$x <- chartr(charmap$from, charmap$to, iconv(terms, sub = "#"))
              warning("the input appears to be misencoded; it was converted, but may have errant #s")
            }
          }
          for (s in names(dict)) {
            args$pattern <- dict[s]
            args$replacement <- s
            args$x <- do.call(fun, args)
          }
          args$x
        }
      } else {
        dict <- paste(unlist(dict[cats]), collapse = "|")
        fun <- if (is.function(as.function)) as.function else grepl
        function(terms, ...) {
          args <- list(...)
          args$pattern <- dict
          args$x <- terms
          if (!is.function(as.function) && !"perl" %in% names(args)) args$perl <- TRUE
          do.call(fun, args)
        }
      }
    } else {
      dict[cats]
    }
  } else {
    lapply(dict[cats], function(l) gsub("\\^|\\$", "", sub("(?<=[^$])$", "*", l, perl = TRUE)))
  }
}
