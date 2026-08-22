# English Function Word Category and Special Character Lists

Returns a list of function words based on the Linguistic Inquiry and
Word Count 2015 dictionary (in terms of category names – words were
selected independently), or a list of special characters and patterns.

## Usage

``` r
lma_dict(..., as.regex = TRUE, as.function = FALSE)
```

## Arguments

- ...:

  Numbers or letters corresponding to category names: ppron, ipron,
  article, adverb, conj, prep, auxverb, negate, quant, interrog, number,
  interjection, or special.

- as.regex:

  Logical: if `FALSE`, lists are returned without regular expression.

- as.function:

  Logical or a function: if specified and `as.regex` is `TRUE`, the
  selected dictionary will be collapsed to a regex string (terms
  separated by `|`), and a function for matching characters to that
  string will be returned. The regex string is passed to the matching
  function ([`grepl`](https://rdrr.io/r/base/grep.html) by default) as a
  'pattern' argument, with the first argument of the returned function
  being passed as an 'x' argument. See examples.

## Value

A list with a vector of terms for each category, or (when
`as.function = TRUE`) a function which accepts an initial "terms"
argument (a character vector), and any additional arguments determined
by function entered as `as.function`
([`grepl`](https://rdrr.io/r/base/grep.html) by default).

## Note

The `special` category is not returned unless specifically requested. It
is a list of regular expression strings attempting to capture special
things like ellipses and emojis, or sets of special characters (those
outside of the Basic Latin range; `[^\u0020-\u007F]`), which can be used
for character conversions. If `special` is part of the returned list,
`as.regex` is set to `TRUE`.

The `special` list is always used by both
[`lma_dtm`](https://miserman.github.io/lingmatch/reference/lma_dtm.md)
and
[`lma_termcat`](https://miserman.github.io/lingmatch/reference/lma_termcat.md).
When creating a dtm, `special` is used to clean the original input (so
that, by default, the punctuation involved in ellipses and emojis are
treated as different – as ellipses and emojis rather than as periods and
parens and colons and such). When categorizing a dtm, the input
dictionary is passed by the special lists to be sure the terms in the
dtm match up with the dictionary (so, for example, ": (" would be
replaced with "repfrown" in both the text and dictionary).

## See also

To score texts with these categories, use
[`lma_termcat()`](https://miserman.github.io/lingmatch/reference/lma_termcat.md).

## Examples

``` r
# return the full dictionary (excluding special)
lma_dict()
#> $ppron
#>  [1] "^dae$"        "^dem$"        "^eir$"        "^eirself$"    "^em$"        
#>  [6] "^he$"         "^he'"         "^her$"        "^hers$"       "^herself$"   
#> [11] "^hes$"        "^him$"        "^himself$"    "^hir$"        "^hirs$"      
#> [16] "^hirself$"    "^his$"        "^hisself$"    "^i$"          "^i'"         
#> [21] "^id$"         "^idc$"        "^idgaf$"      "^idk$"        "^idontknow$" 
#> [26] "^idve$"       "^iirc$"       "^iknow$"      "^ikr$"        "^ill$"       
#> [31] "^ily$"        "^im$"         "^ima$"        "^imean$"      "^imma$"      
#> [36] "^ive$"        "^lets$"       "^let's$"      "^me$"         "^methinks$"  
#> [41] "^mine$"       "^my$"         "^myself$"     "^omfg$"       "^omg$"       
#> [46] "^oneself$"    "^our$"        "^ours"        "^she$"        "^she'"       
#> [51] "^shes$"       "^thee$"       "^their$"      "^their'"      "^theirs"     
#> [56] "^them$"       "^thems"       "^they$"       "^they'"       "^theyd$"     
#> [61] "^theyll$"     "^theyve$"     "^thine$"      "^thou$"       "^thoust$"    
#> [66] "^thy$"        "^thyself$"    "^u$"          "^u'"          "^ud$"        
#> [71] "^ull$"        "^ur$"         "^ure$"        "^us$"         "^we$"        
#> [76] "^we'"         "^weve$"       "^y'"          "^ya'"         "^yall"       
#> [81] "^yins$"       "^yinz$"       "^you$"        "^you'"        "^youd$"      
#> [86] "^youll$"      "^your$"       "^youre$"      "^yours$"      "^yourself$"  
#> [91] "^yourselves$" "^youve$"      "^zer$"        "^zir$"        "^zirs$"      
#> [96] "^zirself$"    "^zis$"       
#> 
#> $ipron
#>  [1] "^another$"   "^anybo"      "^anyone"     "^anything"   "^dat$"      
#>  [6] "^de+z$"      "^dis$"       "^everyb"     "^everyone"   "^everything"
#> [11] "^few$"       "^it$"        "^it'$"       "^it'"        "^itd$"      
#> [16] "^itll$"      "^its$"       "^itself$"    "^many$"      "^nobod"     
#> [21] "^nothing$"   "^other$"     "^others$"    "^same$"      "^somebo"    
#> [26] "^somebody'"  "^someone"    "^something"  "^stuff$"     "^that$"     
#> [31] "^that'"      "^thatd$"     "^thatll$"    "^thats$"     "^these$"    
#> [36] "^these'"     "^thesed$"    "^thesell$"   "^thesere$"   "^thing"     
#> [41] "^this$"      "^this'"      "^thisd$"     "^thisll$"    "^those$"    
#> [46] "^those'"     "^thosed$"    "^thosell$"   "^thosere$"   "^what$"     
#> [51] "^what'"      "^whatd$"     "^whatever$"  "^whatll$"    "^whats$"    
#> [56] "^which"      "^who$"       "^who'"       "^whod$"      "^whoever$"  
#> [61] "^wholl$"     "^whom$"      "^whomever$"  "^whos$"      "^whose$"    
#> [66] "^whosever$"  "^whosoever$"
#> 
#> $article
#> [1] "^a$"   "^an$"  "^da$"  "^teh$" "^the$"
#> 
#> $adverb
#>   [1] "^absolutely$"      "^actively$"        "^actually$"       
#>   [4] "^afk$"             "^again$"           "^ago$"            
#>   [7] "^ahead$"           "^almost$"          "^already$"        
#>  [10] "^altogether$"      "^always$"          "^angrily$"        
#>  [13] "^anxiously$"       "^any$"             "^anymore$"        
#>  [16] "^anyway$"          "^anywhere$"        "^apparently$"     
#>  [19] "^automatically$"   "^away$"            "^awhile$"         
#>  [22] "^back$"            "^badly$"           "^barely$"         
#>  [25] "^basically$"       "^below$"           "^brietermsy$"     
#>  [28] "^carefully$"       "^causiously$"      "^certainly$"      
#>  [31] "^clearly$"         "^closely$"         "^coldly$"         
#>  [34] "^commonly$"        "^completely$"      "^constantly$"     
#>  [37] "^continually$"     "^correctly$"       "^coz$"            
#>  [40] "^currently$"       "^daily$"           "^deeply$"         
#>  [43] "^definitely$"      "^definitly$"       "^deliberately$"   
#>  [46] "^desperately$"     "^differently$"     "^directly$"       
#>  [49] "^early$"           "^easily$"          "^effectively$"    
#>  [52] "^elsewhere$"       "^enough$"          "^entirely$"       
#>  [55] "^equally$"         "^especially$"      "^essentially$"    
#>  [58] "^etc$"             "^even$"            "^eventually$"     
#>  [61] "^ever$"            "^every$"           "^everyday$"       
#>  [64] "^everywhere"       "^exactly$"         "^exclusively$"    
#>  [67] "^extremely$"       "^fairly$"          "^far$"            
#>  [70] "^finally$"         "^fortunately$"     "^frequently$"     
#>  [73] "^fully$"           "^further$"         "^generally$"      
#>  [76] "^gently$"          "^genuinely$"       "^good$"           
#>  [79] "^greatly$"         "^hardly$"          "^heavily$"        
#>  [82] "^hence$"           "^henceforth$"      "^hereafter$"      
#>  [85] "^herein$"          "^heretofore$"      "^hesitantly$"     
#>  [88] "^highly$"          "^hither$"          "^hopefully$"      
#>  [91] "^hotly$"           "^however$"         "^immediately$"    
#>  [94] "^importantly$"     "^increasingly$"    "^incredibly$"     
#>  [97] "^indeed$"          "^initially$"       "^instead$"        
#> [100] "^intensely$"       "^jus$"             "^just$"           
#> [103] "^largely$"         "^lately$"          "^least$"          
#> [106] "^legitimately$"    "^less$"            "^lightly$"        
#> [109] "^likely$"          "^literally$"       "^loudly$"         
#> [112] "^luckily$"         "^mainly$"          "^maybe$"          
#> [115] "^meanwhile$"       "^merely$"          "^more$"           
#> [118] "^moreover$"        "^most$"            "^mostly$"         
#> [121] "^much$"            "^namely$"          "^naturally$"      
#> [124] "^nearly$"          "^necessarily$"     "^nervously$"      
#> [127] "^never$"           "^nevertheless$"    "^no$"             
#> [130] "^nonetheless$"     "^normally$"        "^not$"            
#> [133] "^notwithstanding$" "^obviously$"       "^occasionally$"   
#> [136] "^often$"           "^once$"            "^only$"           
#> [139] "^originally$"      "^otherwise$"       "^overall$"        
#> [142] "^particularly$"    "^passionately$"    "^perfectly$"      
#> [145] "^perhaps$"         "^personally$"      "^physically$"     
#> [148] "^please$"          "^possibly$"        "^potentially$"    
#> [151] "^practically$"     "^presently$"       "^previously$"     
#> [154] "^primarily$"       "^probability$"     "^probably$"       
#> [157] "^profoundly$"      "^prolly$"          "^properly$"       
#> [160] "^quickly$"         "^quietly$"         "^quite$"          
#> [163] "^randomly$"        "^rarely$"          "^rather$"         
#> [166] "^readily$"         "^really$"          "^recently$"       
#> [169] "^regularly$"       "^relatively$"      "^respectively$"   
#> [172] "^right$"           "^roughly$"         "^sadly$"          
#> [175] "^seldomly$"        "^seriously$"       "^shortly$"        
#> [178] "^significantly$"   "^similarly$"       "^simply$"         
#> [181] "^slightly$"        "^slowly$"          "^so$"             
#> [184] "^some$"            "^somehow$"         "^sometimes$"      
#> [187] "^somewhat$"        "^somewhere$"       "^soon$"           
#> [190] "^specifically$"    "^still$"           "^strongly$"       
#> [193] "^subsequently$"    "^successfully$"    "^such$"           
#> [196] "^suddenly$"        "^supposedly$"      "^surely$"         
#> [199] "^surprisingly$"    "^technically$"     "^terribly$"       
#> [202] "^thence$"          "^thereafter$"      "^therefor$"       
#> [205] "^therefore$"       "^thither$"         "^thoroughly$"     
#> [208] "^thus$"            "^thusfar$"         "^thusly$"         
#> [211] "^together$"        "^too$"             "^totally$"        
#> [214] "^truly$"           "^typically$"       "^ultimately$"     
#> [217] "^uncommonly$"      "^unfortunately$"   "^unfortunatly$"   
#> [220] "^usually$"         "^vastly$"          "^very$"           
#> [223] "^virtually$"       "^well$"            "^whence$"         
#> [226] "^where"            "^wherefor"         "^whither$"        
#> [229] "^wholly$"          "^why$"             "^why'"            
#> [232] "^whyd$"            "^whys$"            "^widely$"         
#> [235] "^wither$"          "^yet$"            
#> 
#> $conj
#>  [1] "^also$"     "^altho$"    "^although$" "^and$"      "^b/c$"     
#>  [6] "^bc$"       "^because$"  "^besides$"  "^both$"     "^but$"     
#> [11] "^'cause$"   "^cos$"      "^cuz$"      "^either$"   "^else$"    
#> [16] "^except$"   "^for$"      "^how$"      "^how'"      "^howd$"    
#> [21] "^howll$"    "^hows$"     "^if$"       "^neither$"  "^nor$"     
#> [26] "^or$"       "^than$"     "^tho$"      "^though$"   "^unless$"  
#> [31] "^unlike$"   "^versus$"   "^vs$"       "^when$"     "^when'"    
#> [36] "^whenever$" "^whereas$"  "^whether$"  "^while$"    "^whilst$"  
#> 
#> $prep
#>  [1] "^about$"      "^above$"      "^abt$"        "^across$"     "^acrost$"    
#>  [6] "^afk$"        "^after$"      "^against$"    "^along$"      "^amid"       
#> [11] "^among"       "^around$"     "^as$"         "^at$"         "^atop$"      
#> [16] "^before$"     "^behind$"     "^beneath$"    "^beside$"     "^betwe"      
#> [21] "^beyond$"     "^by$"         "^despite$"    "^down$"       "^during$"    
#> [26] "^excluding$"  "^from$"       "^here$"       "^here'"       "^heres$"     
#> [31] "^in$"         "^including$"  "^inside$"     "^into$"       "^minus$"     
#> [36] "^near$"       "^now$"        "^of$"         "^off$"        "^on$"        
#> [41] "^onto$"       "^out$"        "^outside$"    "^over$"       "^plus$"      
#> [46] "^regarding$"  "^sans$"       "^since$"      "^then$"       "^there$"     
#> [51] "^there'"      "^thered$"     "^therell$"    "^theres$"     "^through$"   
#> [56] "^throughout$" "^thru$"       "^til$"        "^till$"       "^to$"        
#> [61] "^toward"      "^under$"      "^underneath$" "^until$"      "^untill$"    
#> [66] "^unto$"       "^up$"         "^upon$"       "^via$"        "^with$"      
#> [71] "^within$"     "^without$"    "^worth$"     
#> 
#> $auxverb
#>  [1] "^am$"        "^are$"       "^arent$"     "^aren't$"    "^be$"       
#>  [6] "^been$"      "^bein$"      "^being$"     "^brb$"       "^can$"      
#> [11] "^could$"     "^could'"     "^couldnt$"   "^couldn't$"  "^couldve$"  
#> [16] "^did$"       "^didnt$"     "^didn't$"    "^do$"        "^does$"     
#> [21] "^doesnt$"    "^doesn't$"   "^doing$"     "^dont$"      "^don't$"    
#> [26] "^had$"       "^hadnt$"     "^hadn't$"    "^has$"       "^hasnt$"    
#> [31] "^hasn't$"    "^have$"      "^havent$"    "^haven't$"   "^having$"   
#> [36] "^is$"        "^isnt$"      "^isn't$"     "^may$"       "^might$"    
#> [41] "^might'"     "^mightnt$"   "^mightn't$"  "^mightve$"   "^must$"     
#> [46] "^mustnt$"    "^mustn't$"   "^mustve$"    "^ought"      "^shant$"    
#> [51] "^shan't$"    "^sha'nt$"    "^shall$"     "^should$"    "^shouldnt$" 
#> [56] "^shouldn't$" "^shouldve$"  "^was$"       "^wasnt$"     "^wasn't$"   
#> [61] "^were$"      "^werent$"    "^weren't$"   "^will$"      "^would$"    
#> [66] "^would'"     "^wouldnt"    "^wouldn't"   "^wouldve$"  
#> 
#> $negate
#>  [1] "^ain't$"     "^aint$"      "^aren't$"    "^arent$"     "^can't$"    
#>  [6] "^cannot$"    "^cant$"      "^couldn't$"  "^couldnt$"   "^didn't$"   
#> [11] "^didnt$"     "^doesn't$"   "^doesnt$"    "^don't$"     "^dont$"     
#> [16] "^hadn't$"    "^hadnt$"     "^hasn't$"    "^hasnt$"     "^haven't$"  
#> [21] "^havent$"    "^idk$"       "^isn't$"     "^isnt$"      "^must'nt$"  
#> [26] "^mustn't$"   "^mustnt$"    "^nah"        "^need'nt$"   "^needn't$"  
#> [31] "^neednt$"    "^negat"      "^neither$"   "^never$"     "^no$"       
#> [36] "^nobod"      "^noes$"      "^none$"      "^nope$"      "^nor$"      
#> [41] "^not$"       "^nothing$"   "^nowhere$"   "^np$"        "^ought'nt$" 
#> [46] "^oughtn't$"  "^oughtnt$"   "^shant$"     "^shan't$"    "^sha'nt$"   
#> [51] "^should'nt$" "^shouldn't$" "^shouldnt$"  "^uh-uh$"     "^wasn't$"   
#> [56] "^wasnt$"     "^weren't$"   "^werent$"    "^without$"   "^won't$"    
#> [61] "^wont$"      "^wouldn't$"  "^wouldnt$"  
#> 
#> $quant
#>  [1] "^add$"       "^added$"     "^adding$"    "^adds$"      "^all$"      
#>  [6] "^allot$"     "^alot$"      "^amount$"    "^amounts$"   "^another$"  
#> [11] "^any$"       "^approximat" "^average$"   "^bit$"       "^bits$"     
#> [16] "^both$"      "^bunch$"     "^chapter$"   "^couple$"    "^doubl"     
#> [21] "^each$"      "^either$"    "^entire"     "^equal"      "^every$"    
#> [26] "^extra$"     "^few$"       "^fewer$"     "^fewest$"    "^group"     
#> [31] "^inequal"    "^least$"     "^less$"      "^lot$"       "^lotof$"    
#> [36] "^lots$"      "^lotsa$"     "^lotta$"     "^majority$"  "^many$"     
#> [41] "^mo$"        "^mo'"        "^more$"      "^most$"      "^much$"     
#> [46] "^mucho$"     "^multiple$"  "^nada$"      "^none$"      "^part$"     
#> [51] "^partly$"    "^percent"    "^piece$"     "^pieces$"    "^plenty$"   
#> [56] "^remaining$" "^sampl"      "^scarce$"    "^scarcer$"   "^scarcest$" 
#> [61] "^section$"   "^segment"    "^series$"    "^several"    "^single$"   
#> [66] "^singles$"   "^singly$"    "^some$"      "^somewhat$"  "^ton$"      
#> [71] "^tons$"      "^total$"     "^triple"     "^tripling$"  "^variety$"  
#> [76] "^various$"   "^whole$"    
#> 
#> $interrog
#>  [1] "^how$"       "^how'd$"     "^how're$"    "^how's$"     "^howd$"     
#>  [6] "^howre$"     "^hows$"      "^wat$"       "^wattt"      "^what$"     
#> [11] "^what'd$"    "^what'll$"   "^what're$"   "^what's$"    "^whatd$"    
#> [16] "^whatever$"  "^whatll$"    "^whatre$"    "^whatt"      "^when$"     
#> [21] "^when'"      "^whence$"    "^whenever$"  "^where$"     "^where'd$"  
#> [26] "^where's$"   "^wherefore$" "^wherever$"  "^whether$"   "^which$"    
#> [31] "^whichever$" "^whither$"   "^who$"       "^who'd$"     "^who'll$"   
#> [36] "^who's$"     "^whoever$"   "^wholl$"     "^whom$"      "^whomever$" 
#> [41] "^whos$"      "^whose$"     "^whosever$"  "^whoso"      "^why$"      
#> [46] "^why'"       "^whyever$"   "^wut$"      
#> 
#> $number
#>  [1] "^billion"  "^doubl"    "^dozen"    "^eight"    "^eleven$"  "^fift"    
#>  [7] "^first$"   "^firstly$" "^firsts$"  "^five$"    "^four"     "^half$"   
#> [13] "^hundred"  "^infinit"  "^million"  "^nine"     "^once$"    "^one$"    
#> [19] "^quarter"  "^second$"  "^seven"    "^single$"  "^six"      "^ten$"    
#> [25] "^tenth$"   "^third$"   "^thirt"    "^thousand" "^three$"   "^trillion"
#> [31] "^twel"     "^twent"    "^twice$"   "^two$"     "^zero$"    "^zillion" 
#> 
#> $interjection
#>  [1] "^a+h+$"       "^a+w+$"       "^allas$"      "^alright"     "^anyhoo$"    
#>  [6] "^anyway[ysz]" "^bl[eh]+$"    "^g+[eah]+$"   "^h[ah]+$"     "^h[hu]+$"    
#> [11] "^h[mh]+$"     "^l[ol]+$"     "^m[hm]+$"     "^meh$"        "^o+h+$"      
#> [16] "^o+k+$"       "^okie"        "^oo+f+$"      "^soo+$"       "^u[uh]+$"    
#> [21] "^u+g+h+$"     "^w[ow]+$"     "^wee+ll+$"    "^y[aes]+$"    "^ya+h+$"     
#> [26] "^yeah$"       "^yus+$"      
#> 

# return the standard 7 category lsm categories
lma_dict(1:7)
#> $ppron
#>  [1] "^dae$"        "^dem$"        "^eir$"        "^eirself$"    "^em$"        
#>  [6] "^he$"         "^he'"         "^her$"        "^hers$"       "^herself$"   
#> [11] "^hes$"        "^him$"        "^himself$"    "^hir$"        "^hirs$"      
#> [16] "^hirself$"    "^his$"        "^hisself$"    "^i$"          "^i'"         
#> [21] "^id$"         "^idc$"        "^idgaf$"      "^idk$"        "^idontknow$" 
#> [26] "^idve$"       "^iirc$"       "^iknow$"      "^ikr$"        "^ill$"       
#> [31] "^ily$"        "^im$"         "^ima$"        "^imean$"      "^imma$"      
#> [36] "^ive$"        "^lets$"       "^let's$"      "^me$"         "^methinks$"  
#> [41] "^mine$"       "^my$"         "^myself$"     "^omfg$"       "^omg$"       
#> [46] "^oneself$"    "^our$"        "^ours"        "^she$"        "^she'"       
#> [51] "^shes$"       "^thee$"       "^their$"      "^their'"      "^theirs"     
#> [56] "^them$"       "^thems"       "^they$"       "^they'"       "^theyd$"     
#> [61] "^theyll$"     "^theyve$"     "^thine$"      "^thou$"       "^thoust$"    
#> [66] "^thy$"        "^thyself$"    "^u$"          "^u'"          "^ud$"        
#> [71] "^ull$"        "^ur$"         "^ure$"        "^us$"         "^we$"        
#> [76] "^we'"         "^weve$"       "^y'"          "^ya'"         "^yall"       
#> [81] "^yins$"       "^yinz$"       "^you$"        "^you'"        "^youd$"      
#> [86] "^youll$"      "^your$"       "^youre$"      "^yours$"      "^yourself$"  
#> [91] "^yourselves$" "^youve$"      "^zer$"        "^zir$"        "^zirs$"      
#> [96] "^zirself$"    "^zis$"       
#> 
#> $ipron
#>  [1] "^another$"   "^anybo"      "^anyone"     "^anything"   "^dat$"      
#>  [6] "^de+z$"      "^dis$"       "^everyb"     "^everyone"   "^everything"
#> [11] "^few$"       "^it$"        "^it'$"       "^it'"        "^itd$"      
#> [16] "^itll$"      "^its$"       "^itself$"    "^many$"      "^nobod"     
#> [21] "^nothing$"   "^other$"     "^others$"    "^same$"      "^somebo"    
#> [26] "^somebody'"  "^someone"    "^something"  "^stuff$"     "^that$"     
#> [31] "^that'"      "^thatd$"     "^thatll$"    "^thats$"     "^these$"    
#> [36] "^these'"     "^thesed$"    "^thesell$"   "^thesere$"   "^thing"     
#> [41] "^this$"      "^this'"      "^thisd$"     "^thisll$"    "^those$"    
#> [46] "^those'"     "^thosed$"    "^thosell$"   "^thosere$"   "^what$"     
#> [51] "^what'"      "^whatd$"     "^whatever$"  "^whatll$"    "^whats$"    
#> [56] "^which"      "^who$"       "^who'"       "^whod$"      "^whoever$"  
#> [61] "^wholl$"     "^whom$"      "^whomever$"  "^whos$"      "^whose$"    
#> [66] "^whosever$"  "^whosoever$"
#> 
#> $article
#> [1] "^a$"   "^an$"  "^da$"  "^teh$" "^the$"
#> 
#> $adverb
#>   [1] "^absolutely$"      "^actively$"        "^actually$"       
#>   [4] "^afk$"             "^again$"           "^ago$"            
#>   [7] "^ahead$"           "^almost$"          "^already$"        
#>  [10] "^altogether$"      "^always$"          "^angrily$"        
#>  [13] "^anxiously$"       "^any$"             "^anymore$"        
#>  [16] "^anyway$"          "^anywhere$"        "^apparently$"     
#>  [19] "^automatically$"   "^away$"            "^awhile$"         
#>  [22] "^back$"            "^badly$"           "^barely$"         
#>  [25] "^basically$"       "^below$"           "^brietermsy$"     
#>  [28] "^carefully$"       "^causiously$"      "^certainly$"      
#>  [31] "^clearly$"         "^closely$"         "^coldly$"         
#>  [34] "^commonly$"        "^completely$"      "^constantly$"     
#>  [37] "^continually$"     "^correctly$"       "^coz$"            
#>  [40] "^currently$"       "^daily$"           "^deeply$"         
#>  [43] "^definitely$"      "^definitly$"       "^deliberately$"   
#>  [46] "^desperately$"     "^differently$"     "^directly$"       
#>  [49] "^early$"           "^easily$"          "^effectively$"    
#>  [52] "^elsewhere$"       "^enough$"          "^entirely$"       
#>  [55] "^equally$"         "^especially$"      "^essentially$"    
#>  [58] "^etc$"             "^even$"            "^eventually$"     
#>  [61] "^ever$"            "^every$"           "^everyday$"       
#>  [64] "^everywhere"       "^exactly$"         "^exclusively$"    
#>  [67] "^extremely$"       "^fairly$"          "^far$"            
#>  [70] "^finally$"         "^fortunately$"     "^frequently$"     
#>  [73] "^fully$"           "^further$"         "^generally$"      
#>  [76] "^gently$"          "^genuinely$"       "^good$"           
#>  [79] "^greatly$"         "^hardly$"          "^heavily$"        
#>  [82] "^hence$"           "^henceforth$"      "^hereafter$"      
#>  [85] "^herein$"          "^heretofore$"      "^hesitantly$"     
#>  [88] "^highly$"          "^hither$"          "^hopefully$"      
#>  [91] "^hotly$"           "^however$"         "^immediately$"    
#>  [94] "^importantly$"     "^increasingly$"    "^incredibly$"     
#>  [97] "^indeed$"          "^initially$"       "^instead$"        
#> [100] "^intensely$"       "^jus$"             "^just$"           
#> [103] "^largely$"         "^lately$"          "^least$"          
#> [106] "^legitimately$"    "^less$"            "^lightly$"        
#> [109] "^likely$"          "^literally$"       "^loudly$"         
#> [112] "^luckily$"         "^mainly$"          "^maybe$"          
#> [115] "^meanwhile$"       "^merely$"          "^more$"           
#> [118] "^moreover$"        "^most$"            "^mostly$"         
#> [121] "^much$"            "^namely$"          "^naturally$"      
#> [124] "^nearly$"          "^necessarily$"     "^nervously$"      
#> [127] "^never$"           "^nevertheless$"    "^no$"             
#> [130] "^nonetheless$"     "^normally$"        "^not$"            
#> [133] "^notwithstanding$" "^obviously$"       "^occasionally$"   
#> [136] "^often$"           "^once$"            "^only$"           
#> [139] "^originally$"      "^otherwise$"       "^overall$"        
#> [142] "^particularly$"    "^passionately$"    "^perfectly$"      
#> [145] "^perhaps$"         "^personally$"      "^physically$"     
#> [148] "^please$"          "^possibly$"        "^potentially$"    
#> [151] "^practically$"     "^presently$"       "^previously$"     
#> [154] "^primarily$"       "^probability$"     "^probably$"       
#> [157] "^profoundly$"      "^prolly$"          "^properly$"       
#> [160] "^quickly$"         "^quietly$"         "^quite$"          
#> [163] "^randomly$"        "^rarely$"          "^rather$"         
#> [166] "^readily$"         "^really$"          "^recently$"       
#> [169] "^regularly$"       "^relatively$"      "^respectively$"   
#> [172] "^right$"           "^roughly$"         "^sadly$"          
#> [175] "^seldomly$"        "^seriously$"       "^shortly$"        
#> [178] "^significantly$"   "^similarly$"       "^simply$"         
#> [181] "^slightly$"        "^slowly$"          "^so$"             
#> [184] "^some$"            "^somehow$"         "^sometimes$"      
#> [187] "^somewhat$"        "^somewhere$"       "^soon$"           
#> [190] "^specifically$"    "^still$"           "^strongly$"       
#> [193] "^subsequently$"    "^successfully$"    "^such$"           
#> [196] "^suddenly$"        "^supposedly$"      "^surely$"         
#> [199] "^surprisingly$"    "^technically$"     "^terribly$"       
#> [202] "^thence$"          "^thereafter$"      "^therefor$"       
#> [205] "^therefore$"       "^thither$"         "^thoroughly$"     
#> [208] "^thus$"            "^thusfar$"         "^thusly$"         
#> [211] "^together$"        "^too$"             "^totally$"        
#> [214] "^truly$"           "^typically$"       "^ultimately$"     
#> [217] "^uncommonly$"      "^unfortunately$"   "^unfortunatly$"   
#> [220] "^usually$"         "^vastly$"          "^very$"           
#> [223] "^virtually$"       "^well$"            "^whence$"         
#> [226] "^where"            "^wherefor"         "^whither$"        
#> [229] "^wholly$"          "^why$"             "^why'"            
#> [232] "^whyd$"            "^whys$"            "^widely$"         
#> [235] "^wither$"          "^yet$"            
#> 
#> $conj
#>  [1] "^also$"     "^altho$"    "^although$" "^and$"      "^b/c$"     
#>  [6] "^bc$"       "^because$"  "^besides$"  "^both$"     "^but$"     
#> [11] "^'cause$"   "^cos$"      "^cuz$"      "^either$"   "^else$"    
#> [16] "^except$"   "^for$"      "^how$"      "^how'"      "^howd$"    
#> [21] "^howll$"    "^hows$"     "^if$"       "^neither$"  "^nor$"     
#> [26] "^or$"       "^than$"     "^tho$"      "^though$"   "^unless$"  
#> [31] "^unlike$"   "^versus$"   "^vs$"       "^when$"     "^when'"    
#> [36] "^whenever$" "^whereas$"  "^whether$"  "^while$"    "^whilst$"  
#> 
#> $prep
#>  [1] "^about$"      "^above$"      "^abt$"        "^across$"     "^acrost$"    
#>  [6] "^afk$"        "^after$"      "^against$"    "^along$"      "^amid"       
#> [11] "^among"       "^around$"     "^as$"         "^at$"         "^atop$"      
#> [16] "^before$"     "^behind$"     "^beneath$"    "^beside$"     "^betwe"      
#> [21] "^beyond$"     "^by$"         "^despite$"    "^down$"       "^during$"    
#> [26] "^excluding$"  "^from$"       "^here$"       "^here'"       "^heres$"     
#> [31] "^in$"         "^including$"  "^inside$"     "^into$"       "^minus$"     
#> [36] "^near$"       "^now$"        "^of$"         "^off$"        "^on$"        
#> [41] "^onto$"       "^out$"        "^outside$"    "^over$"       "^plus$"      
#> [46] "^regarding$"  "^sans$"       "^since$"      "^then$"       "^there$"     
#> [51] "^there'"      "^thered$"     "^therell$"    "^theres$"     "^through$"   
#> [56] "^throughout$" "^thru$"       "^til$"        "^till$"       "^to$"        
#> [61] "^toward"      "^under$"      "^underneath$" "^until$"      "^untill$"    
#> [66] "^unto$"       "^up$"         "^upon$"       "^via$"        "^with$"      
#> [71] "^within$"     "^without$"    "^worth$"     
#> 
#> $auxverb
#>  [1] "^am$"        "^are$"       "^arent$"     "^aren't$"    "^be$"       
#>  [6] "^been$"      "^bein$"      "^being$"     "^brb$"       "^can$"      
#> [11] "^could$"     "^could'"     "^couldnt$"   "^couldn't$"  "^couldve$"  
#> [16] "^did$"       "^didnt$"     "^didn't$"    "^do$"        "^does$"     
#> [21] "^doesnt$"    "^doesn't$"   "^doing$"     "^dont$"      "^don't$"    
#> [26] "^had$"       "^hadnt$"     "^hadn't$"    "^has$"       "^hasnt$"    
#> [31] "^hasn't$"    "^have$"      "^havent$"    "^haven't$"   "^having$"   
#> [36] "^is$"        "^isnt$"      "^isn't$"     "^may$"       "^might$"    
#> [41] "^might'"     "^mightnt$"   "^mightn't$"  "^mightve$"   "^must$"     
#> [46] "^mustnt$"    "^mustn't$"   "^mustve$"    "^ought"      "^shant$"    
#> [51] "^shan't$"    "^sha'nt$"    "^shall$"     "^should$"    "^shouldnt$" 
#> [56] "^shouldn't$" "^shouldve$"  "^was$"       "^wasnt$"     "^wasn't$"   
#> [61] "^were$"      "^werent$"    "^weren't$"   "^will$"      "^would$"    
#> [66] "^would'"     "^wouldnt"    "^wouldn't"   "^wouldve$"  
#> 

# return just a few categories without regular expression
lma_dict(neg, ppron, aux, as.regex = FALSE)
#> $ppron
#>  [1] "dae"        "dem"        "eir"        "eirself"    "em"        
#>  [6] "he"         "he'*"       "her"        "hers"       "herself"   
#> [11] "hes"        "him"        "himself"    "hir"        "hirs"      
#> [16] "hirself"    "his"        "hisself"    "i"          "i'*"       
#> [21] "id"         "idc"        "idgaf"      "idk"        "idontknow" 
#> [26] "idve"       "iirc"       "iknow"      "ikr"        "ill"       
#> [31] "ily"        "im"         "ima"        "imean"      "imma"      
#> [36] "ive"        "lets"       "let's"      "me"         "methinks"  
#> [41] "mine"       "my"         "myself"     "omfg"       "omg"       
#> [46] "oneself"    "our"        "ours*"      "she"        "she'*"     
#> [51] "shes"       "thee"       "their"      "their'*"    "theirs*"   
#> [56] "them"       "thems*"     "they"       "they'*"     "theyd"     
#> [61] "theyll"     "theyve"     "thine"      "thou"       "thoust"    
#> [66] "thy"        "thyself"    "u"          "u'*"        "ud"        
#> [71] "ull"        "ur"         "ure"        "us"         "we"        
#> [76] "we'*"       "weve"       "y'*"        "ya'*"       "yall*"     
#> [81] "yins"       "yinz"       "you"        "you'*"      "youd"      
#> [86] "youll"      "your"       "youre"      "yours"      "yourself"  
#> [91] "yourselves" "youve"      "zer"        "zir"        "zirs"      
#> [96] "zirself"    "zis"       
#> 
#> $auxverb
#>  [1] "am"        "are"       "arent"     "aren't"    "be"        "been"     
#>  [7] "bein"      "being"     "brb"       "can"       "could"     "could'*"  
#> [13] "couldnt"   "couldn't"  "couldve"   "did"       "didnt"     "didn't"   
#> [19] "do"        "does"      "doesnt"    "doesn't"   "doing"     "dont"     
#> [25] "don't"     "had"       "hadnt"     "hadn't"    "has"       "hasnt"    
#> [31] "hasn't"    "have"      "havent"    "haven't"   "having"    "is"       
#> [37] "isnt"      "isn't"     "may"       "might"     "might'*"   "mightnt"  
#> [43] "mightn't"  "mightve"   "must"      "mustnt"    "mustn't"   "mustve"   
#> [49] "ought*"    "shant"     "shan't"    "sha'nt"    "shall"     "should"   
#> [55] "shouldnt"  "shouldn't" "shouldve"  "was"       "wasnt"     "wasn't"   
#> [61] "were"      "werent"    "weren't"   "will"      "would"     "would'*"  
#> [67] "wouldnt*"  "wouldn't*" "wouldve"  
#> 
#> $negate
#>  [1] "ain't"     "aint"      "aren't"    "arent"     "can't"     "cannot"   
#>  [7] "cant"      "couldn't"  "couldnt"   "didn't"    "didnt"     "doesn't"  
#> [13] "doesnt"    "don't"     "dont"      "hadn't"    "hadnt"     "hasn't"   
#> [19] "hasnt"     "haven't"   "havent"    "idk"       "isn't"     "isnt"     
#> [25] "must'nt"   "mustn't"   "mustnt"    "nah*"      "need'nt"   "needn't"  
#> [31] "neednt"    "negat*"    "neither"   "never"     "no"        "nobod*"   
#> [37] "noes"      "none"      "nope"      "nor"       "not"       "nothing"  
#> [43] "nowhere"   "np"        "ought'nt"  "oughtn't"  "oughtnt"   "shant"    
#> [49] "shan't"    "sha'nt"    "should'nt" "shouldn't" "shouldnt"  "uh-uh"    
#> [55] "wasn't"    "wasnt"     "weren't"   "werent"    "without"   "won't"    
#> [61] "wont"      "wouldn't"  "wouldnt"  
#> 

# return special specifically
lma_dict(special)
#> $special
#> $special$ELLIPSIS
#> [1] "\\.{3, }|\\. +\\. +[. ]+"
#> 
#> $special$SMILE
#> [1] "\\s(?:[[{(<qd]+[\\s<-]*[;:8=]|[;:8=][\\s>-]*[]})>Dpb]+|[uUnwWmM^=+-]_[uUnwWmM^=+-])(?=\\s)"
#> 
#> $special$FROWN
#> [1] "\\s(?:[]D)}>]+[\\s.,<-]*[;:8=]|[;:8=][\\s.,>-]*[[{(<]+|[Tt:;]_[Tt;:]|[uUtT;:][mMn][uUtT;:])(?=\\s)"
#> 
#> $special$LIKE
#>  [1] "(?<=could not) like\\b"  "(?<=did not) like\\b"   
#>  [3] "(?<=did) like\\b"        "(?<=didn't) like\\b"    
#>  [5] "(?<=do not) like\\b"     "(?<=do) like\\b"        
#>  [7] "(?<=does not) like\\b"   "(?<=does) like\\b"      
#>  [9] "(?<=doesn't) like\\b"    "(?<=don't) like\\b"     
#> [11] "(?<=i) like\\b"          "(?<=should not) like\\b"
#> [13] "(?<=they) like\\b"       "(?<=we) like\\b"        
#> [15] "(?<=will not) like\\b"   "(?<=will) like\\b"      
#> [17] "(?<=won't) like\\b"      "(?<=would not) like\\b" 
#> [19] "(?<=you) like\\b"       
#> 
#> $special$CHARACTERS
#>                                                          
#>                                                    "\\s" 
#>                                                        ' 
#>                                      "[´‘’‚‛′‵ʹʻʾʿˈˊˋ˴̡̢̨̛̦̩̀́̍̒̓̔̀́̓͑͗̕]" 
#>                                                        " 
#>                                       "[“”„‟″‴‶‷⁗ʺ˝ˮ˵˶̋̏]" 
#>                                                      ... 
#>                                                      "…" 
#>                                                        - 
#>                                          "[־᠆‐‑–﹘﹣－]" 
#>                                                       -  
#>                                            "[‒—―⸺⸻]|--+" 
#>                                                        a 
#>                    "[ÀÁÂÃÄÅàáâãäåĀāĂăĄąȀȁȂȃȦȧɅɐɑɒɕͣΆΑАа]" 
#>                                                       ae 
#>                                                "[ÆæŒœɶ]" 
#>                                                        b 
#>                           "[ßƀƁƂƃƄƅƆƇƈƉƊƋƌɃɓʙБВбвѢѣҔҕℬ]" 
#>                                                        c 
#>                                      "[ÇçĆćĈĉƆƇƈɔʗͨСсℂ℃]" 
#>                                                        d 
#>                                   "[ÐÞþčĎďĐđƉȡɖɖɗͩΒдԀⅅⅆ]" 
#>                                                        e 
#> "[ÈÉÊËèéêëĒēĔĕĖėĘęĚěƎƏƐȄȅȆȇȨȩɆɇɘəͤΈΕЀЁЄЕЗезѐёєҘҙℇ℈ℨ℮ℯℰⅇ]" 
#>                                                        f 
#>                                             "[ƑƒҒғ℉∱Ⅎⅎ]" 
#>                                                        g 
#>                                      "[ĜĝĞğĠġĢģƓȢɠɡɢℊ⅁]" 
#>                                                        h 
#>                                       "[ĤĥħƕɦɧΉΗђℋℌℍℎℏ]" 
#>                                                        i 
#>                         "[ÌÍÎÏìíîïĨĩĪīĬĭĮįİıƗƚȈȉͥΐΙІЇії]" 
#>                                                        j 
#>                                           "[ĵȶȷɈɉЈј℩ℹⅉ]" 
#>                                                        k 
#>                                                "[ķĸƘƙK]" 
#>                                                        l 
#>                                          "[ĹĺĻļĽľĿŀŁłȴ]" 
#>                                                        m 
#>                                                  "[ɱѠℳ]" 
#>                                                        n 
#>                             "[ÑñŃńŅņŇňŉŊŋȠȵɲɳɴͶͷИЙийℕℵ]" 
#>                                                        h 
#>                                                      "ŉ" 
#>                                                        o 
#>                      "[ÒÓÔÕÖØðòóôõöøŌōŎŏŐőŐőȰȱɵʘͦΘФфѲѳℴ]" 
#>                                                        p 
#>                                                "[Рр℗℘ℙ]" 
#>                                                        q 
#>                                                  "[ƍℚ℺]" 
#>                                                        r 
#>                                "[ŔŕŖŗŘřȑȒȓɹʀʁгѓҐґℛℜℝ℟ℾ]" 
#>                                                        s 
#>                                        "[ŚŜŝŞşŠšŠšȘșЅѕ]" 
#>                                                        t 
#>                                           "[ŢţŤťŦŧͱͳТт]" 
#>                                                        u 
#>                "[ÙÚÛÜùúûüüŨũŪūŬŭŮůŰűŲųǓǔǕǖǗǘǙǚǛǜȔȗɄʉͧЦц]" 
#>                                                        v 
#>                                                 "[ѴѵѶѷ]" 
#>                                                        w 
#>                                             "[ŴŵɰШЩшщѡ]" 
#>                                                        y 
#>                                         "[ÝýÿŶŷŸȲȳУЧуч]" 
#>                                                        z 
#>                                         "[ŹźŻżžȤȥɀʐʑΖℤ]" 
#>                                                        x 
#>                                              "[×ЖХжхҖҗ]" 
#> 
#> $special$SYMBOLS
#>   (cc) number     sm    tel   (tm)  omega  alpha    fax     pi  sigma 
#>    "©"    "№"    "℠"    "℡"    "™"    "Ω"    "℧"    "℻" "[ℼℿ]"    "⅀" 
#> 
#> 

# returning a function
is.ppron <- lma_dict(ppron, as.function = TRUE)
is.ppron(c("i", "am", "you", "were"))
#> [1]  TRUE FALSE  TRUE FALSE

in.lsmcat <- lma_dict(1:7, as.function = TRUE)
in.lsmcat(c("a", "frog", "for", "me"))
#> [1]  TRUE FALSE  TRUE  TRUE

## use as a stopword filter
is.stopword <- lma_dict(as.function = TRUE)
dtm <- lma_dtm("Most of these words might not be all that relevant.")
dtm[, !is.stopword(colnames(dtm))]
#> relevant    words 
#>        1        1 

## use to replace special characters
clean <- lma_dict(special, as.function = gsub)
clean(c(
  "\u201Ccurly quotes\u201D", "na\u00EFve", "typographer\u2019s apostrophe",
  "en\u2013dash", "em\u2014dash"
))
#> [1] "\"curly quotes\""         "naive"                   
#> [3] "typographer's apostrophe" "en-dash"                 
#> [5] "em - dash"               
```
