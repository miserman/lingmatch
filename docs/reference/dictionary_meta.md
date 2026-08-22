# Assess Dictionary Categories Within a Latent Semantic Space

Assess Dictionary Categories Within a Latent Semantic Space

## Usage

``` r
dictionary_meta(dict, space = "auto", n_spaces = 5, suggest = FALSE,
  suggestion_terms = 10, suggest_stopwords = FALSE,
  suggest_discriminate = TRUE, expand_cutoff_freq = 0.98,
  expand_cutoff_spaces = 10, dimension_prop = 1, pairwise = TRUE,
  glob = TRUE, space_dir = getOption("lingmatch.lspace.dir"),
  verbose = TRUE)
```

## Arguments

- dict:

  A vector of terms, list of such vectors, or a matrix-like object to be
  categorized by
  [`read.dic`](https://miserman.github.io/lingmatch/reference/read.dic.md).

- space:

  A vector space used to calculate similarities between terms. Names of
  spaces (see
  [`select.lspace`](https://miserman.github.io/lingmatch/reference/select.lspace.md)),
  a matrix with terms as row names, or `"auto"` to auto-select a space
  based on matched terms. This can also be `multi` to use multiple
  spaces, which are combined after similarities are calculated.

- n_spaces:

  Number of spaces to draw from if `space` is `multi`.

- suggest:

  Logical; if `TRUE`, will search for other terms for possible inclusion
  in `space`.

- suggestion_terms:

  Number of terms to use when selecting suggested additions.

- suggest_stopwords:

  Logical; if `TRUE`, will suggest function words.

- suggest_discriminate:

  Logical; if `TRUE`, will adjust for similarity to other categories
  when finding suggestions.

- expand_cutoff_freq:

  Proportion of mapped terms to include when expanding dictionary terms.
  Applies when `space` is a character (referring to a space to be
  loaded).

- expand_cutoff_spaces:

  Number of spaces in which a term has to appear to be considered for
  expansion. Applies when `space` is a character (referring to a space
  to be loaded).

- dimension_prop:

  Proportion of dimensions to use when searching for suggested
  additions, where less than 1 will calculate similarities to the
  category core using fewer dimensions of the space.

- pairwise:

  Logical; if `FALSE`, will compare candidate suggestion terms with a
  single, averaged category vector rather than all category terms
  separately.

- glob:

  Logical; if `TRUE`, converts globs (asterisk wildcards) to regular
  expressions.

- space_dir:

  Directory from which `space` should be loaded.

- verbose:

  Logical; if `FALSE`, will not show status messages.

## Value

A list:

- **`expanded`**: A version of `dict` with fuzzy terms expanded.

- **`summary`**: A summary of each dictionary category.

- **`terms`**: Match (expanded term) similarities within terms and
  categories.

- **`suggested`**: If `suggest` is `TRUE`, a list with suggested
  additions for each dictionary category. Each entry is a named numeric
  vector with similarities for each suggested term.

## See also

To just expand fuzzy terms, see
[`report_term_matches()`](https://miserman.github.io/lingmatch/reference/report_term_matches.md).

Similar information is provided in the [dictionary
builder](https://miserman.github.io/dictionary_builder/) web tool.

Other Dictionary functions:
[`download.dict()`](https://miserman.github.io/lingmatch/reference/download.dict.md),
[`lma_patcat()`](https://miserman.github.io/lingmatch/reference/lma_patcat.md),
[`lma_termcat()`](https://miserman.github.io/lingmatch/reference/lma_termcat.md),
[`read.dic()`](https://miserman.github.io/lingmatch/reference/read.dic.md),
[`report_term_matches()`](https://miserman.github.io/lingmatch/reference/report_term_matches.md),
[`select.dict()`](https://miserman.github.io/lingmatch/reference/select.dict.md)

## Examples

``` r
if (dir.exists("~/Latent Semantic Spaces")) {
  dict <- list(
    furniture = c("table", "chair", "desk*", "couch*", "sofa*"),
    well_adjusted = c("happy", "bright*", "friend*", "she", "he", "they")
  )
  dictionary_meta(dict, space_dir = "~/Latent Semantic Spaces")
}
#> preparing terms (0)
#> expanding terms (1.14)
#> loading space (1.28)
#> calculating term similarities (2.58)
#> preparing results (2.58)
#> done (2.58)
#> $expanded
#> $expanded$furniture
#>  [1] "table"        "chair"        "desk-top"     "desk"         "desking"     
#>  [6] "deskpro"      "deskilled"    "desktop"      "desktops"     "desks"       
#> [11] "deskjet"      "deskbound"    "deskins"      "deskilling"   "desker"      
#> [16] "deskside"     "deskstar"     "couchdb"      "couchant"     "couchsurfing"
#> [21] "couche"       "couchette"    "couchman"     "couching"     "couched"     
#> [26] "coucher"      "couches"      "couch"        "sofaer"       "sofabed"     
#> [31] "sofala"       "sofas"        "sofar"        "sofa"        
#> 
#> $expanded$well_adjusted
#>  [1] "happy"         "bright-eyed"   "brightmail"    "brightcove"   
#>  [5] "brightling"    "brightnesses"  "brightwater"   "brightpoint"  
#>  [9] "brightstar"    "brightwell"    "brighter"      "brighton"     
#> [13] "brightfield"   "brightest"     "brightwork"    "brighten"     
#> [17] "brightside"    "brightly"      "brightness"    "brightening"  
#> [21] "brightened"    "brightwood"    "brighthouse"   "brightman"    
#> [25] "brightlingsea" "bright"        "brights"       "brightens"    
#> [29] "friendz"       "friendlies"    "friend"        "friendly"     
#> [33] "friendfeed"    "friendswood"   "friendliness"  "friendster"   
#> [37] "friendship"    "friends"       "friendlier"    "friendliest"  
#> [41] "friendless"    "friendships"   "friended"      "friending"    
#> [45] "she"           "he"            "they"         
#> 
#> 
#> $summary
#>                    category n_terms n_expanded   sim.space     sim.min
#> furniture         furniture       5         34 glove_crawl -0.03352657
#> well_adjusted well_adjusted       6         47 glove_crawl -0.01389545
#>                   sim.q1 sim.median   sim.mean     sim.q3   sim.max
#> furniture     0.01484553 0.03518088 0.05212709 0.08518134 0.1520343
#> well_adjusted 0.01743533 0.07671837 0.08098433 0.13679356 0.1828150
#> 
#> $terms
#>           category    term         match      sim.term sim.category
#> 1        furniture   table         table  1.000000e+00  0.520288056
#> 2        furniture   chair         chair  1.000000e+00  0.643952092
#> 3        furniture   desk*      desk-top  2.175893e-01  0.019619600
#> 3.1      furniture   desk*          desk  1.000000e+00  0.543019756
#> 3.2      furniture   desk*       desking  2.288528e-01  0.214574317
#> 3.3      furniture   desk*       deskpro  6.674267e-02 -0.029996732
#> 3.4      furniture   desk*     deskilled -4.967378e-02 -0.139120978
#> 3.5      furniture   desk*       desktop  4.811437e-01  0.196892375
#> 3.6      furniture   desk*      desktops  2.660376e-01  0.120308960
#> 3.7      furniture   desk*         desks  7.055135e-01  0.488119728
#> 3.8      furniture   desk*       deskjet  6.666372e-02  0.014096537
#> 3.9      furniture   desk*     deskbound -3.295457e-02  0.040480064
#> 3.10     furniture   desk*       deskins -7.416633e-02 -0.093441679
#> 3.11     furniture   desk*    deskilling -1.111884e-01 -0.127046688
#> 3.12     furniture   desk*        desker -8.782659e-02 -0.101063485
#> 3.13     furniture   desk*      deskside  1.018355e-01 -0.018340077
#> 3.14     furniture   desk*      deskstar  3.228949e-02 -0.077787116
#> 4        furniture  couch*       couchdb  8.856923e-02  0.047978957
#> 4.1      furniture  couch*      couchant  4.002281e-02  0.002155885
#> 4.2      furniture  couch*  couchsurfing  1.159810e-01  0.046172189
#> 4.3      furniture  couch*        couche  5.873038e-02  0.043086733
#> 4.4      furniture  couch*     couchette  1.557117e-01  0.138566945
#> 4.5      furniture  couch*      couchman  3.607044e-03  0.020909713
#> 4.6      furniture  couch*      couching  9.778941e-02  0.025603836
#> 4.7      furniture  couch*       couched  8.911655e-03 -0.023141308
#> 4.8      furniture  couch*       coucher  5.448386e-02  0.136451238
#> 4.9      furniture  couch*       couches  6.149996e-01  0.642776428
#> 4.10     furniture  couch*         couch  1.000000e+00  0.777768308
#> 5        furniture   sofa*        sofaer -1.746807e-01 -0.174680728
#> 5.1      furniture   sofa*       sofabed  4.930534e-01  0.493053388
#> 5.2      furniture   sofa*        sofala -1.683102e-02 -0.016831021
#> 5.3      furniture   sofa*         sofas  7.376998e-01  0.737699759
#> 5.4      furniture   sofa*         sofar -9.499185e-02 -0.094991854
#> 5.5      furniture   sofa*          sofa  1.000000e+00  1.000000000
#> 6    well_adjusted   happy         happy  1.000000e+00  0.668987316
#> 7    well_adjusted bright*   bright-eyed  2.423866e-01  0.007577284
#> 7.1  well_adjusted bright*    brightmail -7.953414e-02 -0.103623466
#> 7.2  well_adjusted bright*    brightcove -2.638364e-02 -0.020390810
#> 7.3  well_adjusted bright*    brightling -4.155437e-02 -0.049855680
#> 7.4  well_adjusted bright*  brightnesses  9.780009e-02 -0.107159578
#> 7.5  well_adjusted bright*   brightwater -1.177716e-01 -0.073448621
#> 7.6  well_adjusted bright*   brightpoint -1.143870e-01 -0.124154393
#> 7.7  well_adjusted bright*    brightstar  4.630735e-02 -0.071935486
#> 7.8  well_adjusted bright*    brightwell -3.234350e-03 -0.043479127
#> 7.9  well_adjusted bright*      brighter  6.335578e-01  0.203012712
#> 7.10 well_adjusted bright*      brighton  2.579316e-01  0.319441300
#> 7.11 well_adjusted bright*   brightfield  4.749668e-02 -0.059751944
#> 7.12 well_adjusted bright*     brightest  5.378596e-01  0.223565471
#> 7.13 well_adjusted bright*    brightwork -2.986911e-02 -0.020519883
#> 7.14 well_adjusted bright*      brighten  4.917114e-01  0.258820016
#> 7.15 well_adjusted bright*    brightside  8.110498e-02  0.011232105
#> 7.16 well_adjusted bright*      brightly  7.098957e-01  0.173942116
#> 7.17 well_adjusted bright*    brightness  5.076152e-01  0.113206864
#> 7.18 well_adjusted bright*   brightening  2.915817e-01  0.048249708
#> 7.19 well_adjusted bright*    brightened  3.989190e-01  0.144605826
#> 7.20 well_adjusted bright*    brightwood -5.574753e-02 -0.023598381
#> 7.21 well_adjusted bright*   brighthouse  1.314844e-02  0.001678741
#> 7.22 well_adjusted bright*     brightman -7.511471e-03  0.064558972
#> 7.23 well_adjusted bright* brightlingsea -9.688023e-02 -0.031468955
#> 7.24 well_adjusted bright*        bright  1.000000e+00  0.357651252
#> 7.25 well_adjusted bright*       brights  3.541581e-01  0.092125277
#> 7.26 well_adjusted bright*     brightens  3.503756e-01  0.066159802
#> 8    well_adjusted friend*       friendz  1.395804e-01  0.224113174
#> 8.1  well_adjusted friend*    friendlies -5.471498e-05  0.135094430
#> 8.2  well_adjusted friend*        friend  1.000000e+00  0.815147799
#> 8.3  well_adjusted friend*      friendly  4.840347e-01  0.518355797
#> 8.4  well_adjusted friend*    friendfeed  1.858389e-01  0.252301316
#> 8.5  well_adjusted friend*   friendswood  4.390444e-04  0.043916231
#> 8.6  well_adjusted friend*  friendliness  1.435103e-01  0.207111231
#> 8.7  well_adjusted friend*    friendster  2.560383e-01  0.302085908
#> 8.8  well_adjusted friend*    friendship  5.350424e-01  0.613310672
#> 8.9  well_adjusted friend*       friends  8.151478e-01  1.000000000
#> 8.10 well_adjusted friend*    friendlier  1.157377e-01  0.184342733
#> 8.11 well_adjusted friend*   friendliest  1.380033e-01  0.215251090
#> 8.12 well_adjusted friend*    friendless  1.296303e-01  0.162746245
#> 8.13 well_adjusted friend*   friendships  3.297935e-01  0.518973621
#> 8.14 well_adjusted friend*      friended  1.958729e-01  0.259071536
#> 8.15 well_adjusted friend*     friending  1.462934e-01  0.225425703
#> 9    well_adjusted     she           she  1.000000e+00  0.565640744
#> 10   well_adjusted      he            he  1.000000e+00  0.534147512
#> 11   well_adjusted    they          they  1.000000e+00  0.607023614
#> 
#> $suggested
#> NULL
#> 
```
