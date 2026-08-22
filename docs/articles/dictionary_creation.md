# Introduction to Dictionary Creation

Walks through the process of creating and assessing a dictionary.

*Built with R 4.6.1*

See the [dictionary
builder](https://miserman.github.io/dictionary_builder/) for a way to
make dictionaries interactively.

------------------------------------------------------------------------

## Background

**Dictionaries** in this context are sets of term lists that each
represent a category. **Categories** could range from concepts or topics
(e.g., “furniture” or “shopping”) to more abstract aspects of a text
(e.g., “emotionality”). **Terms** may be single literal words (e.g.,
“term”, “terms”), glob-like or fuzzy words (e.g., “term\*“, where the
asterisk matches any number of additional letters), or arbitrary
patterns (literal or regular expressions, e.g.,”a phrase” or “an?
(?:person\|object)”).

The most straightforward way to make a dictionary is to manually assign
terms to categories, but dictionaries can also be created with data,
either by extracting cluster-like structures and assigning them category
names (using unsupervised learning methods; see the [introduction to
word
vectors](https://miserman.github.io/lingmatch/articles/word_vectors.html)
article), or by training a classifier to distinguish between provided
tags (using supervised learning methods; see the [introduction to text
classification](https://miserman.github.io/lingmatch/articles/text_classification.html)
article).

## Implementation

In `lingmatch`, dictionaries are ultimately implemented as either lists
or data.frames.

As lists, categories are named entries with terms as character vectors:

``` r

(dict_unweighted <- list(
  a = c("aa", "ab", "ac"),
  b = c("ba", "bb", "bc")
))
#> $a
#> [1] "aa" "ab" "ac"
#> 
#> $b
#> [1] "ba" "bb" "bc"
```

As data.frames, terms are stored in a single column, and categories are
defined by columns containing weights:

``` r

(dict_dataframe <- data.frame(
  term = c("aa", "ab", "ac", "ba", "bb", "bc"),
  a = c(1, .9, .8, 0, 0, 0),
  b = c(0, 0, 0, .9, 1, .8)
))
#>   term   a   b
#> 1   aa 1.0 0.0
#> 2   ab 0.9 0.0
#> 3   ac 0.8 0.0
#> 4   ba 0.0 0.9
#> 5   bb 0.0 1.0
#> 6   bc 0.0 0.8
```

Lists can also store weights in named numeric vectors:

``` r

(dict_list <- list(
  a = c("aa" = 1, "ab" = .9, "ac" = .8),
  b = c("ba" = .9, "bb" = 1, "bc" = .8)
))
#> $a
#>  aa  ab  ac 
#> 1.0 0.9 0.8 
#> 
#> $b
#>  ba  bb  bc 
#> 0.9 1.0 0.8
```

Weighted dictionaries can be discretized using the `read.dic` function:

``` r

read.dic(dict_dataframe, as.weighted = FALSE)
#> $a
#> [1] "aa" "ab" "ac"
#> 
#> $b
#> [1] "ba" "bb" "bc"
```

And un-weighted dictionaries can be converted to a weighted format with
binary weights:

``` r

read.dic(dict_unweighted, as.weighted = TRUE)
#>   term a b
#> 1   aa 1 0
#> 2   ab 1 0
#> 3   ac 1 0
#> 4   ba 0 1
#> 5   bb 0 1
#> 6   bc 0 1
```

Dictionaries can also be read in from LIWC’s `.dic` format:

``` r

# this can also be written to and read from a file
raw_dic <- write.dic(dict_unweighted, save = FALSE)
cat(raw_dic)
#> %
#> 1    a
#> 2    b
#> %
#> aa   1
#> ab   1
#> ac   1
#> ba   2
#> bb   2
#> bc   2
read.dic(raw = raw_dic)
#> $a
#> [1] "aa" "ab" "ac"
#> 
#> $b
#> [1] "ba" "bb" "bc"
```

For use in web applications, JavaScript Object Notation (JSON) may be a
useful format, which can be converted to from lists:

``` r

cat(jsonlite::toJSON(dict_unweighted, pretty = TRUE))
#> {
#>   "a": ["aa", "ab", "ac"],
#>   "b": ["ba", "bb", "bc"]
#> }
```

`.dic` or `JSON` dictionaries can be used in the [adicat
highlighter](https://miserman.github.io/adicat/highlight) (from the
menu: Dictionary \> load/create/edit \> load), which can be used to see
word matches, or process files.

Any format can also be read into the [dictionary
builder](https://miserman.github.io/dictionary_builder/) (from the
left-side menu: New \> File), which can be used to assess and edit the
dictionary.

## Assessment

Dictionary categories can be thought of as measures of the construct
identified by the category’s name. Constructs can range from being well
defined by the text itself, to being more subtly embedded in the text.

Some question we might ask when assessing a dictionary category are:

1.  How well will each term capture the word or words we have in mind?
    - That is, are there unaccounted for target word variants, or
      unintended wildcard matches?
2.  How well does this set of terms cover the possible instances of the
    construct?
    - That is, might the construct appear in a way that isn’t covered?
3.  How confident would we be that the resulting score reflects the
    construct?
    - That is, how much room is there for false positives due to varying
      contexts or word senses?

For instance, consider this small dictionary:

``` r

dict <- list(
  a_words = "a*",
  self_reference = c("i", "i'*", "me", "my", "mine", "myself"),
  furniture = c("table", "chair", "desk*", "couch*", "sofa*"),
  well_adjusted = c("happy", "bright*", "friend*", "she", "he", "they")
)
```

### Character Variants

The `a_words` category is only defined by characters, so it is perfect
in that its scores can be expected to perfectly align with any other
reliable means of scoring (such as a human counter). The only threat to
this category (assuming texts are lowercased) is special characters that
should count as `a`s but aren’t initially. This can be avoided by
converting special characters as part of pre-processing:

``` r

clean <- lma_dict("special", as.function = gsub)
lma_process(clean("Àn apple and à potato ærosol."), dict = dict[1], meta = FALSE)
#>                             text a_words
#> 1 an apple and a potato aerosol.       5
```

### Use Variants

The `self_reference` category is made up of words, so in addition to
possible character variants, there are spelling/formatting variants to
try and account for. Here “i’\*” is particularly vulnerable since the
apostrophe may be curly or omitted. A new danger introduced in this
category is of false positives due to alternative uses of “i” (e.g., as
a list item label) and alternate senses of “mine”. These issues make for
possible differences between the automatic score, and that theoretically
calculated by a human:

``` r

lma_process(
  c("I) A mine.", "Mmeee! idk how but imma try!"),
  dict = dict[2], meta = FALSE
)
#>                           text self_reference
#> 1                   I) A mine.              2
#> 2 Mmeee! idk how but imma try!              0
```

### Coverage

The `furniture` and `well_adjusted` categories introduce two main
additional considerations:

#### Term Variants

First, they uses broader wildcards, which are probably intended to
simply catch plural forms, but are in danger of over-extending. We can
use the `report_term_matches` function to check this:

``` r

report <- report_term_matches(dict[3:4], space_dir = "~/Latent Semantic Spaces")
#> preparing dict (0)
#> extracting matches (0)
#> preparing results (0.11)
#> done (0.12)

knitr::kable(report[, c("term", "categories", "variants", "matches")])
```

| term | categories | variants | matches |
|:---|:---|---:|:---|
| bright\* | well_adjusted | 63 | bright, brights, brighter, brighton, brighten, brightly, brightway, brightley, brightest, brightens, brightful, brighttag, brightman, brighting, brightener, brightling, brightstar, brightwell, brighteyes, brightwork, brightbill, brightmail, brightside, brightened, brightness, brightwood, brightline, bright-red, brightmoor, brightview, brightroll, brightcove, brightkite, brightspot, bright-eyed, brightfield, brightpoint, brightwater, brightwells, brightening, brighthouse, brightscope, brighteners, brightsolid, brightlight, bright-blue, brightblack, brightspark, brightstone, brightworks, bright-field, brightnesses, bright-green, brightwaters, brightsource, brightly-lit, brightonians, bright-yellow, bright-orange, brightlingsea, bright-colored, brightly-colored, brightly-coloured |
| friend\* | well_adjusted | 38 | friend, friende, friends, friendz, friendy, friendo, frienda, friended, friendly, friendlly, friendlys, friending, friendless, friendlier, friendship, friendster, friendfeed, friendlily, friendlies, friendzone, friendlist, friendcode, friendships, friendliest, friendzoned, friend-zone, friendswood, friendstream, friendlyness, friendliness, friendsville, friendraising, friendly-fire, friendsgiving, friends/family, friendlessness, friendsreunited, friend-of-the-court |
| desk\* | furniture | 23 | desk, deska, desko, desks, deskew, deskop, desker, deskset, deskman, desking, deskpro, desktop, deskjet, deskins, desk-top, desktops, deskside, deskstar, deskilled, deskbound, desk-bound, deskilling, desktop-publishing |
| couch\* | furniture | 18 | couch, couche, couches, coucher, couched, couchdb, couchie, couchois, couchers, couchman, couchant, couching, couchette, couchiching, couchpotato, couch-potato, couchsurfers, couchsurfing |
| sofa\* | furniture | 9 | sofa, sofas, sofar, sofast, sofaer, sofala, sofabed, sofamor, sofa-bed |
| table | furniture | 1 | table |
| chair | furniture | 1 | chair |
| happy | well_adjusted | 1 | happy |
| she | well_adjusted | 1 | she |
| he | well_adjusted | 1 | he |
| they | well_adjusted | 1 | they |

By default, this searches for matches in a large set of common words
found across latent semantic spaces (embeddings), but it can also be run
on sets of text to see matches within narrower contexts.

#### Category Coverage

##### Term Coverage

The second consideration is that these categories are trying to cover
broad concepts, so there are likely to be obvious but overlooked terms
to include. One thing we could do to improve this sort of coverage is
search for similar words within a latent semantic space:

``` r

meta <- dictionary_meta(
  dict[3:4],
  suggest = TRUE, space_dir = "~/Latent Semantic Spaces"
)
#> preparing terms (0)
#> expanding terms (0.62)
#> loading space (0.78)
#> calculating term similarities (16.11)
#> identifying potential additions (29.51)
#> preparing results (32.34)
#> done (32.73)
meta$suggested
#> $furniture
#>    armchairs     loveseat    banquette      armless      settees        futon 
#>   0.06596930   0.06543237   0.06192136   0.05951132   0.05949731   0.05830094 
#>  upholstered  workstation       settee workstations       chaise      chaises 
#>   0.05736860   0.05715411   0.05680740   0.05619739   0.05503267   0.05437331 
#>     credenza        aeron       seater     l-shaped     recliner 
#>   0.05337485   0.05263192   0.05249444   0.05214024   0.05159841 
#> 
#> $well_adjusted
#>       hope      smile     smiles       glow       glad       love   cheerful 
#> 0.08939145 0.08316053 0.07897790 0.07818952 0.07723263 0.07644744 0.07598900 
#>       eyes      shine    excited        way       wish    shining     coming 
#> 0.07564743 0.07539943 0.07490184 0.07400350 0.07336494 0.07310913 0.07289791 
#>     loving       know      bring       life     shines   thankful     joyful 
#> 0.07264139 0.07229958 0.07142593 0.07138101 0.07119462 0.07097478 0.07094606 
#>   facebook    welcome      loved        say       tell       face       sure 
#> 0.07082714 0.07073722 0.07052697 0.07044166 0.07035872 0.07019802 0.07003938 
#>     hoping   sunshine      young       grow       come     better        fun 
#> 0.06978870 0.06950521 0.06944686 0.06924857 0.06920617 0.06916552 0.06908122 
#>      shone        let        sad       turn     seeing     summer        eye 
#> 0.06876386 0.06872918 0.06857477 0.06855629 0.06853710 0.06853068 0.06850545 
#>       meet        'll    vibrant     helped      thank      light     wishes 
#> 0.06840545 0.06829185 0.06823692 0.06808839 0.06788728 0.06777502 0.06771657
```

We can also use the space to assess category cohesiveness by looking at
summaries of pairwise cosine similarities between terms:

``` r

knitr::kable(meta$summary[, -1], digits = 3)
```

|  | n_terms | n_expanded | sim.space | sim.min | sim.q1 | sim.median | sim.mean | sim.q3 | sim.max |
|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|
| furniture | 5 | 34 | glove_crawl | -0.034 | 0.015 | 0.035 | 0.052 | 0.085 | 0.152 |
| well_adjusted | 6 | 47 | glove_crawl | -0.014 | 0.017 | 0.077 | 0.081 | 0.137 | 0.183 |

Or look at those similarities within categories and expanded terms:

``` r

knitr::kable(
  meta$terms[meta$terms$category == "furniture", ],
  digits = 3, row.names = FALSE
)
```

| category  | term    | match        | sim.term | sim.category |
|:----------|:--------|:-------------|---------:|-------------:|
| furniture | table   | table        |    1.000 |        0.520 |
| furniture | chair   | chair        |    1.000 |        0.644 |
| furniture | desk\*  | desk-top     |    0.218 |        0.020 |
| furniture | desk\*  | desk         |    1.000 |        0.543 |
| furniture | desk\*  | desking      |    0.229 |        0.215 |
| furniture | desk\*  | deskpro      |    0.067 |       -0.030 |
| furniture | desk\*  | deskilled    |   -0.050 |       -0.139 |
| furniture | desk\*  | desktop      |    0.481 |        0.197 |
| furniture | desk\*  | desktops     |    0.266 |        0.120 |
| furniture | desk\*  | desks        |    0.706 |        0.488 |
| furniture | desk\*  | deskjet      |    0.067 |        0.014 |
| furniture | desk\*  | deskbound    |   -0.033 |        0.040 |
| furniture | desk\*  | deskins      |   -0.074 |       -0.093 |
| furniture | desk\*  | deskilling   |   -0.111 |       -0.127 |
| furniture | desk\*  | desker       |   -0.088 |       -0.101 |
| furniture | desk\*  | deskside     |    0.102 |       -0.018 |
| furniture | desk\*  | deskstar     |    0.032 |       -0.078 |
| furniture | couch\* | couchdb      |    0.089 |        0.048 |
| furniture | couch\* | couchant     |    0.040 |        0.002 |
| furniture | couch\* | couchsurfing |    0.116 |        0.046 |
| furniture | couch\* | couche       |    0.059 |        0.043 |
| furniture | couch\* | couchette    |    0.156 |        0.139 |
| furniture | couch\* | couchman     |    0.004 |        0.021 |
| furniture | couch\* | couching     |    0.098 |        0.026 |
| furniture | couch\* | couched      |    0.009 |       -0.023 |
| furniture | couch\* | coucher      |    0.054 |        0.136 |
| furniture | couch\* | couches      |    0.615 |        0.643 |
| furniture | couch\* | couch        |    1.000 |        0.778 |
| furniture | sofa\*  | sofaer       |   -0.175 |       -0.175 |
| furniture | sofa\*  | sofabed      |    0.493 |        0.493 |
| furniture | sofa\*  | sofala       |   -0.017 |       -0.017 |
| furniture | sofa\*  | sofas        |    0.738 |        0.738 |
| furniture | sofa\*  | sofar        |   -0.095 |       -0.095 |
| furniture | sofa\*  | sofa         |    1.000 |        1.000 |

And we can visualize this together with the most similar suggested terms
as a network:

``` r

library(visNetwork)

display_network <- function(meta, cat = 1, n = 10, min = .1, seed = 2080) {
  cat_name <- meta$summary$category[[cat]]
  top_suggested <- meta$suggested[[cat_name]][seq_len(n)]
  terms <- meta$expanded[[cat_name]]
  nodes <- data.frame(
    id = c(terms, names(top_suggested)),
    label = c(terms, names(top_suggested)),
    group = rep(
      c("original", "suggested"),
      c(length(terms), length(top_suggested))
    ),
    shape = "box"
  )
  suggested_sim <- lma_simets(lma_lspace(
    nodes$id,
    space = meta$summary$sim.space[[1]]
  ), metric = "cosine")
  nodes$size <- rowMeans(suggested_sim)
  edges <- data.frame(
    from = rep(colnames(suggested_sim), each = nrow(suggested_sim)),
    to = rep(rownames(suggested_sim), nrow(suggested_sim)),
    value = as.numeric(suggested_sim),
    title = as.numeric(suggested_sim)
  )

  visNetwork(
    nodes, within(
      edges[edges$value > min & edges$value < 1, ], value <- (value * 10)^4
    )
  ) |>
    visEdges("title", smooth = FALSE, color = list(opacity = .6)) |>
    visLegend(width = .07) |>
    visLayout(randomSeed = seed) |>
    visPhysics("barnesHut", timestep = .1) |>
    visInteraction(
      dragNodes = TRUE, dragView = TRUE, hover = TRUE, hoverConnectedEdges = TRUE,
      selectable = TRUE, tooltipDelay = 100, tooltipStay = 100
    )
}

display_network(meta)
```

Or we could look at terms across categories within a
dimensionally-reduced version of the space:

``` r

library(plotly)

display_reduced_space <- function(
  meta, space_name = "glove_crawl", method = "umap", dim_prop = 1,
  color_seeds = c("#25cb1a", "#c8fd9e", "#1b85ed", "#91b8fb")
) {
  suggestions <- unlist(unname(meta$suggested))
  terms <- rbind(meta$terms[, c("category", "match", "sim.category")], data.frame(
    category = rep(
      paste0(names(meta$suggested), "_suggested"), vapply(meta$suggested, length, 0)
    ),
    match = names(suggestions),
    sim.category = suggestions
  ))
  space <- lma_lspace(terms$match, space_name)
  space <- space[, Reduce(unique, lapply(meta$expanded, function(l) {
    order(-colMeans(space[l, ]))[seq_len(min(ncol(space), ncol(space) * dim_prop))]
  }))]
  st <- proc.time()[[3]]
  m <- as.data.frame(if (method == "umap") {
    uwot::umap(space, 15, 3, metric = "cosine")
  } else if (method == "taffy") {
    m <- lusilab::taffyInf(lma_simets(space, metric = "cosine"), 3)
    colnames(m) <- c("V1", "V2", "V3")
    m
  } else if (method == "kmeans") {
    m <- t(kmeans(lma_simets(space, metric = "cosine"), 3)$centers)
    colnames(m) <- c("V1", "V2", "V3")
    m
  } else if (method == "svd") {
    m <- svd(lma_simets(space, metric = "cosine"), 3)$u
    rownames(m) <- rownames(space)
    m
  } else {
    m <- prcomp(lma_simets(space, metric = "cosine"))$rotation[, 1:3]
    dimnames(m) <- list(rownames(space), paste0("V", 1:3))
    m
  })
  message(
    "reduced space via ", method, " method in ",
    round(proc.time()[[3]] - st, 4), " seconds"
  )
  d <- cbind(terms, m)
  d$color <- color_seeds[as.numeric(as.factor(d$category))]
  ds <- split(d, d$category)
  p <- plot_ly(
    do.call(rbind, ds),
    x = ~V1, y = ~V2, z = ~V3, textfont = list(size = 9)
  ) |>
    layout(
      showlegend = TRUE, paper_bgcolor = "#000000", font = list(color = "#ffffff"),
      margin = list(r = 0, b = 0, l = 0)
    )
  for (cat in names(ds)) {
    p <- p |> add_text(
      data = ds[[cat]], text = ~match, name = cat, textfont = list(color = ~color)
    )
  }
  p
}

display_reduced_space(meta)
#> reduced space via umap method in 1.38 seconds
```

Here it seems the suggested terms strengthen cores of related terms
within categories, leaving unrelated terms to group together between
categories.

Instead of looking at pairwise comparisons, it might also make sense to
compare with category centroids:

``` r

meta_centroid <- dictionary_meta(
  dict[3:4],
  pairwise = FALSE, suggest = TRUE, space_dir = "~/Latent Semantic Spaces"
)
#> preparing terms (0)
#> expanding terms (0.28)
#> loading space (0.44)
#> calculating term similarities (15.48)
#> identifying potential additions (17.48)
#> preparing results (20.17)
#> done (20.5)

meta_centroid$suggested
#> $furniture
#>    armchairs     loveseat        futon      armless  upholstered  workstation 
#>    0.2517359    0.2474119    0.2238488    0.2218516    0.2198026    0.2195312 
#>    banquette      settees workstations       settee       chaise     recliner 
#>    0.2191410    0.2169368    0.2152506    0.2146026    0.2130485    0.2039399 
#>     credenza       seater      chaises 
#>    0.2013844    0.1996919    0.1978596 
#> 
#> $well_adjusted
#>        hope       smile        glad        glow      smiles    thankful 
#>   0.2833616   0.2475967   0.2413412   0.2372855   0.2366536   0.2291406 
#>     excited      wishes        love    cheerful        eyes       shine 
#>   0.2249745   0.2237435   0.2235930   0.2231660   0.2230449   0.2219152 
#>      joyful     shining      loving      shines        wish       hopes 
#>   0.2190795   0.2177220   0.2168939   0.2162090   0.2156694   0.2155521 
#>      coming    youthful         way       shone    facebook        know 
#>   0.2136403   0.2131742   0.2128437   0.2127467   0.2123793   0.2121608 
#>         sad      hoping      joyous       loved    sparkles         say 
#>   0.2120186   0.2105333   0.2101588   0.2101528   0.2099162   0.2094993 
#>     promise   celebrate    grateful        life       merry      honest 
#>   0.2093688   0.2090422   0.2085683   0.2080989   0.2078806   0.2078371 
#>     blessed        tell      seeing      hearts    positive    feelings 
#>   0.2074664   0.2070684   0.2059093   0.2049330   0.2045796   0.2038124 
#>        face       young    befriend    sunshine         eye       alive 
#>   0.2037544   0.2035204   0.2030985   0.2027419   0.2024249   0.2022952 
#>        grow      helped      caring     glowing     vibrant       proud 
#>   0.2022153   0.2022134   0.2020372   0.2019306   0.2018866   0.2013347 
#>   surprised        knew       thank       cheer        sure        miss 
#>   0.2009025   0.2007124   0.2006891   0.2002314   0.2001261   0.1999670 
#>   attracted      better       bring        past     younger    thrilled 
#>   0.1996485   0.1994686   0.1987255   0.1984401   0.1982115   0.1981868 
#>       earth     welcome      summer       faces      afraid        true 
#>   0.1981317   0.1980738   0.1976383   0.1976157   0.1973665   0.1970078 
#>       sweet         fun        grew  complexion     sincere        fair 
#>   0.1968552   0.1968065   0.1967096   0.1966697   0.1966093   0.1962342 
#>        turn    remember   fortunate      thanks         let     believe 
#>   0.1961961   0.1961387   0.1960371   0.1958364   0.1957286   0.1957258 
#>  optimistic      chance   greetings  remembered        hear encouraging 
#>   0.1957108   0.1953343   0.1948726   0.1943526   0.1942415   0.1939956 
#>        dear  enthusiasm       glows     helping     happier       light 
#>   0.1939174   0.1938838   0.1935520   0.1935429   0.1934165   0.1932916 
#>         sky 
#>   0.1931875
knitr::kable(meta_centroid$summary[, -1], digits = 3)
```

|  | n_terms | n_expanded | sim.space | sim.min | sim.q1 | sim.median | sim.mean | sim.q3 | sim.max |
|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|
| furniture | 5 | 34 | glove_crawl | -0.093 | 0.018 | 0.096 | 0.146 | 0.19 | 0.675 |
| well_adjusted | 6 | 47 | glove_crawl | -0.170 | 0.018 | 0.191 | 0.199 | 0.36 | 0.651 |

The previous examples looked at terms within a single space, but we can
also aggregate across multiple spaces, which might result in more
reliable comparisons:

``` r

# by default, suggestions are sensitive to categories
# (trying to maximize difference between categories),
# but in this case, it seems to make suggestions worse
# so we set `suggest_discriminate` to `FALSE`
meta_multi <- dictionary_meta(
  dict[3:4], "multi",
  suggest_discriminate = FALSE, suggest = TRUE,
  space_dir = "~/Latent Semantic Spaces"
)
#> preparing terms (0)
#> expanding terms (0.26)
#> loading spaces (0.4)
#> calculating term similarities (64.25)
#> identifying potential additions (188.72)
#> preparing results (191.18)
#> done (191.33)

meta_multi$suggested
#> $furniture
#>      banquette        dinette        cubical       snoozing        hassock 
#>      0.3926174      0.3755285      0.3742140      0.3742027      0.3725989 
#>    living-room airconditioned      curtained      open-plan   cross-legged 
#>      0.3721557      0.3671342      0.3652230      0.3648263      0.3623970 
#>       hunching    semi-circle         chaise       cabriole        chaises 
#>      0.3621855      0.3605409      0.3602326      0.3600797      0.3571732 
#>          plops   kitchenettes        sleekly        settees      armchairs 
#>      0.3570083      0.3543388      0.3533520      0.3530020      0.3521166 
#>      verandahs        longues       anteroom      parquetry  reupholstered 
#>      0.3515619      0.3515305      0.3512446      0.3503180      0.3501828 
#>      cubbyhole      staffroom       waterbed     sideboards         doored 
#>      0.3500203      0.3495697      0.3494830      0.3482853      0.3477275 
#>      washstand        regally 
#>      0.3474672      0.3473579 
#> 
#> $well_adjusted
#>        cheery         aglow     workmates      sunshiny        giggly 
#>     0.3801433     0.3762835     0.3742287     0.3667182     0.3647983 
#>     flickered      sunbeams complimenting       babysit      livelier 
#>     0.3550112     0.3531647     0.3521553     0.3520434     0.3472921 
#>       beaming  cheerfulness   schoolmates        jovial      vivacity 
#>     0.3471171     0.3470663     0.3462756     0.3446221     0.3444394 
#>     soulmates   sweethearts         becks mischievously      flitting 
#>     0.3425953     0.3412506     0.3407253     0.3398005     0.3397464 
#>        perked      shinning     enlivened        gaiety         gleam 
#>     0.3386063     0.3385910     0.3382409     0.3380319     0.3372517 
#>     playmates   fantabulous   crestfallen      freckles         winks 
#>     0.3362337     0.3362083     0.3356015     0.3354109     0.3352553 
#>       enliven       squinty       sunnier      greatful       pitying 
#>     0.3351089     0.3349024     0.3348174     0.3346780     0.3346716 
#>         mousy    blossoming    frolicking    nightlight      to-night 
#>     0.3344969     0.3338377     0.3338082     0.3337505     0.3335339 
#>     next-door       eveyone      cheerily    fun-loving       flighty 
#>     0.3333128     0.3330416     0.3326816     0.3326738     0.3324997 
#>    likeminded      sociable      outshine        glowed   girlfriends 
#>     0.3323638     0.3320942     0.3320304     0.3320234     0.3312609
knitr::kable(meta_multi$summary[, -1], digits = 3)
```

|  | n_terms | n_expanded | sim.space | sim.min | sim.q1 | sim.median | sim.mean | sim.q3 | sim.max |
|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|
| furniture | 5 | 33 | glove_crawl, paragram_sl999, paragram_ws353, sensembed, CoNLL17_skipgram | 0.138 | 0.223 | 0.259 | 0.259 | 0.304 | 0.369 |
| well_adjusted | 6 | 45 | glove_crawl, paragram_sl999, paragram_ws353, sensembed, CoNLL17_skipgram | 0.141 | 0.200 | 0.261 | 0.249 | 0.296 | 0.357 |

##### Text Coverage

Suggested terms can help improve the theoretical coverage of these
categories in themselves, but another type of coverage is how much of
the category is covered by the text it’s scoring. Low coverage of this
sort isn’t inherently an issue, but it puts more pressure on the covered
terms to be unambiguous. For instance, compare the score versus coverage
in these texts:

``` r

texts <- c(
  furniture = "There is a chair positioned in the intersection of a desk and table.",
  still_furniture = "I'm selling this chair, since my new chair replaced that chair.",
  business = "The chair took over from the former chair to introduced the new chair.",
  business_mixed = "The chair sat down at their desk to table the discussion."
)
lma_termcat(texts, dict[3], coverage = TRUE)
#>                 furniture coverage_furniture
#> furniture               3                  3
#> still_furniture         3                  1
#> business                3                  1
#> business_mixed          3                  3
#> attr(,"WC")
#> [1] 13 11 13 11
#> attr(,"time")
#>     dtm termcat 
#>       0       0 
#> attr(,"type")
#> [1] "count"
```

These examples illustrate how this sort of coverage could relate to
score validity (i.e., how much the category is actually reflected in the
text), but also how it is not a perfect indicator. Generally, a smaller
variety of term hits within a category should make us less confident in
the category score.
