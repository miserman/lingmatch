# Linguistic Matching and Accommodation

Offers a variety of methods to assess linguistic matching or
accommodation, where *matching* is general similarity (sometimes called
*homophily*), and *accommodation* is some form of conditional similarity
(accounting for some base-rate or precedent; sometimes called
*alignment*).

## Usage

``` r
lingmatch(input = NULL, comp = mean, data = NULL, group = NULL, ...,
  comp.data = NULL, comp.group = NULL, order = NULL, drop = FALSE,
  all.levels = FALSE, type = "lsm")
```

## Arguments

- input:

  Texts to be compared; a vector, document-term matrix (dtm; with terms
  as column names), or path to a file (.txt or .csv, with texts
  separated by one or more lines/rows).

- comp:

  Defines the comparison to be made:

  - If a **function**, this will be applied to `input` within each group
    (overall if there is no group; i.e., `apply(input, 2, comp)`; e.g.,
    `comp = mean` would compare each text to the mean profile of its
    group).

  - If a **character** with a length of 1 and no spaces:

    - If it partially matches one of `lsm_profiles`'s rownames, that row
      will be used as the comparison.

    - If it partially matches `'auto'`, the highest correlating
      `lsm_profiles` row will be used.

    - If it partially matches `'pairwise'`, each text will be compared
      to one another.

    - If it partially matches `'sequential'`, the last variable in
      `group` will be treated as a speaker ID (see the Grouping and
      Comparisons section).

  - If a **character vector**, this will be processed in the same way as
    `input`.

  - If a **vector**, either (a) logical or factor-like (having n levels
    \< length) and of the same length as `nrow(input)`, or (b) numeric
    or logical of length less than `nrow(input)`, this will be used to
    select a subset of `input` (e.g., `1:10` would treat the first 10
    rows of `input` as the comparison;
    `lingmatch(text, type == 'prompt', data)` would use the texts in the
    `text` column identified by the `type` column as the comparison).

  - If a **matrix-like object** (having multiple rows and columns), or a
    named vector, this will be treated as a sort of dtm, assuming there
    are common (column) names between `input` and `comp` (e.g., if you
    had prompt and response texts that were already processed
    separately).

- data:

  A matrix-like object as a reference for column names, if variables are
  referred to in other arguments (e.g., `lingmatch(text, data = data)`
  would be the same as `lingmatch(data$text)`).

- group:

  A logical or factor-like vector the same length as `NROW(input)`, used
  to defined groups.

- ...:

  Passes arguments to
  [`lma_dtm`](https://miserman.github.io/lingmatch/reference/lma_dtm.md),
  [`lma_weight`](https://miserman.github.io/lingmatch/reference/lma_weight.md),
  [`lma_termcat`](https://miserman.github.io/lingmatch/reference/lma_termcat.md),
  and/or
  [`lma_lspace`](https://miserman.github.io/lingmatch/reference/lma_lspace.md)
  (depending on `input` and `comp`), and
  [`lma_simets`](https://miserman.github.io/lingmatch/reference/lma_simets.md).

- comp.data:

  A matrix-like object as a source for `comp` variables.

- comp.group:

  The column name of the grouping variable(s) in `comp.data`; if `group`
  contains references to column names, and `comp.group` is not
  specified, `group` variables will be looked for in `comp.data`.

- order:

  A numeric vector the same length as `nrow(input)` indicating the order
  of the texts and grouping variables when the type of comparison is
  sequential. Only necessary if the texts are not already ordered as
  desired.

- drop:

  logical; if `TRUE`, will drop columns with a sum of 0.

- all.levels:

  logical; if `FALSE`, multiple groups are combined. See the Grouping
  and Comparisons section.

- type:

  A character at least partially matching 'lsm' or 'lsa'; applies
  default settings aligning with the standard calculations of each type:

  |  |  |
  |----|----|
  | LSM | `lingmatch(text, weight = 'freq', dict = lma_dict(1:9), metric = 'canberra')` |
  | LSA | `lingmatch(text, weight = 'tfidf', space = '100k_lsa', metric = 'cosine')` |

## Value

A list with processed components of the input, information about the
comparison, and results of the comparison:

- **`dtm`**: A sparse matrix; the raw count-dtm, or a version of the
  original input if it is more processed.

- **`processed`**: A matrix-like object; a processed version of the
  input (e.g., weighted and categorized).

- **`comp.type`**: A string describing the comparison if applicable.

- **`comp`**: A vector or matrix-like object; the comparison data if
  applicable.

- **`group`**: A string describing the group if applicable.

- **`sim`**: Result of
  [`lma_simets`](https://miserman.github.io/lingmatch/reference/lma_simets.md).

## Details

There are a great many points of decision in the assessment of
linguistic similarity and/or accommodation, partly inherited from the
great many point of decision inherent in the numerical representation of
language. Two general types of matching are implemented here as sets of
defaults: Language/Linguistic Style Matching (LSM; Niederhoffer &
Pennebaker, 2002; Ireland & Pennebaker, 2010), and Latent Semantic
Analysis/Similarity (LSA; Landauer & Dumais, 1997; Babcock, Ta, & Ickes,
2014). See the `type` argument for specifics.

## Grouping and Comparisons

Defining groups and comparisons can sometimes be a bit complicated, and
requires dataset specific knowledge, so it can't always (readily) be
done automatically. Variables entered in the `group` argument are
treated differently depending on their position and other arguments:

- Splitting:

  By default, groups are treated as if they define separate chunks of
  data in which comparisons should be calculated. Functions used to
  calculated comparisons, and pairwise comparisons are performed
  separately in each of these groups. For example, if you wanted to
  compare each text with the mean of all texts in its condition, a
  `group` variable could identify and split by condition. Given multiple
  grouping variables, calculations will either be done in each split (if
  `all.levels = TRUE`; applied in sequence so that groups become smaller
  and smaller), or once after all splits are made (if
  `all.levels = FALSE`). This makes for 'one to many' comparisons with
  either calculated or preexisting standards (i.e., the profile of the
  current data, or a precalculated profile, respectively).

- Comparison ID:

  When comparison data is identified in `comp`, groups are assumed to
  apply to both `input` and `comp` (either both in `data`, or separately
  between `data` and `comp.data`, in which case `comp.group` may be
  needed if the same grouping variable have different names between
  `data` and `comp.data`). In this case, multiple grouping variables are
  combined into a single factor assumed to uniquely identify a
  comparison. This makes for 'one to many' comparisons with specific
  texts (as in the case of manipulated prompts or text-based
  conditions).

- Speaker ID:

  If `comp` matches `'sequential'`, the last grouping variable entered
  is assumed to identify something like speakers (i.e., a factor with
  two or more levels and multiple observations per level). In this case,
  the data are assumed to be ordered (or ordered once sorted by `order`
  if specified). Any additional grouping variables before the last are
  treated as splitting groups. This can set up for probabilistic
  accommodation metrics. At the moment, when sequential comparisons are
  made within groups, similarity scores between speakers are averaged,
  resulting in mean matching between speakers within the group.

## References

Babcock, M. J., Ta, V. P., & Ickes, W. (2014). Latent semantic
similarity and language style matching in initial dyadic interactions.
*Journal of Language and Social Psychology, 33*, 78-88.

Ireland, M. E., & Pennebaker, J. W. (2010). Language style matching in
writing: synchrony in essays, correspondence, and poetry. *Journal of
Personality and Social Psychology, 99*, 549.

Landauer, T. K., & Dumais, S. T. (1997). A solution to Plato's problem:
The latent semantic analysis theory of acquisition, induction, and
representation of knowledge. *Psychological Review, 104*, 211.

Niederhoffer, K. G., & Pennebaker, J. W. (2002). Linguistic style
matching in social interaction. *Journal of Language and Social
Psychology, 21*, 337-360.

## See also

For a general text processing function, see
[`lma_process()`](https://miserman.github.io/lingmatch/reference/lma_process.md).

## Examples

``` r
# compare single strings
lingmatch("Compare this sentence.", "With this other sentence.")
#> $dtm
#> 2 x 5 sparse Matrix of class "dgCMatrix"
#>      compare other sentence this with
#> [1,]       .     1        1    1    1
#> [2,]       1     .        1    1    .
#> 
#> $processed
#> 1 x 5 sparse Matrix of class "dgCMatrix"
#>      compare other sentence this with
#> [1,]       1     .        1    1    .
#> 
#> $comp.type
#> [1] "text"
#> 
#> $comp
#>  compare    other sentence     this     with 
#>        0        1        1        1        1 
#> 
#> $group
#> NULL
#> 
#> $sim
#>    cosine 
#> 0.5773503 
#> attr(,"time")
#> simets 
#>      0 
#> 

# compare each entry in a character vector with...
texts <- c(
  "One bit of text as an entry...",
  "Maybe multiple sentences in an entry. Maybe essays or posts or a book.",
  "Could be lines or a column from a read-in file..."
)

## one another
lingmatch(texts)
#> $dtm
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $processed
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $comp.type
#> [1] "pairwise"
#> 
#> $comp
#> NULL
#> 
#> $group
#> NULL
#> 
#> $sim
#> 3 x 3 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                          
#> [1,] I         .        .
#> [2,] 0.1833397 I        .
#> [3,] 0.0000000 0.280056 I
#> 

## the first
lingmatch(texts, 1)
#> $dtm
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $processed
#> 2 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [2,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $comp.type
#> [1] "1"
#> 
#> $comp
#>         a        an        as        be       bit      book    column     could 
#>         0         1         1         0         1         0         0         0 
#>     entry    essays      file      from        in     lines     maybe  multiple 
#>         1         0         0         0         0         0         0         0 
#>        of       one        or     posts   read-in sentences      text 
#>         1         1         0         0         0         0         1 
#> 
#> $group
#> NULL
#> 
#> $sim
#> [1] 0.1833397 0.0000000
#> attr(,"time")
#> simets 
#>      0 
#> 

## the next
lingmatch(texts, "seq")
#> $dtm
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $processed
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $comp.type
#> [1] "sequential"
#> 
#> $comp
#> NULL
#> 
#> $group
#> NULL
#> 
#> $sim
#>            cosine
#> 1 <-> 2 0.1833397
#> 2 <-> 3 0.2800560
#> 

## the set average
lingmatch(texts, mean)
#> $dtm
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $processed
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $comp.type
#> [1] "mean"
#> 
#> $comp
#>         a        an        as        be       bit      book    column     could 
#> 1.0000000 0.6666667 0.3333333 0.3333333 0.3333333 0.3333333 0.3333333 0.3333333 
#>     entry    essays      file      from        in     lines     maybe  multiple 
#> 0.6666667 0.3333333 0.3333333 0.3333333 0.3333333 0.3333333 0.6666667 0.3333333 
#>        of       one        or     posts   read-in sentences      text 
#> 0.3333333 0.3333333 1.0000000 0.3333333 0.3333333 0.3333333 0.3333333 
#> 
#> $group
#> NULL
#> 
#> $sim
#> [1] 0.4909903 0.8051610 0.6666667
#> attr(,"time")
#> simets 
#>      0 
#> 

## other entries in a group
lingmatch(texts, group = c("a", "a", "b"))
#> $dtm
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $processed
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $comp.type
#> [1] "group mean"
#> 
#> $comp
#>     a an  as be bit book column could entry essays file from  in lines maybe
#> a 0.5  1 0.5  0 0.5  0.5      0     0     1    0.5    0    0 0.5     0     1
#> b 2.0  0 0.0  1 0.0  0.0      1     1     0    0.0    1    1 0.0     1     0
#>   multiple  of one or posts read-in sentences text
#> a      0.5 0.5 0.5  1   0.5       0       0.5  0.5
#> b      0.0 0.0 0.0  1   0.0       1       0.0  0.0
#> 
#> $group
#> [1] "c('a', 'a', 'b')"
#> 
#> $sim
#>   g1    cosine
#> 1  a 0.6428571
#> 2  a 0.8708636
#> 3  b 1.0000000
#> 

## one another, without stop words
lingmatch(texts, exclude = "function")
#> $dtm
#> 3 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names 'book', 'column', 'entry' ... ]]
#>                         
#> [1,] . . 1 . . . . . . 1
#> [2,] 1 . 1 1 . . 1 . 1 .
#> [3,] . 1 . . 1 1 . 1 . .
#> 
#> $processed
#> 3 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names 'book', 'column', 'entry' ... ]]
#>                         
#> [1,] . . 1 . . . . . . 1
#> [2,] 1 . 1 1 . . 1 . 1 .
#> [3,] . 1 . . 1 1 . 1 . .
#> 
#> $comp.type
#> [1] "pairwise"
#> 
#> $comp
#> NULL
#> 
#> $group
#> NULL
#> 
#> $sim
#> 3 x 3 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                   
#> [1,] I         . .
#> [2,] 0.3162278 I .
#> [3,] 0.0000000 0 I
#> 

## a standard average (based on function words)
lingmatch(texts, "auto", dict = lma_dict(1:9))
#> $dtm
#> 3 x 23 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 23 column names 'a', 'an', 'as' ... ]]
#>                                                   
#> [1,] . 1 1 . 1 . . . 1 . . . . . . . 1 1 . . . . 1
#> [2,] 1 1 . . . 1 . . 1 1 . . 1 . 2 1 . . 2 1 . 1 .
#> [3,] 2 . . 1 . . 1 1 . . 1 1 . 1 . . . . 1 . 1 . .
#> 
#> $processed
#>      ppron ipron article adverb conj prep auxverb negate quant
#> [1,]     0     0       1      0    0    2       0      0     1
#> [2,]     0     0       2      2    2    1       0      0     1
#> [3,]     0     0       2      0    1    1       2      0     0
#> attr(,"WC")
#> [1]  7 13 10
#> attr(,"time")
#>     dtm termcat 
#>       0       0 
#> attr(,"type")
#> [1] "count"
#> 
#> $comp.type
#> [1] "auto: nytimes"
#> 
#> $comp
#>         ppron ipron article adverb conj  prep auxverb negate quant
#> nytimes  3.56  3.84    9.08   2.76 4.85 14.27    5.11   0.62  1.94
#> 
#> $group
#> NULL
#> 
#> $sim
#> [1] 0.8341107 0.6844995 0.7757765
#> attr(,"time")
#> simets 
#>      0 
#> 
```
