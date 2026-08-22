# Document-Term Matrix Categorization

Reduces the dimensions of a document-term matrix by dictionary-based
categorization.

## Usage

``` r
lma_termcat(dtm, dict, term.weights = NULL, bias = NULL,
  bias.name = "_intercept", escape = TRUE, partial = FALSE,
  glob = TRUE, term.filter = NULL, term.break = 20000,
  to.lower = FALSE, dir = getOption("lingmatch.dict.dir"),
  coverage = FALSE)
```

## Arguments

- dtm:

  A matrix with terms as column names.

- dict:

  The name of a provided dictionary
  ([osf.io/y6g5b/wiki](https://osf.io/y6g5b/wiki/home)) or of a file
  found in `dir`, or a `list` object with named character vectors as
  word lists, or the path to a file to be read in by
  [`read.dic`](https://miserman.github.io/lingmatch/reference/read.dic.md).

- term.weights:

  A `list` object with named numeric vectors lining up with the
  character vectors in `dict`, used to weight the terms in each `dict`
  vector. If a category in `dict` is not specified in `term.weights`, or
  the `dict` and `term.weights` vectors aren't the same length, the
  weights for that category will be 1.

- bias:

  A list or named vector specifying a constant to add to the named
  category. If a term matching `bias.name` is included in a category,
  it's associated `weight` will be used as the `bias` for that category.

- bias.name:

  A character specifying a term to be used as a category bias; default
  is `'_intercept'`.

- escape:

  Logical indicating whether the terms in `dict` should not be treated
  as plain text (including asterisk wild cards). If `TRUE`, regular
  expression related characters are escaped. Set to `TRUE` if you get
  PCRE compilation errors.

- partial:

  Logical; if `TRUE` terms are partially matched (not padded by ^ and
  \$).

- glob:

  Logical; if `TRUE` (default), will convert initial and terminal
  asterisks to partial matches.

- term.filter:

  A regular expression string used to format the text of each term
  (passed to `gsub`). For example, if terms are part-of-speech tagged
  (e.g., `'a_DT'`), `'_.*'` would remove the tag.

- term.break:

  If a category has more than `term.break` characters, it will be
  processed in chunks. Reduce from 20000 if you get a PCRE compilation
  error.

- to.lower:

  Logical; if `TRUE` will lowercase dictionary terms. Otherwise,
  dictionary terms will be converted to match the terms if they are
  single-cased. Set to `FALSE` to always keep dictionary terms as
  entered.

- dir:

  Path to a folder in which to look for `dict`;  
  will look in `'~/Dictionaries'` by default.  
  Set a session default with
  `options(lingmatch.dict.dir = 'desired/path')`.

- coverage:

  Logical; if `TRUE`, will calculate coverage (number of unique term
  matches) for each category.

## Value

A matrix with a row per `dtm` row and columns per dictionary category
(with added `coverage_` versions if `coverage` is `TRUE`), and a `WC`
attribute with original word counts.

## See also

For applying pattern-based dictionaries (to raw text) see
[`lma_patcat()`](https://miserman.github.io/lingmatch/reference/lma_patcat.md).

Other Dictionary functions:
[`dictionary_meta()`](https://miserman.github.io/lingmatch/reference/dictionary_meta.md),
[`download.dict()`](https://miserman.github.io/lingmatch/reference/download.dict.md),
[`lma_patcat()`](https://miserman.github.io/lingmatch/reference/lma_patcat.md),
[`read.dic()`](https://miserman.github.io/lingmatch/reference/read.dic.md),
[`report_term_matches()`](https://miserman.github.io/lingmatch/reference/report_term_matches.md),
[`select.dict()`](https://miserman.github.io/lingmatch/reference/select.dict.md)

## Examples

``` r
dict <- list(category = c("cat", "dog", "pet*"))
lma_termcat(c(
  "cat, cat, cat, cat, cat, cat, cat, cat",
  "a cat, dog, or anything petlike, really",
  "petite petrochemical petitioned petty peter for petrified petunia petals"
), dict, coverage = TRUE)
#>      category coverage_category
#> [1,]        8                 1
#> [2,]        3                 3
#> [3,]        8                 8
#> attr(,"WC")
#> [1] 8 7 9
#> attr(,"time")
#>     dtm termcat 
#>    0.02    0.02 
#> attr(,"type")
#> [1] "count"

if (FALSE) { # \dontrun{

# Score texts with the NRC Affect Intensity Lexicon

dict <- readLines("https://saifmohammad.com/WebDocs/NRC-AffectIntensity-Lexicon.txt")
dict <- read.table(
  text = dict[-seq_len(grep("term\tscore", dict, fixed = TRUE)[[1]])],
  col.names = c("term", "weight", "category")
)

text <- c(
  angry = paste(
    "We are outraged by their hateful brutality,",
    "and by the way they terrorize us with their hatred."
  ),
  fearful = paste(
    "The horrific torture of that terrorist was tantamount",
    "to the terrorism of terrorists."
  ),
  joyous = "I am jubilant to be celebrating the bliss of this happiest happiness.",
  sad = paste(
    "They are nearly suicidal in their mourning after",
    "the tragic and heartbreaking holocaust."
  )
)

emotion_scores <- lma_termcat(text, dict)
if (require("splot")) splot(emotion_scores ~ names(text), leg = "out")

## or use the standardized version (which includes more categories)

emotion_scores <- lma_termcat(text, "nrc_eil", dir = "~/Dictionaries")
emotion_scores <- emotion_scores[, c("anger", "fear", "joy", "sadness")]
if (require("splot")) splot(emotion_scores ~ names(text), leg = "out")
} # }
```
