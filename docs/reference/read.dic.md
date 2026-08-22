# Read/Write Dictionary Files

Read in or write dictionary files in Comma-Separated Values (.csv;
weighted) or Linguistic Inquiry and Word Count (.dic; non-weighted)
format.

## Usage

``` r
read.dic(path, cats = NULL, type = "asis", as.weighted = FALSE,
  dir = getOption("lingmatch.dict.dir"), ..., term.name = "term",
  category.name = "category", raw = FALSE)

write.dic(dict, filename = NULL, type = "asis", as.weighted = FALSE,
  save = TRUE)
```

## Arguments

- path:

  Path to a file, a name corresponding to a file in
  `getOption('lingmatch.dict.dir')` (or `'~/Dictionaries'`) or one of
  the dictionaries available at [osf.io/y6g5b](https://osf.io/y6g5b), a
  matrix-like object to be categorized, or a list to be formatted.

- cats:

  A character vector of category names to be returned. All categories
  are returned by default.

- type:

  A character indicating whether and how terms should be altered.
  Unspecified or matching 'asis' leaves terms as they are. Other options
  change wildcards to regular expressions: `'pattern'` (`'^[poi]'`)
  replaces initial asterisks with `'\\b\\w*'`, and terminal asterisks
  with `'\\w*\\b'`, to match terms within raw text; for anything else,
  terms are padded with `^` and `$`, then those bounding marks are
  removed when an asterisk is present, to match tokenized terms.

- as.weighted:

  Logical; if `TRUE`, prevents weighted dictionaries from being
  converted to unweighted versions, or converts unweighted dictionaries
  to a binary weighted version – a data.frame with a "term" column of
  unique terms, and a column for each category.

- dir:

  Path to a folder containing dictionaries, or where you would like
  dictionaries to be downloaded; passed to
  [`select.dict`](https://miserman.github.io/lingmatch/reference/select.dict.md)
  and/or
  [`download.dict`](https://miserman.github.io/lingmatch/reference/download.dict.md).

- ...:

  Passes arguments to
  [`readLines`](https://rdrr.io/r/base/readLines.html).

- term.name, category.name:

  Strings identifying column names in `path` containing terms and
  categories respectively.

- raw:

  Logical or a character. As logical, indicates if `path` should be
  treated as a raw dictionary (as might be read in from a .dic file). As
  a character, replaces `path` as if it were read in from a file.

- dict:

  A `list` with a named entry of terms for each category, or a
  `data.frame` with terms in one column, and categories or weights in
  the rest.

- filename:

  The name of the file to be saved.

- save:

  Logical: if `FALSE`, does not write a file.

## Value

`read.dic`: A `list` (unweighted) with an entry for each category
containing character vectors of terms, or a `data.frame` (weighted) with
columns for terms (first, "term") and weights (all subsequent, with
category labels as names).

`write.dic`: A version of the written dictionary – a raw character
vector for unweighted dictionaries, or a `data.frame` for weighted
dictionaries.

## See also

Other Dictionary functions:
[`dictionary_meta()`](https://miserman.github.io/lingmatch/reference/dictionary_meta.md),
[`download.dict()`](https://miserman.github.io/lingmatch/reference/download.dict.md),
[`lma_patcat()`](https://miserman.github.io/lingmatch/reference/lma_patcat.md),
[`lma_termcat()`](https://miserman.github.io/lingmatch/reference/lma_termcat.md),
[`report_term_matches()`](https://miserman.github.io/lingmatch/reference/report_term_matches.md),
[`select.dict()`](https://miserman.github.io/lingmatch/reference/select.dict.md)

## Examples

``` r
# make a small murder related dictionary
dict <- list(
  kill = c("kill*", "murd*", "wound*", "die*"),
  death = c("death*", "dying", "die*", "kill*")
)

# convert it to a weighted format
(dict_weighted <- read.dic(dict, as.weighted = TRUE))
#>     term kill death
#> 1  kill*    1     1
#> 2  murd*    1     0
#> 3 wound*    1     0
#> 4   die*    1     1
#> 5 death*    0     1
#> 6  dying    0     1

# categorize it back
read.dic(dict_weighted)
#> $kill
#> [1] "kill*"  "murd*"  "wound*" "die*"  
#> 
#> $death
#> [1] "kill*"  "die*"   "death*" "dying" 
#> 

# convert it to a string without writing to a file
cat(raw_dict <- write.dic(dict, save = FALSE))
#> %
#> 1    kill
#> 2    death
#> %
#> kill*    1   2
#> murd*    1
#> wound*   1
#> die* 1   2
#> death*   2
#> dying    2

# parse it back in
read.dic(raw = raw_dict)
#> $kill
#> [1] "kill*"  "murd*"  "wound*" "die*"  
#> 
#> $death
#> [1] "kill*"  "die*"   "death*" "dying" 
#> 

if (FALSE) { # \dontrun{

# save it as a .dic file
write.dic(dict, "murder")

# read it back in as a list
read.dic("murder.dic")

# read in the Moral Foundations or LUSI dictionaries from urls
moral_dict <- read.dic("https://osf.io/download/whjt2")
lusi_dict <- read.dic("https://osf.io/download/29ayf")

# save and read in a version of the General Inquirer dictionary
inquirer <- read.dic("inquirer", dir = "~/Dictionaries")
} # }
```
