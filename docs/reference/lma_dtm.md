# Document-Term Matrix Creation

Creates a document-term matrix (dtm) from a set of texts.

## Usage

``` r
lma_dtm(text, exclude = NULL, context = NULL, replace.special = FALSE,
  numbers = FALSE, punct = FALSE, urls = TRUE, emojis = FALSE,
  to.lower = TRUE, word.break = " +", dc.min = 0, dc.max = Inf,
  sparse = TRUE, tokens.only = FALSE)
```

## Arguments

- text:

  Texts to be processed. This can be a vector (such as a column in a
  data frame) or list. When a list, these can be in the form returned
  with `tokens.only = TRUE`, or a list with named vectors, where names
  are tokens and values are frequencies or the like.

- exclude:

  A character vector of words to be excluded. If `exclude` is a single
  string matching `'function'`, `lma_dict(1:9)` will be used.

- context:

  A character vector used to reformat text based on look- ahead/behind.
  For example, you might attempt to disambiguate *like* by reformatting
  certain *like*s (e.g.,
  `context = c('(i) like*', '(you) like*', '(do) like')`, where words in
  parentheses are the context for the target word, and asterisks denote
  partial matching). This would be converted to regular expression
  (i.e., `'(? <= i) like\\b'`) which, if matched, would be replaced with
  a coded version of the word (e.g., `"Hey, i like that!"` would become
  `"Hey, i i-like that!"`). This would probably only be useful for
  categorization, where a dictionary would only include one or another
  version of a word (e.g., the LIWC 2015 dictionary does something like
  this with *like*, and LIWC 2007 did something like this with *kind
  (of)*, both to try and clean up the posemo category).

- replace.special:

  Logical: if `TRUE`, special characters are replaced with regular
  equivalents using the
  [`lma_dict`](https://miserman.github.io/lingmatch/reference/lma_dict.md)
  special function.

- numbers:

  Logical: if `TRUE`, numbers are preserved.

- punct:

  Logical: if `TRUE`, punctuation is preserved.

- urls:

  Logical: if `FALSE`, attempts to replace all urls with "repurl".

- emojis:

  Logical: if `TRUE`, attempts to replace emojis (e.g., ":(" would be
  replaced with "repfrown").

- to.lower:

  Logical: if `FALSE`, words with different capitalization are treated
  as different terms.

- word.break:

  A regular expression string determining the way words are split.
  Default is `' +'` which breaks words at one or more blank spaces. You
  may also like to break by dashes or slashes (`'[ /-]+'`), depending on
  the text.

- dc.min:

  Numeric: excludes terms appearing in the set number or fewer
  documents. Default is 0 (no limit).

- dc.max:

  Numeric: excludes terms appearing in the set number or more. Default
  is Inf (no limit).

- sparse:

  Logical: if `FALSE`, a regular dense matrix is returned.

- tokens.only:

  Logical: if `TRUE`, returns a list rather than a matrix, with these
  entries:

  |               |                                                          |
  |---------------|----------------------------------------------------------|
  | `tokens`      | A vector of indices with terms as names.                 |
  | `frequencies` | A vector of counts with terms as names.                  |
  | `WC`          | A vector of term counts for each document.               |
  | `indices`     | A list with a vector of token indices for each document. |

## Value

A sparse matrix (or regular matrix if `sparse = FALSE`), with a row per
`text`, and column per term, or a list if `tokens.only = TRUE`. Includes
an attribute with options (`opts`), and attributes with word count
(`WC`) and column sums (`colsums`) if `tokens.only = FALSE`.

## Note

This is a relatively simple way to make a dtm. To calculate the (more or
less) standard forms of LSM and LSS, a somewhat raw dtm should be fine,
because both processes essentially use dictionaries (obviating stemming)
and weighting or categorization (largely obviating 'stop word' removal).
The exact effect of additional processing will depend on the
dictionary/semantic space and weighting scheme used (particularly for
LSA). This function also does some processing which may matter if you
plan on categorizing with categories that have terms with look-
ahead/behind assertions (like LIWC dictionaries). Otherwise, other
methods may be faster, more memory efficient, and/or more featureful.

## Examples

``` r
text <- c(
  "Why, hello there! How are you this evening?",
  "I am well, thank you for your inquiry!",
  "You are a most good at social interactions person!",
  "Why, thank you! You're not all bad yourself!"
)

lma_dtm(text)
#> 4 x 27 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 27 column names 'a', 'all', 'am' ... ]]
#>                                                           
#> [1,] . . . 1 . . 1 . . 1 1 . . . . . . . . 1 1 . 1 1 . . .
#> [2,] . . 1 . . . . 1 . . . 1 1 . . . . . 1 . . 1 . 1 . 1 .
#> [3,] 1 . . 1 1 . . . 1 . . . . 1 1 . 1 1 . . . . . 1 . . .
#> [4,] . 1 . . . 1 . . . . . . . . . 1 . . 1 . . . 1 1 1 . 1

# return tokens only
(tokens <- lma_dtm(text, tokens.only = TRUE))
#> $tokens
#>            a          all           am          are           at          bad 
#>            1            2            3            4            5            6 
#>      evening          for         good        hello          how            i 
#>            7            8            9           10           11           12 
#>      inquiry interactions         most          not       person       social 
#>           13           14           15           16           17           18 
#>        thank        there         this         well          why          you 
#>           19           20           21           22           23           24 
#>       you're         your     yourself 
#>           25           26           27 
#> 
#> $frequencies
#>            a          all           am          are           at          bad 
#>            1            1            1            2            1            1 
#>      evening          for         good        hello          how            i 
#>            1            1            1            1            1            1 
#>      inquiry interactions         most          not       person       social 
#>            1            1            1            1            1            1 
#>        thank        there         this         well          why          you 
#>            2            1            1            1            2            4 
#>       you're         your     yourself 
#>            1            1            1 
#> 
#> $WC
#> [1] 8 8 9 8
#> 
#> $indices
#> $indices[[1]]
#> [1] 23 10 20 11  4 24 21  7
#> 
#> $indices[[2]]
#> [1] 12  3 22 19 24  8 26 13
#> 
#> $indices[[3]]
#> [1] 24  4  1 15  9  5 18 14 17
#> 
#> $indices[[4]]
#> [1] 23 19 24 25 16  2  6 27
#> 
#> 
#> attr(,"opts")
#>  numbers    punct     urls to.lower 
#>    FALSE    FALSE     TRUE     TRUE 
#> attr(,"time")
#> dtm 
#>   0 

## convert those to a regular DTM
lma_dtm(tokens)
#> 4 x 27 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 27 column names 'a', 'all', 'am' ... ]]
#>                                                           
#> [1,] . . . 1 . . 1 . . 1 1 . . . . . . . . 1 1 . 1 1 . . .
#> [2,] . . 1 . . . . 1 . . . 1 1 . . . . . 1 . . 1 . 1 . 1 .
#> [3,] 1 . . 1 1 . . . 1 . . . . 1 1 . 1 1 . . . . . 1 . . .
#> [4,] . 1 . . . 1 . . . . . . . . . 1 . . 1 . . . 1 1 1 . 1

# convert a list-representation to a sparse matrix
lma_dtm(list(
  doc1 = c(why = 1, hello = 1, there = 1),
  doc2 = c(i = 1, am = 1, well = 1)
))
#> 2 x 6 sparse Matrix of class "dgCMatrix"
#>      why hello there i am well
#> doc1   1     1     1 .  .    .
#> doc2   .     .     . 1  1    1
```
