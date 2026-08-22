# Generate a Report of Term Matches

Extract matches to fuzzy terms (globs/wildcards or regular expressions)
from provided text, in order to assess their appropriateness for
inclusion in a dictionary.

## Usage

``` r
report_term_matches(dict, text = NULL, space = NULL, glob = TRUE,
  parse_phrases = TRUE, tolower = TRUE, punct = TRUE, special = TRUE,
  as_terms = FALSE, bysentence = FALSE, as_string = TRUE,
  term_map_freq = 1, term_map_spaces = 1, outFile = NULL,
  space_dir = getOption("lingmatch.lspace.dir"), verbose = TRUE)
```

## Arguments

- dict:

  A vector of terms, list of such vectors, or a matrix-like object to be
  categorized by
  [`read.dic`](https://miserman.github.io/lingmatch/reference/read.dic.md).

- text:

  A vector of text to extract matches from. If not specified, will use
  the terms in the `term_map` retrieved from
  [`select.lspace`](https://miserman.github.io/lingmatch/reference/select.lspace.md).

- space:

  A vector space used to calculate similarities between term matches.
  Name of a the space (see
  [`select.lspace`](https://miserman.github.io/lingmatch/reference/select.lspace.md)),
  a matrix with terms as row names, or `TRUE` to auto-select a space
  based on matched terms.

- glob:

  Logical; if `TRUE`, converts globs (asterisk wildcards) to regular
  expressions. If not specified, this will be set automatically.

- parse_phrases:

  Logical; if `TRUE` (default) and `space` is specified, will break
  unmatched phrases into single terms, and average across and matched
  vectors.

- tolower:

  Logical; if `FALSE`, will retain `text`'s case.

- punct:

  Logical; if `FALSE`, will remove punctuation markings in `text`.

- special:

  Logical; if `FALSE`, will attempt to replace special characters in
  `text`.

- as_terms:

  Logical; if `TRUE`, will treat `text` as terms, meaning `dict` terms
  will only count as matches when matching the complete text.

- bysentence:

  Logical; if `TRUE`, will split `text` into sentences, and only
  consider unique sentences.

- as_string:

  Logical; if `FALSE`, returns matches as tables rather than a string.

- term_map_freq:

  Proportion of terms to include when using the term map as a source of
  terms. Applies when `text` is not specified.

- term_map_spaces:

  Number of spaces in which a term has to appear to be included. Applies
  when `text` is not specified.

- outFile:

  File path to write results to, always ending in `.csv`.

- space_dir:

  Directory from which `space` should be loaded.

- verbose:

  Logical; if `FALSE`, will not display status messages.

## Value

A `data.frame` of results, with a row for each unique term, and the
following columns:

- **`term`**: The originally entered term.

- **`regex`**: The converted and applied regular expression form of the
  term.

- **`categories`**: Comma-separated category names, if `dict` is a list
  with named entries.

- **`count`**: Total number of matches to the term.

- **`max_count`**: Number of matches to the most representative (that
  with the highest average similarity) variant of the term.

- **`variants`**: Number of variants of the term.

- **`space`**: Name of the latent semantic space, if one was used.

- **`mean_sim`**: Average similarity to the most representative variant
  among terms found in the space, if one was used.

- **`min_sim`**: Minimal similarity to the most representative variant.

- **`matches`**: Variants, with counts and similarity (Pearson's r) to
  the most representative term (if a space was specified). Either in the
  form of a comma-separated string or a `data.frame` (if `as_string` is
  `FALSE`).

## Note

Matches are extracted for each term independently, so they may not align
with some implementations of dictionaries. For instance, by default
[`lma_patcat`](https://miserman.github.io/lingmatch/reference/lma_patcat.md)
matches destructively, and sorts terms by length such that shorter terms
will not match the same text as longer terms that overlap. Here, the
match would show up for both terms.

## See also

For a more complete assessment of dictionaries, see
[`dictionary_meta()`](https://miserman.github.io/lingmatch/reference/dictionary_meta.md).

Similar information is provided in the [dictionary
builder](https://miserman.github.io/dictionary_builder/) web tool.

Other Dictionary functions:
[`dictionary_meta()`](https://miserman.github.io/lingmatch/reference/dictionary_meta.md),
[`download.dict()`](https://miserman.github.io/lingmatch/reference/download.dict.md),
[`lma_patcat()`](https://miserman.github.io/lingmatch/reference/lma_patcat.md),
[`lma_termcat()`](https://miserman.github.io/lingmatch/reference/lma_termcat.md),
[`read.dic()`](https://miserman.github.io/lingmatch/reference/read.dic.md),
[`select.dict()`](https://miserman.github.io/lingmatch/reference/select.dict.md)

## Examples

``` r
text <- c(
  "I am sadly homeless, and suffering from depression :(",
  "This wholesome happiness brings joy to my heart! :D:D:D",
  "They are joyous in these fearsome happenings D:",
  "I feel weightless now that my sadness has been depressed! :()"
)
dict <- list(
  sad = c("*less", "sad*", "depres*", ":("),
  happy = c("*some", "happ*", "joy*", "d:"),
  self = c("i *", "my *")
)

report_term_matches(dict, text)
#> preparing text (0)
#> preparing dict (0)
#> extracting matches (0)
#> preparing results (0)
#> done (0)
#>       term               regex categories count max_count variants
#> 1    *less   \\b[^\\s]*less\\b        sad     2         1        2
#> 2     sad*    \\bsad[^\\s]*\\b        sad     2         1        2
#> 3  depres* \\bdepres[^\\s]*\\b        sad     2         1        2
#> 4    *some   \\b[^\\s]*some\\b      happy     2         1        2
#> 5    happ*   \\bhapp[^\\s]*\\b      happy     2         1        2
#> 6     joy*    \\bjoy[^\\s]*\\b      happy     2         1        2
#> 7      i *     \\bi [^\\s]*\\b       self     2         1        2
#> 8     my *    \\bmy [^\\s]*\\b       self     2         1        2
#> 9       d:            \\bd:\\b      happy     2         2        1
#> 10      :(          \\b:\\(\\b        sad     0         0        0
#>                          matches
#> 1   homeless (1), weightless (1)
#> 2         sadly (1), sadness (1)
#> 3  depressed (1), depression (1)
#> 4    fearsome (1), wholesome (1)
#> 5  happiness (1), happenings (1)
#> 6            joy (1), joyous (1)
#> 7           i am (1), i feel (1)
#> 8   my heart (1), my sadness (1)
#> 9                         d: (2)
#> 10                              
```
