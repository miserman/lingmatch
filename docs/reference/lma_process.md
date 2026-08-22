# Process Text

A wrapper to other pre-processing functions, potentially from
[`read.segments`](https://miserman.github.io/lingmatch/reference/read.segments.md),
to
[`lma_dtm`](https://miserman.github.io/lingmatch/reference/lma_dtm.md)
or
[`lma_patcat`](https://miserman.github.io/lingmatch/reference/lma_patcat.md),
to
[`lma_weight`](https://miserman.github.io/lingmatch/reference/lma_weight.md),
then
[`lma_termcat`](https://miserman.github.io/lingmatch/reference/lma_termcat.md)
or
[`lma_lspace`](https://miserman.github.io/lingmatch/reference/lma_lspace.md),
and optionally including
[`lma_meta`](https://miserman.github.io/lingmatch/reference/lma_meta.md)
output.

## Usage

``` r
lma_process(input = NULL, ..., meta = TRUE, coverage = FALSE)
```

## Arguments

- input:

  A vector of text, or path to a text file or folder.

- ...:

  arguments to be passed to
  [`lma_dtm`](https://miserman.github.io/lingmatch/reference/lma_dtm.md),
  [`lma_patcat`](https://miserman.github.io/lingmatch/reference/lma_patcat.md),
  [`lma_weight`](https://miserman.github.io/lingmatch/reference/lma_weight.md),
  [`lma_termcat`](https://miserman.github.io/lingmatch/reference/lma_termcat.md),
  and/or
  [`lma_lspace`](https://miserman.github.io/lingmatch/reference/lma_lspace.md).
  All arguments must be named.

- meta:

  Logical; if `FALSE`, metastatistics are not included. Only applies
  when raw text is available. If included, meta categories are added as
  the last columns, with names starting with "meta\_".

- coverage:

  Logical; if `TRUE` and a dictionary is provided (`dict`), will
  calculate the coverage (number of unique term matches) of each
  dictionary category.

## Value

A matrix with texts represented by rows, and features in columns, unless
there are multiple rows per output (e.g., when a latent semantic space
is applied without terms being mapped) in which case only the special
output is returned (e.g., a matrix with terms as rows and latent
dimensions in columns).

## See also

If you just want to compare texts, see the
[`lingmatch()`](https://miserman.github.io/lingmatch/reference/lingmatch.md)
function.

## Examples

``` r
# starting with some texts in a vector
texts <- c(
  "Firstly, I would like to say, and with all due respect...",
  "Please, proceed. I hope you feel you can speak freely...",
  "Oh, of course, I just hope to be clear, and not cause offense...",
  "Oh, no, don't monitor yourself on my account..."
)

# by default, term counts and metastatistics are returned
lma_process(texts)
#>                                                               text account all
#> 1        Firstly, I would like to say, and with all due respect...       0   1
#> 2         Please, proceed. I hope you feel you can speak freely...       0   0
#> 3 Oh, of course, I just hope to be clear, and not cause offense...       0   0
#> 4                  Oh, no, don't monitor yourself on my account...       1   0
#>   and be can cause clear course don't due feel firstly freely hope i just like
#> 1   1  0   0     0     0      0     0   1    0       1      0    0 1    0    1
#> 2   0  0   1     0     0      0     0   0    1       0      1    1 1    0    0
#> 3   1  1   0     1     1      1     0   0    0       0      0    1 1    1    0
#> 4   0  0   0     0     0      0     1   0    0       0      0    0 0    0    0
#>   monitor my no not of offense oh on please proceed respect say speak to with
#> 1       0  0  0   0  0       0  0  0      0       0       1   1     0  1    1
#> 2       0  0  0   0  0       0  0  0      1       1       0   0     1  0    0
#> 3       0  0  0   1  1       1  1  0      0       0       0   0     0  1    0
#> 4       1  1  1   0  0       0  1  1      0       0       0   0     0  0    0
#>   would you yourself meta_characters meta_syllables meta_words
#> 1     1   0        0              42             12         11
#> 2     0   2        0              42             11         10
#> 3     0   0        0              46             14         13
#> 4     0   0        1              35             12          8
#>   meta_unique_words meta_clauses meta_sentences meta_words_per_clause
#> 1                11            3              1              3.666667
#> 2                 9            3              2              3.333333
#> 3                13            4              1              3.250000
#> 4                 8            3              1              2.666667
#>   meta_words_per_sentence meta_sixltr meta_characters_per_word
#> 1                      11           2                 3.818182
#> 2                       5           3                 4.200000
#> 3                      13           2                 3.538462
#> 4                       8           3                 4.375000
#>   meta_syllables_per_word meta_type_token_ratio meta_reading_grade meta_numbers
#> 1                1.090909                   1.0           1.572727            0
#> 2                1.100000                   0.9          -0.660000            0
#> 3                1.076923                   1.0           2.187692            0
#> 4                1.500000                   1.0           5.230000            0
#>   meta_puncts meta_periods meta_commas meta_qmarks meta_exclams meta_quotes
#> 1           5            3           2           0            0           0
#> 2           5            4           1           0            0           0
#> 3           6            3           3           0            0           0
#> 4           5            3           2           0            0           0
#>   meta_apostrophes meta_brackets meta_orgmarks
#> 1                0             0             0
#> 2                0             0             0
#> 3                0             0             0
#> 4                1             0             0

# add dictionary and percent arguments for standard dictionary-based results
lma_process(texts, dict = lma_dict(), percent = TRUE)
#>                                                               text     ppron
#> 1        Firstly, I would like to say, and with all due respect...  9.090909
#> 2         Please, proceed. I hope you feel you can speak freely... 30.000000
#> 3 Oh, of course, I just hope to be clear, and not cause offense...  7.692308
#> 4                  Oh, no, don't monitor yourself on my account... 25.000000
#>   ipron article   adverb     conj     prep   auxverb    negate    quant
#> 1     0       0  0.00000 9.090909 18.18182  9.090909  0.000000 9.090909
#> 2     0       0 10.00000 0.000000  0.00000 10.000000  0.000000 0.000000
#> 3     0       0 15.38462 7.692308 15.38462  7.692308  7.692308 0.000000
#> 4     0       0 12.50000 0.000000 12.50000 12.500000 25.000000 0.000000
#>   interrog   number interjection meta_characters meta_syllables meta_words
#> 1        0 9.090909     0.000000              42             12         11
#> 2        0 0.000000     0.000000              42             11         10
#> 3        0 0.000000     7.692308              46             14         13
#> 4        0 0.000000    12.500000              35             12          8
#>   meta_unique_words meta_clauses meta_sentences meta_words_per_clause
#> 1                11            3              1              3.666667
#> 2                 9            3              2              3.333333
#> 3                13            4              1              3.250000
#> 4                 8            3              1              2.666667
#>   meta_words_per_sentence meta_sixltr meta_characters_per_word
#> 1                      11    18.18182                 3.818182
#> 2                       5    30.00000                 4.200000
#> 3                      13    15.38462                 3.538462
#> 4                       8    37.50000                 4.375000
#>   meta_syllables_per_word meta_type_token_ratio meta_reading_grade meta_numbers
#> 1                1.090909                   1.0           1.572727            0
#> 2                1.100000                   0.9          -0.660000            0
#> 3                1.076923                   1.0           2.187692            0
#> 4                1.500000                   1.0           5.230000            0
#>   meta_puncts meta_periods meta_commas meta_qmarks meta_exclams meta_quotes
#> 1    45.45455     27.27273    18.18182           0            0           0
#> 2    50.00000     40.00000    10.00000           0            0           0
#> 3    46.15385     23.07692    23.07692           0            0           0
#> 4    62.50000     37.50000    25.00000           0            0           0
#>   meta_apostrophes meta_brackets meta_orgmarks
#> 1              0.0             0             0
#> 2              0.0             0             0
#> 3              0.0             0             0
#> 4             12.5             0             0

# add space and weight arguments for standard word-centroid vectors
lma_process(texts, space = lma_lspace(texts), weight = "tfidf")
#>                                                               text          V1
#> 1        Firstly, I would like to say, and with all due respect... -0.07509574
#> 2         Please, proceed. I hope you feel you can speak freely... -0.08118897
#> 3 Oh, of course, I just hope to be clear, and not cause offense... -0.09709788
#> 4                  Oh, no, don't monitor yourself on my account... -0.01913927
#>            V2 meta_characters meta_syllables meta_words meta_unique_words
#> 1  0.05737681              42             12         11                11
#> 2 -0.16696471              42             11         10                 9
#> 3  0.03907020              46             14         13                13
#> 4  0.01993145              35             12          8                 8
#>   meta_clauses meta_sentences meta_words_per_clause meta_words_per_sentence
#> 1            3              1              3.666667                      11
#> 2            3              2              3.333333                       5
#> 3            4              1              3.250000                      13
#> 4            3              1              2.666667                       8
#>   meta_sixltr meta_characters_per_word meta_syllables_per_word
#> 1   0.1818182                 3.818182                1.090909
#> 2   0.3000000                 4.200000                1.100000
#> 3   0.1538462                 3.538462                1.076923
#> 4   0.3750000                 4.375000                1.500000
#>   meta_type_token_ratio meta_reading_grade meta_numbers meta_puncts
#> 1                   1.0           1.572727            0   0.4545455
#> 2                   0.9          -0.660000            0   0.5000000
#> 3                   1.0           2.187692            0   0.4615385
#> 4                   1.0           5.230000            0   0.6250000
#>   meta_periods meta_commas meta_qmarks meta_exclams meta_quotes
#> 1    0.2727273   0.1818182           0            0           0
#> 2    0.4000000   0.1000000           0            0           0
#> 3    0.2307692   0.2307692           0            0           0
#> 4    0.3750000   0.2500000           0            0           0
#>   meta_apostrophes meta_brackets meta_orgmarks
#> 1            0.000             0             0
#> 2            0.000             0             0
#> 3            0.000             0             0
#> 4            0.125             0             0
```
