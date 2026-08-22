# Select Latent Semantic Spaces

Retrieve information and links to latent semantic spaces (sets of word
vectors/embeddings) available at [osf.io/489he](https://osf.io/489he),
and optionally download their term mappings
([osf.io/xr7jv](https://osf.io/xr7jv)).

## Usage

``` r
select.lspace(query = NULL, dir = getOption("lingmatch.lspace.dir"),
  terms = NULL, get.map = FALSE, check.md5 = TRUE, mode = "wb")
```

## Arguments

- query:

  A character used to select spaces, based on names or other features.
  If length is over 1, `get.map` is set to `TRUE`. Use `terms` alone to
  select spaces based on term coverage.

- dir:

  Path to a directory containing `lma_term_map.rda` and downloaded
  spaces;  
  will look in `getOption('lingmatch.lspace.dir')` and
  `'~/Latent Semantic Spaces'` by default.

- terms:

  A character vector of terms to search for in the downloaded term map,
  to calculate coverage of spaces, or select by coverage if `query` is
  not specified.

- get.map:

  Logical; if `TRUE` and `lma_term_map.rda` is not found in `dir`, the
  term map ([lma_term_map.rda](https://osf.io/xr7jv)) is downloaded and
  decompressed.

- check.md5:

  Logical; if `TRUE` (default), retrieves the MD5 checksum from OSF, and
  compares it with that calculated from the downloaded file to check its
  integrity.

- mode:

  Passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html) when
  downloading the term map.

## Value

A list with varying entries:

- **`info`**: The version of [osf.io/9yzca](https://osf.io/9yzca) stored
  internally; a `data.frame` with spaces as row names, and information
  about each space in columns:

  - **`terms`**: number of terms in the space

  - **`corpus`**: corpus(es) on which the space was trained

  - **`model`**: model from which the space was trained

  - **`dimensions`**: number of dimensions in the model (columns of the
    space)

  - **`model_info`**: some parameter details about the model

  - **`original_max`**: maximum value used to normalize the space; the
    original space would be `(vectors *` `original_max) /` `100`

  - **`osf_dat`**: OSF id for the `.dat` files; the URL would be
    https://osf.io/`osf_dat`

  - **`osf_terms`**: OSF id for the `_terms.txt` files; the URL would be
    https://osf.io/`osf_terms`

  - **`wiki`**: link to the wiki for the space

  - **`downloaded`**: path to the `.dat` file if downloaded, and `''`
    otherwise.

- **`selected`**: A subset of `info` selected by `query`.

- **`term_map`**: If `get.map` is `TRUE` or `lma_term_map.rda` is found
  in `dir`, a copy of [osf.io/xr7jv](https://osf.io/xr7jv), which has
  space names as column names, terms as row names, and indices as
  values, with 0 indicating the term is not present in the associated
  space.

## See also

Other Latent Semantic Space functions:
[`download.lspace()`](https://miserman.github.io/lingmatch/reference/download.lspace.md),
[`lma_lspace()`](https://miserman.github.io/lingmatch/reference/lma_lspace.md),
[`standardize.lspace()`](https://miserman.github.io/lingmatch/reference/standardize.lspace.md)

## Examples

``` r
# just retrieve information about available spaces
spaces <- select.lspace()
spaces$info[1:10, c("terms", "dimensions", "original_max")]
#>                   terms dimensions original_max
#> 100k              99188        300  129.8630950
#> 100k_cbow         99186        300    7.3177360
#> 100k_lsa          99188        300 1144.5951390
#> blogs             27277        300    0.4780927
#> CoNLL17_skipgram 459818        100    4.1434050
#> dcp_cbow         215142        400    1.8966740
#> dcp_svd          215142        500    0.6560584
#> eigenwords       159908        200    1.0000000
#> eigenwords_tscca  55879        200    1.5136493
#> facebook_crawl    81653        300    4.5530000

# retrieve all spaces that used word2vec
w2v_spaces <- select.lspace("word2vec")$selected
w2v_spaces[, c("terms", "dimensions", "original_max")]
#>                   terms dimensions original_max
#> 100k_cbow         99186        300    7.3177360
#> CoNLL17_skipgram 459818        100    4.1434050
#> dcp_cbow         215142        400    1.8966740
#> google           345655        300    2.8437500
#> nasari           151776        300    0.7317708
#> paragram_sl999   456295        300    4.0129000
#> paragram_ws353   456295        300    4.0129000
#> sensembed        409078        400    1.7170870
#> ukwac_cbow       171187        400    6.6162160

if (FALSE) { # \dontrun{

# select spaces by terms
select.lspace(terms = c(
  "part-time", "i/o", "'cause", "brexit", "debuffs"
))$selected[, c("terms", "coverage")]
} # }
```
