# Latent Semantic Space (Embeddings) Operations

Map a document-term matrix onto a latent semantic space, extract terms
from a latent semantic space (if `dtm` is a character vector, or
`map.space =` `FALSE`), or perform a singular value decomposition of a
document-term matrix (if `dtm` is a matrix and `space` is missing).

## Usage

``` r
lma_lspace(dtm = "", space, map.space = TRUE, fill.missing = FALSE,
  term.map = NULL, dim.cutoff = 0.5, keep.dim = FALSE,
  use.scan = FALSE, dir = getOption("lingmatch.lspace.dir"))
```

## Arguments

- dtm:

  A matrix with terms as column names, or a character vector of terms to
  be extracted from a specified space. If this is of length 1 and
  `space` is missing, it will be treated as `space`.

- space:

  A matrix with terms as rownames. If missing, this will be the right
  singular vectors of a singular value decomposition of `dtm`. If a
  character, a file matching the character will be searched for in `dir`
  (e.g., `space = 'google'`). If a file is not found and the character
  matches one of the [available spaces](https://osf.io/489he/wiki/home),
  you will be given the option to download it, as handled by
  [`download.lspace`](https://miserman.github.io/lingmatch/reference/download.lspace.md).
  If `dtm` is missing, the entire space will be loaded and returned.

- map.space:

  Logical: if `FALSE`, the original vectors of `space` for terms found
  in `dtm` are returned. Otherwise `dtm` `%*%` `space` is returned,
  excluding uncommon columns of `dtm` and rows of `space`.

- fill.missing:

  Logical: if `TRUE` and terms are being extracted from a space,
  includes terms not found in the space as rows of 0s, such that the
  returned matrix will have a row for every requested term.

- term.map:

  A matrix with `space` as a column name, terms as row names, and
  indices of the terms in the given space as values, or a numeric vector
  of indices with terms as names, or a character vector of terms
  corresponding to rows of the space. This is used instead of reading in
  an "\_terms.txt" file corresponding to a `space` entered as a
  character (the name of a space file).

- dim.cutoff:

  If a `space` is calculated, this will be used to decide on the number
  of dimensions to be retained: `cumsum(d) / sum(d) < dim.cutoff`, where
  `d` is a vector of singular values of `dtm` (i.e., `svd(dtm)$d`). The
  default is `.5`; lower cutoffs result in fewer dimensions.

- keep.dim:

  Logical: if `TRUE`, and a space is being calculated from the input, a
  matrix in the same dimensions as `dtm` is returned. Otherwise, a
  matrix with terms as rows and dimensions as columns is returned.

- use.scan:

  Logical: if `TRUE`, reads in the rows of `space` with
  [`scan`](https://rdrr.io/r/base/scan.html).

- dir:

  Path to a folder containing spaces.  
  Set a session default with
  `options(lingmatch.lspace.dir = 'desired/path')`.

## Value

A matrix or sparse matrix with either (a) a row per term and column per
latent dimension (a latent space, either calculated from the input, or
retrieved when `map.space = FALSE`), (b) a row per document and column
per latent dimension (when a dtm is mapped to a space), or (c) a row per
document and column per term (when a space is calculated and
`keep.dim = TRUE`).

## Note

A traditional latent semantic space is a selection of right singular
vectors from the singular value decomposition of a dtm
(`svd(dtm)$v[, 1:k]`, where `k` is the selected number of dimensions,
decided here by `dim.cutoff`).

Mapping a new dtm into a latent semantic space consists of multiplying
common terms: `dtm[, ct]` `%*% space[ct, ]`, where `ct` `=`
`colnames(dtm)[colnames(dtm)` `%in%` `rownames(space)]` – the terms
common between the dtm and the space. This results in a matrix with
documents as rows, and dimensions as columns, replacing terms.

## See also

Other Latent Semantic Space functions:
[`download.lspace()`](https://miserman.github.io/lingmatch/reference/download.lspace.md),
[`select.lspace()`](https://miserman.github.io/lingmatch/reference/select.lspace.md),
[`standardize.lspace()`](https://miserman.github.io/lingmatch/reference/standardize.lspace.md)

## Examples

``` r
text <- c(
  paste(
    "Hey, I like kittens. I think all kinds of cats really are just the",
    "best pet ever."
  ),
  paste(
    "Oh year? Well I really like cars. All the wheels and the turbos...",
    "I think that's the best ever."
  ),
  paste(
    "You know what? Poo on you. Cats, dogs, rabbits -- you know, living",
    "creatures... to think you'd care about anything else!"
  ),
  paste(
    "You can stick to your opinion. You can be wrong if you want. You know",
    "what life's about? Supercharging, diesel guzzling, exhaust spewing,",
    "piston moving ignitions."
  )
)

dtm <- lma_dtm(text)

# calculate a latent semantic space from the example text
lss <- lma_lspace(dtm)

# show that document similarities between the truncated and full space are the same
spaces <- list(
  full = lma_lspace(dtm, keep.dim = TRUE),
  truncated = lma_lspace(dtm, lss)
)
sapply(spaces, lma_simets, metric = "cosine")
#> $full
#> 4 x 4 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                                        
#> [1,] I            .          .        .
#> [2,] 0.999420475  I          .        .
#> [3,] 0.140738442  0.10695580 I        .
#> [4,] 0.001947292 -0.03209365 0.990319 I
#> 
#> $truncated
#> 4 x 4 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                                        
#> [1,] I            .          .        .
#> [2,] 0.999420475  I          .        .
#> [3,] 0.140738442  0.10695580 I        .
#> [4,] 0.001947292 -0.03209365 0.990319 I
#> 

if (FALSE) { # \dontrun{

# specify a directory containing spaces,
# or where you would like to download spaces
space_dir <- "~/Latent Semantic Spaces"

# map to a pretrained space
ddm <- lma_lspace(dtm, "100k", dir = space_dir)

# load the matching subset of the space
# without mapping
lss_100k_part <- lma_lspace(colnames(dtm), "100k", dir = space_dir)

## or
lss_100k_part <- lma_lspace(dtm, "100k", map.space = FALSE, dir = space_dir)

# load the full space
lss_100k <- lma_lspace("100k", dir = space_dir)

## or
lss_100k <- lma_lspace(space = "100k", dir = space_dir)
} # }
```
