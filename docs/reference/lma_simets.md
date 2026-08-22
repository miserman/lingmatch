# Similarity Calculations

Enter a numerical matrix, set of vectors, or set of matrices to
calculate similarity per vector.

## Usage

``` r
lma_simets(a, b = NULL, metric = NULL, group = NULL, lag = 0,
  agg = TRUE, agg.mean = TRUE, pairwise = TRUE, symmetrical = FALSE,
  mean = FALSE, return.list = FALSE)
```

## Arguments

- a:

  A vector or matrix. If a vector, `b` must also be provided. If a
  matrix and `b` is missing, each row will be compared. If a matrix and
  `b` is not missing, each row will be compared with `b` or each row of
  `b`.

- b:

  A vector or matrix to be compared with `a` or rows of `a`.

- metric:

  A character or vector of characters at least partially matching one of
  the available metric names (or 'all' to explicitly include all
  metrics), or a number or vector of numbers indicating the metric by
  index:

  - **`jaccard`**: `sum(a & b) / sum(a | b)`

  - **`euclidean`**: `1 / (1 + sqrt(sum((a - b) ^ 2)))`

  - **`canberra`**: `mean(1 - abs(a - b) / (a + b))`

  - **`cosine`**: `sum(a * b) / sqrt(sum(a ^ 2 * sum(b ^ 2)))`

  - **`pearson`**: `(mean(a * b) - (mean(a) * mean(b))) /`  
    `sqrt(mean(a ^ 2) - mean(a) ^ 2) / sqrt(mean(b ^ 2) - mean(b) ^ 2)`

- group:

  If `b` is missing and `a` has multiple rows, this will be used to make
  comparisons between rows of `a`, as modified by `agg` and `agg.mean`.

- lag:

  Amount to adjust the `b` index; either rows if `b` has multiple rows
  (e.g., for `lag = 1`, `a[1, ]` is compared with `b[2, ]`), or values
  otherwise (e.g., for `lag = 1`, `a[1]` is compared with `b[2]`). If
  `b` is not supplied, `b` is a copy of `a`, resulting in lagged
  self-comparisons or autocorrelations.

- agg:

  Logical: if `FALSE`, only the boundary rows between groups will be
  compared, see example.

- agg.mean:

  Logical: if `FALSE` aggregated rows are summed instead of averaged.

- pairwise:

  Logical: if `FALSE` and `a` and `b` are matrices with the same number
  of rows, only paired rows are compared. Otherwise (and if only `a` is
  supplied), all pairwise comparisons are made.

- symmetrical:

  Logical: if `TRUE` and pairwise comparisons between `a` rows were
  made, the results in the lower triangle are copied to the upper
  triangle.

- mean:

  Logical: if `TRUE`, a single mean for each metric is returned per row
  of `a`.

- return.list:

  Logical: if `TRUE`, a list-like object will always be returned, with
  an entry for each metric, even when only one metric is requested.

## Value

Output varies based on the dimensions of `a` and `b`:

- **Out:** A vector with a value per metric.  
  **In:** Only when `a` and `b` are both vectors.

- **Out:** A vector with a value per row.  
  **In:** Any time a single value is expected per row: `a` or `b` is a
  vector, `a` and `b` are matrices with the same number of rows and
  `pairwise = FALSE`, a group is specified, or `mean = TRUE`, and only
  one metric is requested.

- **Out:** A data.frame with a column per metric.  
  **In:** When multiple metrics are requested in the previous case.

- **Out:** A sparse matrix with a `metric` attribute with the metric
  name.  
  **In:** Pairwise comparisons within an `a` matrix or between an `a`
  and `b` matrix, when only 1 metric is requested.

- **Out:** A list with a sparse matrix per metric.  
  **In:** When multiple metrics are requested in the previous case.

## Details

Use
[`setThreadOptions`](https://rdrr.io/pkg/RcppParallel/man/setThreadOptions.html)
to change parallelization options; e.g., run
RcppParallel::setThreadOptions(4) before a call to lma_simets to set the
number of CPU threads to 4.

## Examples

``` r
text <- c(
  "words of speaker A", "more words from speaker A",
  "words from speaker B", "more words from speaker B"
)
(dtm <- lma_dtm(text))
#> 4 x 7 sparse Matrix of class "dgCMatrix"
#>      a b from more of speaker words
#> [1,] 1 .    .    .  1       1     1
#> [2,] 1 .    1    1  .       1     1
#> [3,] . 1    1    .  .       1     1
#> [4,] . 1    1    1  .       1     1

# compare each entry
lma_simets(dtm)
#> $jaccard
#> 4 x 4 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                               
#> [1,] I         .         .   .
#> [2,] 0.5000000 I         .   .
#> [3,] 0.3333333 0.5000000 I   .
#> [4,] 0.2857143 0.6666667 0.8 I
#> 
#> $euclidean
#> 4 x 4 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                               
#> [1,] I         .         .   .
#> [2,] 0.3660254 I         .   .
#> [3,] 0.3333333 0.3660254 I   .
#> [4,] 0.3090170 0.4142136 0.5 I
#> 
#> $canberra
#> 4 x 4 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                                     
#> [1,] I         .         .         .
#> [2,] 0.5714286 I         .         .
#> [3,] 0.4285714 0.5714286 I         .
#> [4,] 0.2857143 0.7142857 0.8571429 I
#> 
#> $cosine
#> 4 x 4 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                                     
#> [1,] I         .         .         .
#> [2,] 0.6708204 I         .         .
#> [3,] 0.5000000 0.6708204 I         .
#> [4,] 0.4472136 0.8000000 0.8944272 I
#> 
#> $pearson
#> 4 x 4 sparse Matrix of class "dtCMatrix" (unitriangular)
#>                                        
#> [1,]  I          .          .         .
#> [2,]  0.09128709 I          .         .
#> [3,] -0.16666667 0.09128709 I         .
#> [4,] -0.54772256 0.30000000 0.7302967 I
#> 
#> attr(,"time")
#> simets 
#>      0 

# compare each entry with the mean of all entries
lma_simets(dtm, colMeans(dtm))
#>     jaccard euclidean  canberra    cosine   pearson
#> 1 0.5714286 0.4220645 0.4380952 0.7484552 0.1964186
#> 2 0.7142857 0.5166852 0.5986395 0.9128709 0.6454972
#> 3 0.5714286 0.5166852 0.5034014 0.8845380 0.7463905
#> 4 0.7142857 0.5166852 0.5986395 0.9128709 0.6454972

# compare by group (corresponding to speakers and turns in this case)
speaker <- c("A", "A", "B", "B")

## by default, consecutive rows from the same group are averaged:
lma_simets(dtm, group = speaker)
#>                 jaccard euclidean  canberra    cosine    pearson
#> 1, 2 <-> 3, 4 0.5714286 0.3874259 0.5238095 0.6888467 -0.1324532

## with agg = FALSE, only the rows at the boundary between
## groups (rows 2 and 3 in this case) are used:
lma_simets(dtm, group = speaker, agg = FALSE)
#>         jaccard euclidean  canberra    cosine    pearson
#> 2 <-> 3     0.5 0.3660254 0.5714286 0.6708204 0.09128709
```
