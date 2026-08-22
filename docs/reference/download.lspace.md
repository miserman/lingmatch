# Download Latent Semantic Spaces

Downloads the specified semantic space from
[osf.io/489he](https://osf.io/489he).

## Usage

``` r
download.lspace(space = "100k_lsa", decompress = TRUE, check.md5 = TRUE,
  mode = "wb", dir = getOption("lingmatch.lspace.dir"),
  overwrite = FALSE)
```

## Arguments

- space:

  Name of one or more spaces you want to download, or `'all'` for all
  available. `'100k_lsa'` is the default, and some other common options
  might be `'google'`, `'facebook'`, or `'glove'`. See
  [osf.io/489he/wiki](https://osf.io/489he/wiki/home) for more
  information, and a full list of spaces.

- decompress:

  Logical; if `TRUE` (default), decompresses the downloaded file with
  the `bunzip2` system command assuming it is available  
  (as indicated by `Sys.which('bunzip2')`).

- check.md5:

  Logical; if `TRUE` (default), retrieves the MD5 checksum from OSF, and
  compares it with that calculated from the downloaded file to check its
  integrity.

- mode:

  A character specifying the file write mode; default is 'wb'. See
  [`download.file`](https://rdrr.io/r/utils/download.file.html).

- dir:

  Directory in which to save the space. Specify this here, or set the
  lspace directory option (e.g.,
  `options(lingmatch.lspace.dir = '~/Latent Semantic Spaces')`), or use
  [`lma_initdirs`](https://miserman.github.io/lingmatch/reference/lma_initdirs.md)
  to initialize a directory.

- overwrite:

  Logical; if `TRUE`, will replace existing files.

## Value

A character vector with paths to the \[1\] data and \[2\] term files.

## See also

Other Latent Semantic Space functions:
[`lma_lspace()`](https://miserman.github.io/lingmatch/reference/lma_lspace.md),
[`select.lspace()`](https://miserman.github.io/lingmatch/reference/select.lspace.md),
[`standardize.lspace()`](https://miserman.github.io/lingmatch/reference/standardize.lspace.md)

## Examples

``` r
if (FALSE) { # \dontrun{

download.lspace("glove_crawl", dir = "~/Latent Semantic Spaces")
} # }
```
