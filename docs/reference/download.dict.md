# Download Dictionaries

Downloads the specified dictionaries from
[osf.io/y6g5b](https://osf.io/y6g5b).

## Usage

``` r
download.dict(dict = "lusi", check.md5 = TRUE, mode = "wb",
  dir = getOption("lingmatch.dict.dir"), overwrite = FALSE)
```

## Arguments

- dict:

  One or more names of dictionaries to download, or `'all'` for all
  available. See [osf.io/y6g5b/wiki](https://osf.io/y6g5b/wiki/home) for
  more information, and a list of available dictionaries.

- check.md5:

  Logical; if `TRUE` (default), retrieves the MD5 checksum from OSF, and
  compares it with that calculated from the downloaded file to check its
  integrity.

- mode:

  A character specifying the file write mode; default is 'wb'. See
  [`download.file`](https://rdrr.io/r/utils/download.file.html).

- dir:

  Directory in which to save the dictionary;  
  default is `getOption('lingmatch.dict.dir')`.  
  This must be specified, or the option must be set – use
  [`lma_initdirs`](https://miserman.github.io/lingmatch/reference/lma_initdirs.md)
  to initialize a directory.

- overwrite:

  Logical; if `TRUE`, will replace existing files.

## Value

Path to the downloaded dictionary, or a list of such if multiple were
downloaded.

## See also

Other Dictionary functions:
[`dictionary_meta()`](https://miserman.github.io/lingmatch/reference/dictionary_meta.md),
[`lma_patcat()`](https://miserman.github.io/lingmatch/reference/lma_patcat.md),
[`lma_termcat()`](https://miserman.github.io/lingmatch/reference/lma_termcat.md),
[`read.dic()`](https://miserman.github.io/lingmatch/reference/read.dic.md),
[`report_term_matches()`](https://miserman.github.io/lingmatch/reference/report_term_matches.md),
[`select.dict()`](https://miserman.github.io/lingmatch/reference/select.dict.md)

## Examples

``` r
if (FALSE) { # \dontrun{

download.dict("lusi", dir = "~/Dictionaries")
} # }
```
