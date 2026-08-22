# Standardize a Latent Semantic Space

Reformat a .rda file which has a matrix with terms as row names, or a
plain-text embeddings file which has a term at the start of each line,
and consistent delimiting characters. Plain-text files are processed
line-by-line, so large spaces can be reformatted RAM-conservatively.

## Usage

``` r
standardize.lspace(infile, name, sep = " ", digits = 9,
  dir = getOption("lingmatch.lspace.dir"), outdir = dir, remove = "",
  term_check = "^[a-zA-Z]+$|^['a-zA-Z][a-zA-Z.'\\/-]*[a-zA-Z.]$",
  verbose = FALSE)
```

## Arguments

- infile:

  Name of the .rda or plain-text file relative to `dir`,  
  e.g., "default.rda" or "glove/glove.6B.300d.txt".

- name:

  Base name of the reformatted file and term file; e.g., "glove" would
  result in `glove.dat` and `glove_terms.txt` in `outdir`.

- sep:

  Delimiting character between values in each line, e.g., `" "` or
  `"\t"`. Only applies to plain-text files.

- digits:

  Number of digits to round values to; default is 9.

- dir:

  Path to folder containing `infile`s.  
  Default is `getOption('lingmatch.lspace.dir')`, which must be set in
  the current session. If this is not specified and `infile` is a full
  path, `dir` will be set to `infile`'s parent directory.

- outdir:

  Path to folder in which to save standardized files; default is `dir`.

- remove:

  A string with a regex pattern to be removed from term names  
  (i.e., `gsub(remove,` `"", term)`); default is `""`, which is ignored.

- term_check:

  A string with a regex pattern by which to filter terms; i.e., only
  lines with fully matched terms are written to the reformatted file.
  The default attempts to retain only regular words, including those
  with dashes, foreword slashes, and periods. Set to an empty string
  (`""`) to write all lines regardless of term.

- verbose:

  Logical: if `TRUE`, prints the current line number and its term to the
  console every 1,000 lines. Only applies to plain-text files.

## Value

Path to the standardized \[1\] data file and \[2\] terms file if
applicable.

## See also

Other Latent Semantic Space functions:
[`download.lspace()`](https://miserman.github.io/lingmatch/reference/download.lspace.md),
[`lma_lspace()`](https://miserman.github.io/lingmatch/reference/lma_lspace.md),
[`select.lspace()`](https://miserman.github.io/lingmatch/reference/select.lspace.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# from https://sites.google.com/site/fritzgntr/software-resources/semantic_spaces
standardize.lspace("EN_100k_lsa.rda", "100k_lsa")

# from https://fasttext.cc/docs/en/english-vectors.html
standardize.lspace("crawl-300d-2M.vec", "facebook_crawl")

# Standardized versions of these spaces can also be downloaded with download.lspace.
} # }
```
