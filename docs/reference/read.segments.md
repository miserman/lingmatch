# Read and Segment Multiple Texts

Split texts by word count or specific characters. Input texts directly,
or read them in from files.

## Usage

``` r
read.segments(path = ".", segment = NULL, ext = ".txt", subdir = FALSE,
  segment.size = -1, bysentence = FALSE, end_in_quotes = TRUE,
  preclean = FALSE, text = NULL)
```

## Arguments

- path:

  Path to a folder containing files, or a vector of paths to files. If
  no folders or files are recognized in `path`, it is treated as `text`.

- segment:

  Specifies how the text of each file should be segmented. If a
  character, split at that character; '\n' by default. If a number,
  texts will be broken into that many segments, each with a roughly
  equal number of words.

- ext:

  The extension of the files you want to read in. '.txt' by default.

- subdir:

  Logical; if `TRUE`, files in folders in `path` will also be included.

- segment.size:

  Logical; if specified, `segment` will be ignored, and texts will be
  broken into segments containing roughly `segment.size` number of
  words.

- bysentence:

  Logical; if `TRUE`, and `segment` is a number or `segment.size` is
  specified, sentences will be kept together, rather than potentially
  being broken across segments.

- end_in_quotes:

  Logical; if `FALSE`, sentence-ending marks (`.?!`) will not be
  considered when immediately followed by a quotation mark. For example,
  `'"Word." Word.'` would be considered one sentence.

- preclean:

  Logical; if `TRUE`, text will be cleaned with `lma_dict(special)`
  before segmentation.

- text:

  A character vector with text to be split, used in place of `path`.
  Each entry is treated as a file.

## Value

A `data.frame` with columns for file names (`input`), segment number
within file (`segment`), word count for each segment (`WC`), and the
text of each segment (`text`).

## Examples

``` r
# split preloaded text
read.segments("split this text into two segments", 2)
#>   input segment WC              text
#> 1     1       1  3   split this text
#> 2     1       2  3 into two segments

if (FALSE) { # \dontrun{

# read in all files from the package directory
texts <- read.segments(path.package("lingmatch"), ext = "")
texts[, -4]

# segment .txt files in dir in a few ways:
dir <- "path/to/files"

## into 1 line segments
texts_lines <- read.segments(dir)

## into 5 even segments each
texts_5segs <- read.segments(dir, 5)

## into 50 word segments
texts_50words <- read.segments(dir, segment.size = 50)

## into 1 sentence segments
texts_1sent <- read.segments(dir, segment.size = 1, bysentence = TRUE)
} # }
```
