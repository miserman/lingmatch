# Initialize Directories for Dictionaries and Latent Semantic Spaces

Creates directories for dictionaries and latent semantic spaces if
needed, sets them as the  
`lingmatch.dict.dir` and `lingmatch.lspace.dir` options if they are not
already set, and creates links to them in their expected locations
(`'~/Dictionaries'` and `'~/Latent Semantic Spaces'`) by default if
applicable.

## Usage

``` r
lma_initdirs(base = "", dict = "Dictionaries",
  lspace = "Latent Semantic Spaces", link = TRUE)
```

## Arguments

- base:

  Path to a directory in which to create the `dict` and `lspace`
  subdirectories.

- dict:

  Path to the dictionaries directory relative to `base`.

- lspace:

  Path to the latent semantic spaces directory relative to `base`.

- link:

  Logical; if `TRUE` (default), the full `dict` and/or `lspace` paths
  exist (potentially after being created), and they are not
  `'~/Dictionaries'` or `'~/Latent Semantic Spaces'` respectively,
  junctions (Windows) or symbolic links will be created:
  `~/Dictionaries` `<<===>>` `dict` and `~/Latent Semantic Spaces`
  `<<===>>` `lspace`.

## Value

Paths to the \[1\] dictionaries and \[2\] latent semantic space
directories, or a single path if only `dict` or `lspace` is specified.

## Examples

``` r
if (FALSE) { # \dontrun{

# set up the expected dictionary and latent semantic space directories
lma_initdirs("~")

# set up directories elsewhere, and links to the expected locations
lma_initdirs("d:")

# point options and create links to preexisting directories
lma_initdirs("~/NLP_Resources", "Dicts", "Dicts/Embeddings")

# create just a dictionaries directory and set the
# lingmatch.dict.dir option without creating a link
lma_initdirs(dict = "z:/external_dictionaries", link = FALSE)
} # }
```
