# Select Dictionaries

Retrieve information and links to dictionaries (lexicons/word lists)
available at [osf.io/y6g5b](https://osf.io/y6g5b).

## Usage

``` r
select.dict(query = NULL, dir = getOption("lingmatch.dict.dir"),
  check.md5 = TRUE, mode = "wb")
```

## Arguments

- query:

  A character matching a dictionary name, or a set of keywords to search
  for in dictionary information.

- dir:

  Path to a folder containing dictionaries, or where you want them to be
  saved. Will look in getOption('lingmatch.dict.dir') and
  '~/Dictionaries' by default.

- check.md5:

  Logical; if `TRUE` (default), retrieves the MD5 checksum from OSF, and
  compares it with that calculated from the downloaded file to check its
  integrity.

- mode:

  Passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html) when
  downloading files.

## Value

A list with varying entries:

- **`info`**: The version of [osf.io/kjqb8](https://osf.io/kjqb8) stored
  internally; a `data.frame` with dictionary names as row names, and
  information about each dictionary in columns.  
  Also described at
  [osf.io/y6g5b/wiki/dict_variables](https://osf.io/y6g5b/wiki/dict_variables),
  here `short` (corresponding to the file name \[`{short}.(csv|dic)`\]
  and wiki urls \[`https://osf.io/y6g5b/wiki/{short}`\]) is set as row
  names and removed:

  - **`name`**: Full name of the dictionary.

  - **`description`**: Description of the dictionary, relating to its
    purpose and development.

  - **`note`**: Notes about processing decisions that additionally alter
    the original.

  - **`constructor`**: How the dictionary was constructed:

    - **`algorithm`**: Terms were selected by some automated process,
      potentially learned from data or other resources.

    - **`crowd`**: Several individuals rated the terms, and in aggregate
      those ratings translate to categories and weights.

    - **`mixed`**: Some combination of the other methods, usually in
      some iterative process.

    - **`team`**: One of more individuals make decisions about term
      inclusions, categories, and weights.

  - **`subject`**: Broad, rough subject or purpose of the dictionary:

    - **`emotion`**: Terms relate to emotions, potentially exemplifying
      or expressing them.

    - **`general`**: A large range of categories, aiming to capture the
      content of the text.

    - **`impression`**: Terms are categorized and weighted based on the
      impression they might give.

    - **`language`**: Terms are categorized or weighted based on their
      linguistic features, such as part of speech, specificity, or area
      of use.

    - **`social`**: Terms relate to social phenomena, such as
      characteristics or concerns of social entities.

  - **`terms`**: Number of unique terms across categories.

  - **`term_type`**: Format of the terms:

    - **`glob`**: Include asterisks which denote inclusion of any
      characters until a word boundary.

    - **`glob+`**: Glob-style asterisks with regular expressions within
      terms.

    - **`ngram`**: Includes any number of words as a term, separated by
      spaces.

    - **`pattern`**: A string of characters, potentially within or
      between words, or spanning words.

    - **`regex`**: Regular expressions.

    - **`stem`**: Unigrams with common endings removed.

    - **`unigram`**: Complete single words.

  - **`weighted`**: Indicates whether weights are associated with terms.
    This determines the file type of the dictionary: dictionaries with
    weights are stored as .csv, and those without are stored as .dic
    files.

  - **`regex_characters`**: Logical indicating whether special regular
    expression characters are present in any term, which might need to
    be escaped if the terms are used in regular expressions. Glob-type
    terms allow complete parens (at least one open and one closed,
    indicating preceding or following words), and initial and terminal
    asterisks. For all other terms, `[](){}*.^$+?\|` are counted as
    regex characters. These could be escaped in R with
    `gsub('([][)(}{*.^$+?\\|])', '\\\1', terms)` if `terms` is a
    character vector, and in Python with (importing re)
    `[re.sub(r'([][(){}*.^$+?\|])', r'\\1', term)` `for term in terms]`
    if `terms` is a list.

  - **`categories`**: Category names in the order in which they appear
    in the dictionary file, separated by commas.

  - **`ncategories`**: Number of categories.

  - **`original_max`**: Maximum value of the original dictionary before
    standardization: `original values / max(original values) * 100`.
    Dictionaries with no weights are considered to have a max of `1`.

  - **`osf`**: ID of the file on OSF, translating to the file's URL:
    https://osf.io/`osf`.

  - **`wiki`**: URL of the dictionary's wiki.

  - **`downloaded`**: Path to the file if downloaded, and `''`
    otherwise.

- **`selected`**: A subset of `info` selected by `query`.

## See also

Other Dictionary functions:
[`dictionary_meta()`](https://miserman.github.io/lingmatch/reference/dictionary_meta.md),
[`download.dict()`](https://miserman.github.io/lingmatch/reference/download.dict.md),
[`lma_patcat()`](https://miserman.github.io/lingmatch/reference/lma_patcat.md),
[`lma_termcat()`](https://miserman.github.io/lingmatch/reference/lma_termcat.md),
[`read.dic()`](https://miserman.github.io/lingmatch/reference/read.dic.md),
[`report_term_matches()`](https://miserman.github.io/lingmatch/reference/report_term_matches.md)

## Examples

``` r
# just retrieve information about available dictionaries
dicts <- select.dict()$info
dicts[1:10, 4:9]
#>                  constructor    subject terms term_type weighted
#> adicat_function         team   language   759     glob+    FALSE
#> adict                  mixed     social 12168     ngram    FALSE
#> afinn                   team    emotion  3381   unigram     TRUE
#> agency_communion        team     social   447      glob    FALSE
#> allslang                team   language 10109     ngram    FALSE
#> anew                   crowd impression  1034   unigram     TRUE
#> anew_emotion           crowd    emotion  1034   unigram     TRUE
#> banbuilder              team impression   199   unigram    FALSE
#> banned                  team impression    77     ngram    FALSE
#> cost_benefit            team     social   154      glob    FALSE
#>                  regex_characters
#> adicat_function              TRUE
#> adict                        TRUE
#> afinn                       FALSE
#> agency_communion            FALSE
#> allslang                     TRUE
#> anew                        FALSE
#> anew_emotion                 TRUE
#> banbuilder                  FALSE
#> banned                      FALSE
#> cost_benefit                FALSE

# select all dictionaries mentioning sentiment or emotion
sentiment_dicts <- select.dict("sentiment emotion")$selected
sentiment_dicts[1:10, 4:9]
#>              constructor    subject  terms term_type weighted regex_characters
#> afinn               team    emotion   3381   unigram     TRUE            FALSE
#> anew_emotion       crowd    emotion   1034   unigram     TRUE             TRUE
#> depechemood    algorithm    emotion 114000   unigram     TRUE            FALSE
#> emolex         algorithm    emotion  28480   unigram     TRUE            FALSE
#> emosenticnet   algorithm    emotion  13175     ngram    FALSE             TRUE
#> emote              crowd impression   2197   unigram     TRUE            FALSE
#> galc                team    emotion    274      glob    FALSE            FALSE
#> huliu               team    emotion   6789   unigram    FALSE             TRUE
#> inquirer            team    general   8624   unigram    FALSE             TRUE
#> labmt              crowd    emotion   3934   unigram     TRUE            FALSE
```
