# version 1.0.8

### Improvements
* Improves input resolution.

# version 1.0.7

### Features
* Adds `dictionary_meta` to assess dictionary categories.
* Adds dictionary category coverage reporting to `lma_termcat` and `lma_process`.

### Bug Fixes
* Correctly applies category selection in `read.dic` in more cases.

### Improvements
* Removes option to download embeddings without terms files; works toward
  independence between spaces and term map.
* Allows for direct weight discretization from a weighted list dictionary.

# version 1.0.6

### Features
* Adds conversion of list-representation to sparse matrix to `lma_dtm`.

### Improvements
* Adds overwrite options to download functions.
* `lingmatch` more flexibly handles pairwise comparisons within groups.
* `lma_dtm` applies row names where possible.

### Bug Fixes
* Corrects handling of multiple groups given `all.levels` in some cases.
* Adjusts `lma_simets` pairwise mean for self match.

# version 1.0.5

### Features
* Adds `report_term_match` to assess fuzzy terms in dictionaries.

### Bug Fixes
* Avoids integer overflow when setting up large comparisons.
* Fixes `select.lspace` downloaded indicator.
* Catches more invalid regular expressions to avoid crashes from `pattern_search`.

# version 1.0.4

### Improvements
* Avoids checking if long texts are files.
* Improves feedback when a directory has not been specified.
* Changes results of singular pairwise comparisons from `1` to `NA`.

# version 1.0.3

### Bug Fixes
* Fixes `lma_simets` vector to matrix comparisons in some cases.
* Fixes `lma_dtm` `tokens.only` when one text ends up empty.

# version 1.0.2

### Features
* Adds options to specify term and category names in `read.dic`.
* Adds an option to remove unmatched categories or terms in `lma_patcat`.

### Improvements
* Improves input-handling of dictionaries.
* Enables term exclusions for token-only `lma_dtm`.
* Makes output formats more consistent.
* Handles special-character conversion on misencoded text.
* Better handles unrecognized weight and metric names.
* Handles inconsistently named dictionaries.

### Bug Fixes
* Fixes `lma_dtm` token to dtm conversion when final token indices entries are empty.
* Appropriately allows `read.dic` to read urls.
* Term weights correctly default to count given an empty weight.
* Adds the `pois.x` argument to `lma_weight` to allow for separately specified augment alphas
  and d/ppois quantiles or probabilities.
* Corrects handling of empty texts in some cases.
* Corrects `lma_process` routing and argument passing in some cases.

# version 1.0.1

### Bug Fixes
* Avoids a compilation issue on older macOS platforms.
* Avoids factor-related issues when the `stringsAsFactors` option is `TRUE`.
