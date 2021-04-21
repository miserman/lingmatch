# version 1.0.2

## Features
* Adds options to specify term and category names in read.dic.
* Adds an option to remove unmatched categories or terms in lma_patcat.

## Improvements
* Improves input-handling of dictionaries.
* Enables term exclusions for token-only lma_dtm.
* Makes output formats more consistent.
* Handles special-character conversion on misencoded text.
* Better handles unrecognized weight and metric names.
* Handles inconsistently named dictionaries.

## Bug Fixes
* Appropriately allows read.dic to read urls.
* Term weights correctly default to count given an empty weight.
* Adds the pois.x argument to lma_weight to allow for separately specified augment alphas
  and d/ppois quantiles or probabilities.
* Corrects handling of empty texts in some cases.
* Corrects lma_process routing and argument passing in some cases.

# version 1.0.1

## Bug Fixes
* Avoids a compilation issue on older macOS platforms.
* Avoids factor-related issues when the stringsAsFactors option is TRUE.
