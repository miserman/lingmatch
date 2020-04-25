# lingmatch
An all-in-one R package for the assessment of linguistic matching and/or accommodation.

## features

* Input raw text, a document-term matrix (DTM), or LIWC output.
* Apply various weighting functions to a DTM.
* Measure similarity and/or accommodation with various metrics.
* Calculate standard forms of Language Style Matching (LSM) and Latent Semantic Similarity (LSS).

## resources
* Documentation and guides: [miserman.github.io/lingmatch](https://miserman.github.io/lingmatch/)
  * [Introduction](https://miserman.github.io/lingmatch/#introduction)
  * [Comparisons](https://miserman.github.io/lingmatch/#comparisons)
  * [Word vectors](https://miserman.github.io/lingmatch/#word_vectors)
* Latent semantic space repository: [osf.io/489he](https://osf.io/489he/wiki/home/)

## installation
Download R from [r-project.org](https://www.r-project.org/).

```R
install.packages('devtools')
devtools::install_github('miserman/lingmatch')
```
Then load the package:
```R
library(lingmatch)
```
## examples
Can make a quick comparison between two bits of text;
by default this will give the cosine similarity between raw word-count vectors:
```R
lingmatch('First text to look at.', 'Text to compare that text with.')
```

Or, given a vector of texts:
```R
text = c(
  "Why, hello there! How are you this evening?",
  "I am well, thank you for your inquiry!",
  "You are a most good at social interactions person!",
  "Why, thank you! You're not all bad yourself!"
)
```

Process the texts, then measure similarity between each:
```R
dtm = lma_dtm(text)
dtm_weighted = lma_weight(dtm)
dtm_categorized = lma_termcat(dtm_weighted, lma_dict(1:9))
similarity = lma_simets(dtm_categorized, metric = 'canberra')
```

Or do that within a single function call:
```R
similarity = lingmatch(text, 'pairwise',
  weight = 'frequency', dict = lma_dict(1:9), metric = 'canberra'
)
```

Or, if you want a standard form (as in this example), specify a default:
```R
similarity = lingmatch(text, 'pairwise', type = 'lsm', percent = FALSE)
```
