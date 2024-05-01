# lingmatch
An all-in-one R package for the assessment of linguistic matching and/or accommodation.

## features

* Input raw text, a document-term matrix (DTM), or LIWC output.
* Apply various weighting functions to a DTM.
* Measure similarity and/or accommodation with various metrics.
* Calculate standard forms of Language Style Matching (LSM) and Latent Semantic Similarity (LSS).

## resources
* Documentation and guides: [miserman.github.io/lingmatch](https://miserman.github.io/lingmatch/)
  * [Quick Start](https://miserman.github.io/lingmatch/articles/quickstart.html)
  * [Comparison Specification](https://miserman.github.io/lingmatch/articles/groups.html)
  * [Introduction to Text Analysis](https://miserman.github.io/lingmatch/articles/introduction.html)
  * [Word Vectors](https://miserman.github.io/lingmatch/articles/word_vectors.html)
  * [Text Classification](https://miserman.github.io/lingmatch/articles/text_classification.html)
  * [Dictionary Creation](https://miserman.github.io/lingmatch/articles/dictionary_creation.html)
* Dictionary repository: [osf.io/y6g5b](https://osf.io/y6g5b/wiki/home/)
* Latent semantic space repository: [osf.io/489he](https://osf.io/489he/wiki/home/)
* Dictionary builder: [miserman.github.io/dictionary_builder](https://miserman.github.io/dictionary_builder/)

## installation
Download R from [r-project.org](https://www.r-project.org/), then install the package from an R console:

Release ([version 1.0.6](https://CRAN.R-project.org/package=lingmatch))
```R
install.packages("lingmatch")
```
Development (version 1.0.7)
```R
# install.packages("remotes")
remotes::install_github("miserman/lingmatch")
```

And load the package:
```R
library(lingmatch)
```
## examples
Can make a quick comparison between two bits of text; by default this will give the cosine similarity between raw
word-count vectors:
```R
lingmatch("First text to look at.", "Text to compare that text with.")
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
Process the texts in one step:
```R
# with a dictionary
inquirer_cats = lma_process(text, dict = "inquirer", dir = "~/Dictionaries")

# with a latent semantic space
glove_vectors = lma_process(text, space = "glove", dir = "~/Latent Semantic Spaces")
```

Or process the texts step by step, then measure similarity between each:
```R
dtm = lma_dtm(text)
dtm_weighted = lma_weight(dtm)
dtm_categorized = lma_termcat(dtm_weighted, lma_dict(1:9))
similarity = lma_simets(dtm_categorized, metric = "canberra")
```

Or do that within a single function call:
```R
similarity = lingmatch(
  text, weight = "frequency", dict = lma_dict(1:9), metric = "canberra"
)$sim
```

Or, if you want a standard form (as in this example), specify a default:
```R
similarity = lingmatch(text, type = "lsm")$sim
```
