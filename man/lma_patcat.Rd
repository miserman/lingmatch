% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{lma_patcat}
\alias{lma_patcat}
\title{Categorize raw texts using a pattern-based dictionary}
\usage{
lma_patcat(text, dict, term = "term", category = "category",
  weight = "weight", to.lower = TRUE, to.percent = TRUE, bias = NULL,
  intname = "_intercept", dtm = FALSE)
}
\arguments{
\item{text}{A vector of raw text to be categorized.}

\item{dict}{At least a vector of terms (patterns), usually a matrix-like object with columns for terms,
categories, and weights.}

\item{term, category, weight}{Strings specifying the relevant column names in \code{dict}.}

\item{to.lower}{Logical indicating whether \code{text} should be convered to lower case.}

\item{to.percent}{Logical indicating whether term-counts should be devided by document-counts before
being weighted.}

\item{bias}{A constant to add to each category after weighting and summing. Can be a vector with names
corresponding to the unique values in \code{dict[,category]}, but is usually extracted from dict based
on the intercept included in each category (defined by \code{intname}).}

\item{intname}{The term representing the intercept (bias) of a category, to be extracted from \code{dict}
and used as \code{bias}.}

\item{dtm}{Logical; if \code{TRUE}, only a document-term matrix will be returned, rather than the
weighted, summed, and adjusted category value.}
}
\description{
Categorize raw texts using a pattern-based dictionary
}
\examples{
# example text
text = c(
  "Oh what youth was! What I had and gave away. What I took and spent and saw. What I lost. And now? Ruin.",
  "God, are you so bored?! You just want what's gone from us all? I miss the you that was too. I love that you.",
  "Tomorrow! Tomorrow--nay, even tonight--you wait, I am about to change. Soon I will off to revert. Please wait."
)

# read in the temporal orientation lexicon from the World Well-Being Project
tempori = read.csv('https://wwbp.org/downloads/public_data/temporalOrientationLexicon.csv')

lma_patcat(text,tempori)

}