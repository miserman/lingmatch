#' Calculate Text-Based Metastatistics
#'
#' Calculate simple descriptive statistics from text.
#'
#' @param text A character vector of texts.
#' @return A data.frame:
#'   \itemize{
#'     \item \strong{\code{characters}}: Total number of characters.
#'     \item \strong{\code{syllables}}: Total number of syllables, as estimated by split length of \cr
#'       \code{'a+[eu]*|e+a*|i+|o+[ui]*|u+|y+[aeiou]*'} - 1.
#'     \item \strong{\code{words}}: Total number of words (raw word count).
#'     \item \strong{\code{unique_words}}: Number of unique words (binary word count).
#'     \item \strong{\code{clauses}}: Number of clauses, as marked by commas, colons, semicolons, dashes, or brackets
#'       within sentences.
#'     \item \strong{\code{sentences}}: Number of sentences, as marked by periods, question marks, exclamation points,
#'       or new line characters.
#'     \item \strong{\code{words_per_clause}}: Average number of words per clause.
#'     \item \strong{\code{words_per_sentence}}: Average number of words per sentence.
#'     \item \strong{\code{sixltr}}: Number of words 6 or more characters long.
#'     \item \strong{\code{characters_per_word}}: Average number of characters per word
#'       (\code{characters} / \code{words}).
#'     \item \strong{\code{syllables_per_word}}: Average number of syllables per word
#'       (\code{syllables} / \code{words}).
#'     \item \strong{\code{type_token_ratio}}: Ratio of unique to total words: \code{unique_words} / \code{words}.
#'     \item \strong{\code{reading_grade}}: Flesch-Kincaid grade level: .39 * \code{words} / \code{sentences} +
#'       11.8 * \code{syllables} / \code{words} - 15.59.
#'     \item \strong{\code{numbers}}: Number of terms starting with numbers.
#'     \item \strong{\code{punct}}: Number of terms starting with non-alphanumeric characters.
#'     \item \strong{\code{periods}}: Number of periods.
#'     \item \strong{\code{commas}}: Number of commas.
#'     \item \strong{\code{qmarks}}: Number of question marks.
#'     \item \strong{\code{exclams}}: Number of exclamation points.
#'     \item \strong{\code{quotes}}: Number of quotation marks (single and double).
#'     \item \strong{\code{apostrophes}}: Number of apostrophes, defined as any modified letter apostrophe, or backtick
#'       or single straight or curly quote surrounded by letters.
#'     \item \strong{\code{brackets}}: Number of bracketing characters (including parentheses, and square,
#'       curly, and angle brackets).
#'     \item \strong{\code{orgmarks}}: Number of characters used for organization or structuring (including
#'       dashes, foreword slashes, colons, and semicolons).
#'   }
#' @examples
#' text <- c(
#'   succinct = "It is here.",
#'   verbose = "Hear me now. I shall tell you about it. It is here. Do you hear?",
#'   couched = "I might be wrong, but it seems to me that it might be here.",
#'   bigwords = "Object located thither.",
#'   excited = "It's there! It's there! It's there!",
#'   drippy = "It's 'there', right? Not 'here'? 'there'? Are you Sure?",
#'   struggly = "It's here -- in that place where it is. Like... the 1st place (here)."
#' )
#' lma_meta(text)
#' @export

lma_meta <- function(text) {
  text <- gsub("^\\s+|\\s+$", "", text)
  dtm <- lma_dtm(text, numbers = TRUE, punct = TRUE, urls = FALSE)
  text <- gsub(paste0(
    "((?:^|\\s)[a-z]+\\.[a-z.]+|\\d|(?:^|\\s)[a-z]|(?:^|\\s)[iv]+|",
    "ans|govt|apt|etc|st|rd|ft|feat|dr|drs|mr|ms|mrs|messrs|jr|prof)\\."
  ), "", text, TRUE)
  terms <- colnames(dtm)
  dwm <- dtm[, grepl("^[a-z']", terms), drop = FALSE]
  words <- colnames(dwm)
  word_lengths <- nchar(words)
  word_syllables <- vapply(strsplit(words, "a+[eu]*|e+a*|i+|o+[ui]*|u+|y+[aeiou]*"), length, 0) - 1
  word_syllables[word_syllables == 0] <- 1
  res <- data.frame(
    characters = as.numeric(dwm %*% word_lengths),
    syllables = as.numeric(dwm %*% word_syllables),
    words = rowSums(dwm),
    unique_words = rowSums(dwm != 0),
    clauses = vapply(strsplit(text, '([.?!\n,:;)}>-]|\\])([.?!\n,:;)}>\n\'"-]|\\s|\\])*'), length, 0),
    sentences = vapply(strsplit(text, '[.?!\n]([.?!\n\'"]|\\s)*'), length, 0),
    stringsAsFactors = FALSE
  )
  cbind(res, with(res, data.frame(
    words_per_clause = words / clauses,
    words_per_sentence = words / sentences,
    sixltr = as.numeric(dwm %*% (word_lengths > 5)),
    characters_per_word = characters / words,
    syllables_per_word = syllables / words,
    type_token_ratio = unique_words / words,
    reading_grade = .39 * words / sentences + 11.8 * syllables / words - 15.59,
    numbers = if (any(su <- grepl("^[0-9]", terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    puncts = if (any(su <- grepl("^[^a-z0-9]", terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    periods = if ("." %in% terms) dtm[, "."] else 0,
    commas = if ("," %in% terms) dtm[, ","] else 0,
    qmarks = if ("?" %in% terms) dtm[, "?"] else 0,
    exclams = if ("!" %in% terms) dtm[, "!"] else 0,
    quotes = if (any(su <- grepl('^[\'"]', terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    apostrophes = vapply(strsplit(text, "[\u02bc]+|[a-zA-Z][\u0027\u0060\u2019]+[a-zA-Z]"), length, 0) - 1,
    brackets = if (any(su <- grepl("[(\\)<>{\\}[]|\\]", terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    orgmarks = if (any(su <- grepl("[/:;-]", terms))) rowSums(dtm[, su, drop = FALSE]) else 0,
    row.names = seq_len(nrow(res)), stringsAsFactors = FALSE
  )))
}
