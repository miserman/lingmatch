# Calculate Text-Based Metastatistics

Calculate simple descriptive statistics from text.

## Usage

``` r
lma_meta(text)
```

## Arguments

- text:

  A character vector of texts.

## Value

A data.frame:

- **`characters`**: Total number of characters.

- **`syllables`**: Total number of syllables, as estimated by split
  length of  
  `'a+[eu]*|e+a*|i+|o+[ui]*|u+|y+[aeiou]*'` - 1.

- **`words`**: Total number of words (raw word count).

- **`unique_words`**: Number of unique words (binary word count).

- **`clauses`**: Number of clauses, as marked by commas, colons,
  semicolons, dashes, or brackets within sentences.

- **`sentences`**: Number of sentences, as marked by periods, question
  marks, exclamation points, or new line characters.

- **`words_per_clause`**: Average number of words per clause.

- **`words_per_sentence`**: Average number of words per sentence.

- **`sixltr`**: Number of words 6 or more characters long.

- **`characters_per_word`**: Average number of characters per word
  (`characters` / `words`).

- **`syllables_per_word`**: Average number of syllables per word
  (`syllables` / `words`).

- **`type_token_ratio`**: Ratio of unique to total words: `unique_words`
  / `words`.

- **`reading_grade`**: Flesch-Kincaid grade level: .39 \* `words` /
  `sentences` + 11.8 \* `syllables` / `words` - 15.59.

- **`numbers`**: Number of terms starting with numbers.

- **`punct`**: Number of terms starting with non-alphanumeric
  characters.

- **`periods`**: Number of periods.

- **`commas`**: Number of commas.

- **`qmarks`**: Number of question marks.

- **`exclams`**: Number of exclamation points.

- **`quotes`**: Number of quotation marks (single and double).

- **`apostrophes`**: Number of apostrophes, defined as any modified
  letter apostrophe, or backtick or single straight or curly quote
  surrounded by letters.

- **`brackets`**: Number of bracketing characters (including
  parentheses, and square, curly, and angle brackets).

- **`orgmarks`**: Number of characters used for organization or
  structuring (including dashes, foreword slashes, colons, and
  semicolons).

## Examples

``` r
text <- c(
  succinct = "It is here.",
  verbose = "Hear me now. I shall tell you about it. It is here. Do you hear?",
  couched = "I might be wrong, but it seems to me that it might be here.",
  bigwords = "Object located thither.",
  excited = "It's there! It's there! It's there!",
  drippy = "It's 'there', right? Not 'here'? 'there'? Are you Sure?",
  struggly = "It's here -- in that place where it is. Like... the 1st place (here)."
)
lma_meta(text)
#>          characters syllables words unique_words clauses sentences
#> succinct          8         3     3            3       1         1
#> verbose          46        16    15           12       4         4
#> couched          44        14    14           11       2         1
#> bigwords         20         7     3            3       1         1
#> excited          27         6     6            2       3         3
#> drippy           36         9     9            8       5         4
#> struggly         44        12    12           10       3         2
#>          words_per_clause words_per_sentence sixltr characters_per_word
#> succinct             3.00               3.00      0            2.666667
#> verbose              3.75               3.75      0            3.066667
#> couched              7.00              14.00      0            3.142857
#> bigwords             3.00               3.00      3            6.666667
#> excited              2.00               2.00      0            4.500000
#> drippy               1.80               2.25      0            4.000000
#> struggly             4.00               6.00      0            3.666667
#>          syllables_per_word type_token_ratio reading_grade numbers puncts
#> succinct           1.000000        1.0000000     -2.620000       0      1
#> verbose            1.066667        0.8000000     -1.540833       0      4
#> couched            1.000000        0.7857143      1.670000       0      2
#> bigwords           2.333333        1.0000000     13.113333       0      1
#> excited            1.000000        0.3333333     -3.010000       0      3
#> drippy             1.000000        0.8888889     -2.912500       0     11
#> struggly           1.000000        0.8333333     -1.450000       1      9
#>          periods commas qmarks exclams quotes apostrophes brackets orgmarks
#> succinct       1      0      0       0      0           0        0        0
#> verbose        3      0      1       0      0           0        0        0
#> couched        1      1      0       0      0           0        0        0
#> bigwords       1      0      0       0      0           0        0        0
#> excited        0      0      0       3      0           3        0        0
#> drippy         0      1      4       0      6           1        0        0
#> struggly       5      0      0       0      0           1        2        2
```
