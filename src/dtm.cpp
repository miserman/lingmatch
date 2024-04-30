#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

/**
 * @brief Converts tokenized texts to indices, as part of document-term matrix
 * creation.
 *
 * @param tokens A list of character vectors; tokenized texts.
 * @param terms A character vector containing all terms.
 * @param isword A logical vector indicating whether each term is a word or.
 * punctuation mark (and so should count toward word-count).
 * @param dim Dimensions of the output matrix [n rows, n cols].
 * @param complete Logical indicating whether to all terms tokens should be
 * counted, or only those in `terms` should count.
 * @param tokensonly Logical specifying the return type.
 * @return List; a term map with row indices (if `tokensonly`), or a sparse
 * matrix.
 */

// [[Rcpp::export]]
List match_terms(const List &tokens, const CharacterVector &terms,
                 const LogicalVector &isword, const IntegerVector &dim,
                 const bool &complete, const bool &tokensonly) {
  const int n = tokens.length();
  int i = terms.length(), colindex, s = 0, un, p, ck = 1e3;
  vector<int> rows, columns, rowsums(n), colsums(i);
  vector<double> counts;
  CharacterVector uses;
  unordered_map<String, int> dict;
  for (; i--;) dict.insert({terms[i], i});
  if (tokensonly) {
    for (; s < n; s++) {
      uses = tokens[s];
      for (un = uses.length(), p = 0; p < un; p++)
        if (complete || dict.find(uses[p]) != dict.end()) {
          colindex = dict.at(uses[p]);
          rows.push_back(colindex);
          colsums[colindex]++;
          rowsums[s]++;
        }
      if (!--ck) {
        checkUserInterrupt();
        ck = 1e3;
      }
    }
    return List::create(dict, colsums, rowsums, rows);
  } else {
    for (; s < n; s++) {
      uses = tokens[s];
      for (un = uses.length(), p = 0; p < un; p++)
        if (complete || dict.find(uses[p]) != dict.end()) {
          colindex = dict.at(uses[p]);
          colsums[colindex]++;
          if (isword[colindex]) rowsums[s]++;
          if (p == 0 || uses[p] != uses[p - 1]) {
            rows.push_back(s);
            columns.push_back(colindex);
            counts.push_back(1);
            i++;
          } else
            counts[i]++;
        }
      if (!--ck) {
        checkUserInterrupt();
        ck = 1e3;
      }
    }
    S4 dtm("dgTMatrix");
    dtm.slot("Dim") = dim;
    dtm.slot("Dimnames") = List::create(R_NilValue, terms);
    dtm.slot("i") = rows;
    dtm.slot("j") = columns;
    dtm.slot("x") = counts;
    return List::create(dtm, rowsums, colsums);
  }
}
