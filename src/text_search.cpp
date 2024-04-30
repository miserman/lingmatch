// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include <fstream>
#define BOOST_REGEX_USE_C_LOCALE
#include <boost/regex.hpp>
using namespace std;
using namespace Rcpp;

/**
 * @brief Count pattern matches within text.
 *
 * @param texts Text to search within.
 * @param patterns Patterns to search for.
 * @param terms Integer vector containing term indices.
 * @param fixed Logical indicating whether patterns are regular expressions or
 * fixed.
 * @param exclusive Logical indicating whether matches should be removed on
 * match (avoiding sub-matches).
 * @return List; a sparse document-term matrix.
 */

// [[Rcpp::export]]
List pattern_search(const CharacterVector &texts,
                    const CharacterVector &patterns, const IntegerVector &terms,
                    const bool &fixed, const bool &exclusive) {
  int cx, r, l, i = 0, n = patterns.length(), tn = texts.length();
  unsigned int p, tp;
  std::string txt;
  IntegerVector dim{tn, (int)patterns.length()}, rows, columns, rowsums(dim[0]);
  NumericVector values;
  if (fixed) {
    for (r = 0; r < tn; r++) {
      checkUserInterrupt();
      txt = texts[r];
      for (i = 0, n = patterns.length(); i < n; i++) {
        l = patterns[i].size(), cx = 0;
        if (l)
          for (std::string::size_type r, p = 0;
               (r = txt.find(patterns[i], p)) != std::string::npos;) {
            if (exclusive) {
              txt.replace(r, l, " ");
              p = r + 1;
            } else
              p = r + l;
            cx += 1;
          }
        if (cx) {
          rows.push_back(r);
          columns.push_back(terms[i]);
          values.push_back(cx);
          rowsums[r] += cx;
        }
      }
    }
  } else {
    vector<boost::regex> pats;
    if (!fixed)
      for (; i < n; i++)
        pats.push_back(
            boost::regex(patterns[i], boost::regex_constants::optimize));
    const unsigned int zero = 0;
    for (r = zero; r < tn; r++) {
      checkUserInterrupt();
      txt = texts[r];
      boost::smatch re;
      for (i = zero, n = pats.size(); i < n; i++) {
        std::string::const_iterator start = txt.cbegin();
        cx = zero;
        if (patterns[i].size())
          for (tp = zero;
               boost::regex_search(start, txt.cend(), re, pats[i]);) {
            p = re.position(zero);
            if (exclusive) {
              txt.replace(tp + p, re.length(), " ");
              start += p + 1;
              tp += p + 1;
            } else
              start += p + re.length();
            cx += 1;
          }
        if (cx) {
          rows.push_back(r);
          columns.push_back(terms[i]);
          values.push_back(cx);
          rowsums[r] += cx;
        }
      }
    }
  }
  S4 dtm("dgTMatrix");
  dtm.slot("Dim") = dim;
  dtm.slot("i") = rows;
  dtm.slot("j") = columns;
  dtm.slot("x") = values;
  return List::create(dtm, rowsums);
}

/**
 * @brief Extract matches to fuzzy terms from text.
 *
 * @param terms Character vector containing regular expression patterns.
 * @param text Text to extract term matches from.
 * @param raw Logical indicating whether `text` is raw text or collapsed terms.
 * @return List; a table of matches and their counts for each term.
 */

// [[Rcpp::export]]
List extract_matches(const CharacterVector &terms,
                     const std::vector<std::string> &text, const bool &raw) {
  const int n = terms.length();
  List res(n);
  string match;
  for (int i = 0, ck = 1e3; i < n; i++) {
    unordered_map<std::string, int> matches;
    const boost::regex re(terms[i]);
    for (const std::string &t : text) {
      if (raw) {
        boost::sregex_iterator search(t.cbegin(), t.cend(), re), search_end;
        for (; search != search_end; search++) {
          match = (*search).str();
          if (matches.find(match) == matches.end()) {
            matches.insert({match, 1});
          } else
            matches[match]++;
        }
      } else {
        if (boost::regex_match(t, re)) {
          if (matches.find(t) == matches.end()) {
            matches.insert({t, 1});
          } else
            matches[t]++;
        }
      }
      if (!--ck) {
        checkUserInterrupt();
        ck = 1e3;
      }
    }
    res[i] = matches;
  }
  return res;
}
