// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include <fstream>
#define BOOST_REGEX_USE_C_LOCALE
#include <boost/regex.hpp>
using namespace std;
using namespace Rcpp;

/**
 * @brief Standardizes embeddings.
 *
 * @param infile Path to original embeddings file.
 * @param outfile Base path to reformatted files, which will translate to a
 * `.dat` and `_terms.txt` file.
 * @param sep Separating character(s).
 * @param digits Number of digits to round to.
 * @param remove String to remove to any terms.
 * @param term_check Regex pattern used to check terms for inclusion.
 * @param verbose Logical indicating whither to print every thousandth line
 * during reformatting.
 * @return void; writes output files.
 */

// [[Rcpp::export]]
void reformat_embedding(const std::string &infile, const std::string &outfile,
                        const char &sep = ' ', const int &digits = 9,
                        const std::string &remove = "",
                        const std::string &term_check =
                            "^[a-zA-Z]+$|^['a-zA-Z][a-zA-Z.'\\/-]*[a-zA-Z.]$",
                        const bool &verbose = false) {
  ifstream d(infile);
  ofstream o(outfile + ".dat"), cn(outfile + "_terms.txt");
  o << setprecision(digits) << fixed;
  int n, i, ln, cl = 0, ck = 1e3;
  const string num = "-.0123456789";
  bool start = true, filter = term_check != "";
  boost::regex ckterm(term_check, boost::regex_constants::optimize),
      rm(remove, boost::regex_constants::optimize);
  std::string line, term, value;
  for (; getline(d, line);) {
    for (cl++, term = "", n = line.length(), i = 0; i < n; i++) {
      if (line[i] == sep) break;
      term.push_back(line[i]);
    }
    if (remove != "") term = boost::regex_replace(term, rm, "");
    if (term != "" && n > 100 && i++ < n &&
        (!filter || boost::regex_match(term, ckterm)) &&
        num.find(line[i]) != string::npos)
      try {
        ln = line.length();
        if (ln) {
          cn << term << endl;
          for (start = true, value = ""; i < ln; i++) {
            if (line[i] == sep) {
              if (value != "") {
                if (start) {
                  o << atof(value.c_str());
                  start = false;
                } else
                  o << ' ' << atof(value.c_str());
                value = "";
              }
            } else
              value.push_back(line[i]);
          }
          value == "" ? o << endl : o << ' ' << atof(value.c_str()) << endl;
        }
      } catch (const std::exception &e) {
        Rcout << line << endl;
      }
    if (!--ck) {
      checkUserInterrupt();
      if (verbose) Rcout << "line " << cl << ": " << term << endl;
      ck = 1e3;
    }
  }
}

/**
 * @brief Read embeddings vectors based on term indices.
 *
 * @param indices Integer vector containing the indices of the vectors to load.
 * @param file File containing vectors.
 * @param sep Separator between values.
 * @return NumericVector; term by dimension matrix.
 */

// [[Rcpp::export]]
NumericVector extract_indices(const IntegerVector &indices,
                              const std::string &file, const char &sep = ' ') {
  int nr = indices.length(), ck = 1e3, nc = 0, c = 0, t = 0, p = 0, ln, i;
  std::string line, value;
  ifstream d(file);
  for (getline(d, line), t = p = line.length(), value = ""; p--;) {
    if (line[p] == sep) {
      if (p != t - 1) nc++;
    } else if (!nc)
      value.insert(value.begin(), line[p]);
  }
  nc == 1 ? nc = atoi(value.c_str()) : nc++;
  IntegerVector dims = {nc, nr};
  NumericVector r(nc * nr);
  if (indices[0] == 1) {
    d.seekg(0, d.beg);
    p = 0;
  } else
    p = 1;
  for (t = 0; t < nr;) {
    if (++p == indices[t]) {
      getline(d, line);
      for (value = "", ln = line.length(), c = 0, i = 0; i < ln; i++) {
        if (line[i] == sep) {
          r[t * nc + c++] = atof(value.c_str());
          value = "";
        } else
          value.push_back(line[i]);
      }
      r[t++ * nc + c] = atof(value.c_str());
    } else
      d.ignore(numeric_limits<streamsize>::max(), '\n');
    if (!--ck) {
      checkUserInterrupt();
      ck = 1e3;
    }
  }
  d.close();
  r.attr("dim") = dims;
  return r;
}
