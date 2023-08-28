// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <fstream>
#define BOOST_REGEX_USE_C_LOCALE
#include <boost/regex.hpp>
using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::export]]
List match_terms(const List &tokens, const CharacterVector &terms, const LogicalVector &isword,
  const IntegerVector &dim, const bool &complete, const bool &tokensonly){
  const int n = tokens.length();
  int i = terms.length(), colindex, s = 0, un, p, ck = 1e3;
  vector<int> rows, columns, rowsums(n), colsums(i);
  vector<double> counts;
  CharacterVector uses;
  unordered_map<String, int> dict;
  for(; i--;) dict.insert({terms[i], i});
  if(tokensonly){
    for(; s < n; s++){
      uses = tokens[s];
      for(un = uses.length(), p = 0; p < un; p++) if(complete || dict.find(uses[p]) != dict.end()){
        colindex = dict.at(uses[p]);
        rows.push_back(colindex);
        colsums[colindex]++;
        rowsums[s]++;
      }
      if(!--ck){
        checkUserInterrupt();
        ck = 1e3;
      }
    }
    return List::create(dict, colsums, rowsums, rows);
  }else{
    for(; s < n; s++){
      uses = tokens[s];
      for(un = uses.length(), p = 0; p < un; p++) if(complete || dict.find(uses[p]) != dict.end()){
        colindex = dict.at(uses[p]);
        colsums[colindex]++;
        if(isword[colindex]) rowsums[s]++;
        if(p == 0 || uses[p] != uses[p - 1]){
          rows.push_back(s);
          columns.push_back(colindex);
          counts.push_back(1);
          i++;
        }else counts[i]++;
      }
      if(!--ck){
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

// [[Rcpp::export]]
void reformat_embedding(const std::string &infile, const std::string &outfile,
  const char &sep = ' ', const int &digits = 9, const std::string &remove = "",
  const std::string &term_check = "^[a-zA-Z]+$|^['a-zA-Z][a-zA-Z.'\\/-]*[a-zA-Z.]$",
  const bool &verbose = false){
  ifstream d(infile);
  ofstream o(outfile + ".dat"), cn(outfile + "_terms.txt");
  o << setprecision(digits) << fixed;
  int n, i, ln, cl = 0, ck = 1e3;
  const string num = "-.0123456789";
  bool start = true, filter = term_check != "";
  boost::regex ckterm(term_check, boost::regex_constants::optimize), rm(remove, boost::regex_constants::optimize);
  std::string line, term, value;
  for(; getline(d, line);){
    for(cl++, term = "", n = line.length(), i = 0; i < n; i++){
      if(line[i] == sep) break;
      term.push_back(line[i]);
    }
    if(remove != "") term = boost::regex_replace(term, rm, "");
    if(term != "" && n > 100 && i++ < n && (!filter || boost::regex_match(term, ckterm)) &&
      num.find(line[i]) != string::npos) try{
      ln = line.length();
      if(ln){
        cn << term << endl;
        for(start = true, value = ""; i < ln; i++){
          if(line[i] == sep){
            if(value != ""){
              if(start){
                o << atof(value.c_str());
                start = false;
              }else o << ' ' << atof(value.c_str());
              value = "";
            }
          }else value.push_back(line[i]);
        }
        value == "" ? o << endl : o << ' ' << atof(value.c_str()) << endl;
      }
    }catch(const std::exception &e){
      Rcout << line << endl;
    }
    if(!--ck){
      checkUserInterrupt();
      if(verbose) Rcout << "line " << cl << ": " << term << endl;
      ck = 1e3;
    }
  }
}

// [[Rcpp::export]]
NumericVector extract_indices(const IntegerVector &indices, const std::string &file, const char &sep = ' '){
  int nr = indices.length(), ck = 1e3, nc = 0, c = 0, t = 0, p = 0, ln, i;
  std::string line, value;
  ifstream d(file);
  for(getline(d, line), t = p = line.length(), value = ""; p--;){
    if(line[p] == sep){
      if(p != t - 1) nc++;
    }else if(!nc) value.insert(value.begin(), line[p]);
  }
  nc == 1 ? nc = atoi(value.c_str()) : nc++;
  IntegerVector dims = {nc, nr};
  NumericVector r(nc * nr);
  if(indices[0] == 1){
    d.seekg(0, d.beg);
    p = 0;
  }else p = 1;
  for(t = 0; t < nr;){
    if(++p == indices[t]){
      getline(d, line);
      for(value = "", ln = line.length(), c = 0, i = 0; i < ln; i++){
        if(line[i] == sep){
          r[t * nc + c++] = atof(value.c_str());
          value = "";
        }else value.push_back(line[i]);
      }
      r[t++ * nc + c] = atof(value.c_str());
    }else d.ignore(numeric_limits<streamsize>::max(), '\n');
    if(!--ck){
      checkUserInterrupt();
      ck = 1e3;
    }
  }
  d.close();
  r.attr("dim") = dims;
  return r;
}

// [[Rcpp::export]]
List pattern_search(const CharacterVector &texts, const CharacterVector &patterns, const IntegerVector &terms,
  const bool &fixed, const bool &exclusive){
  int cx, r, l, i = 0, n = patterns.length(), tn = texts.length();
  unsigned int p, tp;
  std::string txt;
  IntegerVector dim{tn, (int)patterns.length()}, rows, columns, rowsums(dim[0]);
  NumericVector values;
  if(fixed){
    for(r = 0; r < tn; r++){
      checkUserInterrupt();
      txt = texts[r];
      for(i = 0, n = patterns.length(); i < n; i++){
        l = patterns[i].size(), cx = 0;
        if(l) for(std::string::size_type r, p = 0; (r = txt.find(patterns[i], p)) != std::string::npos;){
          if(exclusive){
            txt.replace(r, l, " ");
            p = r + 1;
          }else p = r + l;
          cx += 1;
        }
        if(cx){
          rows.push_back(r);
          columns.push_back(terms[i]);
          values.push_back(cx);
          rowsums[r] += cx;
        }
      }
    }
  }else{
    vector<boost::regex> pats;
    if(!fixed) for(; i < n; i++) pats.push_back(boost::regex(patterns[i], boost::regex_constants::optimize));
    const unsigned int zero = 0;
    for(r = zero; r < tn; r++){
      checkUserInterrupt();
      txt = texts[r];
      boost::smatch re;
      for(i = zero, n = pats.size(); i < n; i++){
        std::string::const_iterator start = txt.cbegin();
        cx = zero;
        if(patterns[i].size()) for(tp = zero; boost::regex_search(start, txt.cend(), re, pats[i]);){
          p = re.position(zero);
          if(exclusive){
            txt.replace(tp + p, re.length(), " ");
            start += p + 1;
            tp += p + 1;
          }else start += p + re.length();
          cx += 1;
        }
        if(cx){
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

// [[Rcpp::export]]
List extract_matches(const CharacterVector &terms, const std::vector<std::string> &text, const bool &raw){
  const int n = terms.length();
  List res(n);
  string match;
  for(int i = 0, ck = 1e3; i < n; i++) {
    unordered_map<std::string, int> matches;
    const boost::regex re(terms[i]);
    for(const std::string &t : text) {
      if (raw) {
        boost::sregex_iterator search(t.cbegin(), t.cend(), re), search_end;
        for (; search != search_end; search++) {
          match = (*search).str();
          if (matches.find(match) == matches.end()) {
            matches.insert({match, 1});
          } else matches[match]++;
        }
      } else {
        if (boost::regex_match(t, re)) {
          if (matches.find(t) == matches.end()) {
            matches.insert({t, 1});
          } else matches[t]++;
        }
      }
      if(!--ck){
        checkUserInterrupt();
        ck = 1e3;
      }
    }
    res[i] = matches;
  }
  return res;
}
