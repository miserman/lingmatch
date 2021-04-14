// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <unordered_map>
#include <fstream>
#include <limits>
#include <regex>
#include <cmath>
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
NumericVector vector_similarity(NumericVector &a, NumericVector &b, const IntegerVector &metrics){
  const int n = a.length();
  bool ck = true;
  NumericVector op;
  LogicalVector su = is_na(a) | is_na(b) | ((a == 0) & (b == 0));
  if(is_true(all(su))) ck = false; else if(is_true(any(su))){
    a = a[!su];
    b = b[!su];
  }
  if(metrics[0]){
    op.push_back(ck ? (float)sum((a != 0) & (b != 0)) / sum((a != 0) | (b != 0)) : NA_REAL, "jaccard");
  }
  if(metrics[1]) op.push_back(ck ? 1 / (1 + sqrt(sum(pow(a - b, 2)))) : 1, "euclidean");
  if(metrics[2]) op.push_back(ck ? 1 - sum(abs(a - b) / (abs(a) + abs(b))) / n : NA_REAL, "canberra");
  if(metrics[3]){
    op.push_back(ck ? sum(a * b) / sqrt(sum(pow(a, 2) * sum(pow(b, 2)))) : NA_REAL, "cosine");
    if(isnan((float)op["cosine"])) op["cosine"] = NA_REAL;
  }
  if(metrics[4]){
    const double ma = sum(a) / n, mb = sum(b) / n;
    op.push_back(ck ? (sum(a * b) / n - (ma * mb)) / sqrt(sum(pow(a, 2)) / n - pow(ma, 2)) /
      sqrt(sum(pow(b, 2)) / n - pow(mb, 2)) : NA_REAL, "pearson");
    if(isnan((float)op["pearson"])) op["pearson"] = NA_REAL;
  }
  return op;
}

class Sparse_Arrays{
public:
  const NumericVector values;
  const IntegerVector dims, i, p;
  const int n = 0, ncol = 0;
  IntegerVector row_starts, row_maps, columns;
  Sparse_Arrays(const S4 &m):
    values(m.slot("x")), dims(m.slot("Dim")), i(m.slot("i")), p(m.slot("p")),
    n(values.length()), ncol(dims[1]), row_starts(IntegerVector(dims[0], -1)),
    row_maps(IntegerVector(n, -1)), columns(IntegerVector(n))
  {
    int index = 0, r = 0, c = p[0];
    unordered_map<int, int> row_steps;
    for(; index < n; index++){
      r = i[index];
      if(row_starts[r] == -1){
        row_starts[r] = index;
        row_steps.insert({r, index});
      }else{
        row_maps[row_steps.at(r)] = index;
        row_steps.at(r) = index;
      }
      if(c + 1 < ncol && index == p[c + 1]){
        c++;
        while(c + 1 < ncol && p[c] == p[c + 1]) c++;
      }
      columns[index] = c;
    }
  }
};

struct Compare : public Worker{
  const Sparse_Arrays a, b;
  const int n, ncol, type, nmetrics = 5;
  RVector<int> aind, bind, metrics;
  RVector<double> jaccard, euclidean, canberra, cosine, pearson;
  Compare(
    const Sparse_Arrays &a, const Sparse_Arrays &b, const IntegerVector &i, const IntegerVector &j,
    const int &type, const IntegerVector &metrics, unordered_map<String, NumericVector> &out
  ): a(a), b(b), n(a.n), ncol(a.ncol), type(type), aind(i), bind(j), metrics(metrics),
     jaccard(out.at("jaccard")), euclidean(out.at("euclidean")), canberra(out.at("canberra")),
     cosine(out.at("cosine")), pearson(out.at("pearson")) {}
  void operator()(size_t p, size_t final){
    int r, comp, i, c, bi, bc, l, col, ck = 1e3;
    double x, bx, dif, sa, sb, sdif, sadif, sse, sne, cp, asq, bsq, sj, si, ma, mb;
    for(; p < final; p++){
      r = aind[p];
      comp = bind[p];
      i = a.row_starts[r];
      c = i == -1 ? -1 : a.columns[i];
      bi = b.row_starts[comp];
      bc = bi == -1 ? -1 : b.columns[bi];
      x = bx = sa = sb = sdif = sadif = sse = sne = cp = asq = bsq = sj = si = 0;
      for(l = ncol; l--;){
        col = c;
        if(i == -1 || (bc != -1 && c > bc)){
          x = 0;
          if(i == -1) c = -1;
        }else{
          x = a.values[i];
          i = a.row_maps[i];
          c = i == -1 ? -1 : a.columns[i];
          if(NumericVector::is_na(x)) continue;
        }
        if(bi == -1 || (col != -1 && bc > col)){
          bx = 0;
          if(bi == -1) bc = -1;
        }else{
          bx = b.values[bi];
          bi = b.row_maps[bi];
          bc = bi == -1 ? -1 : b.columns[bi];
          if(NumericVector::is_na(bx) || bx + x == 0) continue;
        }
        sa += x;
        sb += bx;
        dif = x - bx;
        sse += pow(dif, 2);
        dif = abs(dif);
        sdif += dif;
        if(x || bx) sne += dif / (abs(x) + abs(bx));
        cp += x * bx;
        asq += pow(x, 2);
        bsq += pow(bx, 2);
        sj += x && bx;
        si += x || bx;
        if(i == -1 && bi == -1) break;
      }
      ma = sa / ncol;
      mb = sb / ncol;
      if(si){
        if(metrics[0]) jaccard[p] = sj / si;
        if(metrics[2]) canberra[p] = 1 - sne / ncol;
      }
      if(metrics[1]) euclidean[p] = 1 / (1 + sqrt(sse));
      if(sa && sb){
        if(metrics[3] && asq && bsq) cosine[p] = cp / sqrt(asq) / sqrt(bsq);
        if(metrics[4]){
          x = (cp / ncol - ma * mb) / sqrt(asq / ncol - pow(ma, 2)) / sqrt(bsq / ncol - pow(mb, 2));
          pearson[p] = !isnan(x) ? x : NA_REAL;
        }
      }
    }
    if(!--ck){
      checkUserInterrupt();
      ck = 1e3;
    }
  }
};

// [[Rcpp::export]]
List calculate_similarities(const S4 &m, const RObject &comp, int &type, const IntegerVector &metrics){
  const bool procb = type != 2 && comp.isS4();
  Sparse_Arrays a(m), b = procb ? Sparse_Arrays(as<S4>(comp)) : a;
  const IntegerVector dim{a.dims[0], b.dims[0]};
  int nrow = b.dims[0], n = type == 1 ? a.dims[0] : type == 2 ? a.dims[0] * (a.dims[0] - 1) / 2 :
      a.dims[0] * nrow, index = metrics.length(), ai = 1, bi = 0;
  // setting up output vectors
  const CharacterVector metric_names{"jaccard", "euclidean", "canberra", "cosine", "pearson"};
  unordered_map<String, NumericVector> res;
  for(; index--;) res.insert({metric_names[index], NumericVector(metrics[index] ? n : 0)});
  // mapping out row-row comparisons
  IntegerVector aind(n), bind(n), column_starts(nrow + 1);
  column_starts[nrow] = n;
  switch(type){
    case 1: // each a row to a single b row or paired b rows
      aind = seq_len(n) - 1;
      if(nrow == n) bind = aind;
      break;
    case 2: // pairwise between a rows
      for(index = 0; index < n; index++){
        aind[index] = ai;
        bind[index] = bi;
        if(++ai == nrow){
          column_starts[++bi] = index + 1;
          ai = bi + 1;
        }
      }
      break;
    case 3: // each a row with each b row
      ai = 0, nrow = a.dims[0], index = 0;
      for(; index < n; index++){
        aind[index] = ai;
        bind[index] = bi;
        if(++ai == nrow){
          column_starts[++bi] = index + 1;
          ai = 0;
        }
      }
      break;
  }
  // making comparisons
  Compare sims(a, b, aind, bind, type, metrics, res);
  parallelFor(0, n, sims);
  // formatting output
  if(a.dims[0] == 1) type = 1;
  List op;
  NumericVector sim;
  for(const String &met : metric_names){
    sim = res.at(met);
    if(sim.length()){
      if(type == 1){
        op.push_back(sim, met);
      }else{
        S4 simsm(type == 2 ? "dtCMatrix" : "dgCMatrix");
        simsm.slot("Dim") = dim;
        List dimnames(m.slot("Dimnames")), newdimnames{dimnames[0],
          dim[1] == dim[0] ? dimnames[0] : R_NilValue};
        if(procb){
          List dimnames2(comp.slot("Dimnames"));
          newdimnames[1] = dimnames2[0];
        }
        simsm.slot("Dimnames") = newdimnames;
        simsm.slot("i") = aind;
        simsm.slot("p") = column_starts;
        simsm.slot("x") = sim;
        if(type == 2){
          simsm.slot("uplo") = "L";
          simsm.slot("diag") = "U";
        }
        op.push_back(simsm, met);
      }
    }
  }
  return op;
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
  regex ckterm(term_check, regex_constants::optimize), rm(remove, regex_constants::optimize);
  std::string line, term, value;
  for(; getline(d, line);){
    for(cl++, term = "", n = line.length(), i = 0; i < n; i++){
      if(line[i] == sep) break;
      term.push_back(line[i]);
    }
    if(remove != "") term = regex_replace(term, rm, "");
    if(term != "" && n > 100 && i++ < n && (!filter || regex_match(term, ckterm)) &&
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
    vector<regex> pats;
    if(!fixed) for(; i < n; i++) pats.push_back(regex(patterns[i], regex_constants::optimize));
    for(r = 0; r < tn; r++){
      checkUserInterrupt();
      txt = texts[r];
      smatch re;
      for(i = 0, n = pats.size(); i < n; i++){
        std::string::const_iterator start = txt.cbegin();
        cx = 0;
        if(patterns[i].size()) for(tp = 0; regex_search(start, txt.cend(), re, pats[i]);){
          p = re.position(0);
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
