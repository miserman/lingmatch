// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <unordered_map>
using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::export]]
List match_terms(const List &tokens, const CharacterVector &terms, const LogicalVector &isword,
  const IntegerVector &dim, const bool &complete){
  const int n = tokens.length();
  int i = terms.length(), colindex, s = 0, un, p;
  vector<int> rows, columns, rowsums(n), colsums(i);
  vector<double> counts;
  CharacterVector uses;
  unordered_map<String, int> dict;
  for(; i--;) dict.insert({terms[i], i});
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
  }
  S4 dtm("dgTMatrix");
  dtm.slot("Dim") = dim;
  dtm.slot("Dimnames") = List::create(R_NilValue, terms);
  dtm.slot("i") = rows;
  dtm.slot("j") = columns;
  dtm.slot("x") = counts;
  return List::create(dtm, rowsums, colsums);
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
  if(metrics[0]) op.push_back(ck ? sum((a != 0) & (b != 0)) / sum((a != 0) | (b != 0)) : NA_REAL, "jaccard");
  if(metrics[1]) op.push_back(ck ? 1 / (1 + sqrt(sum(pow(a - b, 2)))) : 1, "euclidean");
  if(metrics[2]) op.push_back(ck ? 1 - sum(abs(a - b) / (abs(a) + abs(b))) / n : NA_REAL, "canberra");
  if(metrics[3]){
    op.push_back(ck ? sum(a * b) / sqrt(sum(pow(a, 2) * sum(pow(b, 2)))) : NA_REAL, "cosine");
    if(isnan(op["cosine"])) op["cosine"] = NA_REAL;
  }
  if(metrics[4]){
    const double ma = sum(a) / n, mb = sum(b) / n;
    op.push_back(ck ? (sum(a * b) / n - (ma * mb)) / sqrt(sum(pow(a, 2)) / n - pow(ma, 2)) /
      sqrt(sum(pow(b, 2)) / n - pow(mb, 2)) : NA_REAL, "pearson");
    if(isnan(op["pearson"])) op["pearson"] = NA_REAL;
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
      if(c + 1 < ncol && index == p[c + 1]) c++;
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
    int r, comp, i, c, bi, bc, l, col;
    double x, bx, dif, sa, sb, sdif, sadif, sse, sne, cp, asq, bsq, sj, si, ma, mb;
    for(; p < final; p++){
      r = aind[p];
      comp = bind[p];
      i = a.row_starts[r];
      c = a.columns[i];
      bi = b.row_starts[comp];
      bc = b.columns[bi];
      x = bx = sa = sb = sdif = sadif = sse = sne = cp = asq = bsq = sj = si = 0;
      for(l = ncol; l--;){
        col = c;
        if(i == -1 || (bc != -1 && c > bc)) x = 0; else{
          x = a.values[i];
          i = a.row_maps[i];
          c = i == -1 ? -1 : a.columns[i];
          if(NumericVector::is_na(x)) continue;
        }
        if(bi == -1 || (col != -1 && bc > col)) bx = 0; else{
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
        if(x + bx) sne += dif / (abs(x) + abs(bx));
        cp += x * bx;
        asq += pow(x, 2);
        bsq += pow(bx, 2);
        sj += x && bx;
        si += x || bx;
        if(i == -1 && bi == -1) break;
      }
      ma = sa / ncol;
      mb = sb / ncol;
      if(metrics[0] && si) jaccard[p] = sj / si;
      if(metrics[1]) euclidean[p] = 1 / (1 + sqrt(sse));
      if(metrics[2]) canberra[p] = 1 - sne / ncol;
      if(metrics[3] && asq && bsq) cosine[p] = cp / sqrt(asq) / sqrt(bsq);
      if(metrics[4]){
        x = (cp / ncol - ma * mb) / sqrt(asq / ncol - pow(ma, 2)) / sqrt(bsq / ncol - pow(mb, 2));
        pearson[p] = !isnan(x) ? x : NA_REAL;
      }
    }
  }
};

// [[Rcpp::export]]
List calculate_similarities(const S4 &m, const RObject &comp, int &type, const IntegerVector &metrics){
  const bool procb = type != 2 && comp.isS4();
  Sparse_Arrays a(m), b = procb ? Sparse_Arrays(as<S4>(comp)) : a;
  const IntegerVector adim = m.slot("Dim"), bdim = procb ? comp.slot("Dim") : adim, dim{adim[0], bdim[0]};
  int nrow = bdim[0], n = type == 1 ? adim[0] : type == 2 ? adim[0] * (adim[0] - 1) / 2 : adim[0] * nrow,
      index = metrics.length(), ai = 1, bi = 0;
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
      ai = 0, nrow = adim[0], index = 0;
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
  if(adim[0] == 1) type = 1;
  List op;
  NumericVector sim;
  for(const String &m : metric_names){
    sim = res.at(m);
    if(sim.length()){
      if(type == 1){
        op.push_back(sim, m);
      }else{
        S4 simsm(type == 2 ? "dtCMatrix" : "dgCMatrix");
        simsm.slot("Dim") = dim;
        simsm.slot("i") = aind;
        simsm.slot("p") = column_starts;
        simsm.slot("x") = sim;
        if(type == 2){
          simsm.slot("uplo") = "L";
          simsm.slot("diag") = "U";
        }
        op.push_back(simsm, m);
      }
    }
  }
  return op;
}
