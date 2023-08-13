// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

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
    int r, comp, i, c, bi, bc, l, col;
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
  }
};

// [[Rcpp::export]]
List calculate_similarities(const S4 &m, const RObject &comp, int &type, const IntegerVector &metrics){
  const bool procb = type != 2 && comp.isS4();
  Sparse_Arrays a(m), b = procb ? Sparse_Arrays(as<S4>(comp)) : a;
  const IntegerVector dim{a.dims[0], b.dims[0]};
  size_t nrow = b.dims[0], ai = 1, bi = 0, index = metrics.length();
  const size_t arow = a.dims[0], n = type == 1
    ? a.dims[0] : type == 2 ? arow * (arow - 1) / 2 : arow * nrow;
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
  checkUserInterrupt();
  parallelFor(0, n, sims);
  // formatting output
  if(a.dims[0] == 1) type = 1;
  List op;
  NumericVector sim;
  for(const String met : metric_names){
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
