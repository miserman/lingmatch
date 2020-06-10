// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// match_terms
List match_terms(const List& tokens, const CharacterVector& terms, const LogicalVector& isword, const IntegerVector& dim, const bool& complete, const bool& tokensonly);
RcppExport SEXP _lingmatch_match_terms(SEXP tokensSEXP, SEXP termsSEXP, SEXP iswordSEXP, SEXP dimSEXP, SEXP completeSEXP, SEXP tokensonlySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type tokens(tokensSEXP);
    Rcpp::traits::input_parameter< const CharacterVector& >::type terms(termsSEXP);
    Rcpp::traits::input_parameter< const LogicalVector& >::type isword(iswordSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const bool& >::type complete(completeSEXP);
    Rcpp::traits::input_parameter< const bool& >::type tokensonly(tokensonlySEXP);
    rcpp_result_gen = Rcpp::wrap(match_terms(tokens, terms, isword, dim, complete, tokensonly));
    return rcpp_result_gen;
END_RCPP
}
// vector_similarity
NumericVector vector_similarity(NumericVector& a, NumericVector& b, const IntegerVector& metrics);
RcppExport SEXP _lingmatch_vector_similarity(SEXP aSEXP, SEXP bSEXP, SEXP metricsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type b(bSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type metrics(metricsSEXP);
    rcpp_result_gen = Rcpp::wrap(vector_similarity(a, b, metrics));
    return rcpp_result_gen;
END_RCPP
}
// calculate_similarities
List calculate_similarities(const S4& m, const RObject& comp, int& type, const IntegerVector& metrics);
RcppExport SEXP _lingmatch_calculate_similarities(SEXP mSEXP, SEXP compSEXP, SEXP typeSEXP, SEXP metricsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const S4& >::type m(mSEXP);
    Rcpp::traits::input_parameter< const RObject& >::type comp(compSEXP);
    Rcpp::traits::input_parameter< int& >::type type(typeSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type metrics(metricsSEXP);
    rcpp_result_gen = Rcpp::wrap(calculate_similarities(m, comp, type, metrics));
    return rcpp_result_gen;
END_RCPP
}
// reformat_embedding
void reformat_embedding(const std::string& infile, const std::string& outfile, const char& sep, const int& digits, const std::string& remove, const std::string& term_check, const bool& verbose);
RcppExport SEXP _lingmatch_reformat_embedding(SEXP infileSEXP, SEXP outfileSEXP, SEXP sepSEXP, SEXP digitsSEXP, SEXP removeSEXP, SEXP term_checkSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type infile(infileSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type outfile(outfileSEXP);
    Rcpp::traits::input_parameter< const char& >::type sep(sepSEXP);
    Rcpp::traits::input_parameter< const int& >::type digits(digitsSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type remove(removeSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type term_check(term_checkSEXP);
    Rcpp::traits::input_parameter< const bool& >::type verbose(verboseSEXP);
    reformat_embedding(infile, outfile, sep, digits, remove, term_check, verbose);
    return R_NilValue;
END_RCPP
}
// extract_indices
NumericVector extract_indices(const IntegerVector& indices, const std::string& file, const char& sep);
RcppExport SEXP _lingmatch_extract_indices(SEXP indicesSEXP, SEXP fileSEXP, SEXP sepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector& >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type file(fileSEXP);
    Rcpp::traits::input_parameter< const char& >::type sep(sepSEXP);
    rcpp_result_gen = Rcpp::wrap(extract_indices(indices, file, sep));
    return rcpp_result_gen;
END_RCPP
}
// extract_matches
NumericVector extract_matches(const CharacterVector& terms, const std::string& file, const char& sep);
RcppExport SEXP _lingmatch_extract_matches(SEXP termsSEXP, SEXP fileSEXP, SEXP sepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector& >::type terms(termsSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type file(fileSEXP);
    Rcpp::traits::input_parameter< const char& >::type sep(sepSEXP);
    rcpp_result_gen = Rcpp::wrap(extract_matches(terms, file, sep));
    return rcpp_result_gen;
END_RCPP
}
// read_segments
List read_segments(const CharacterVector& files, const int& nseg, const int& segsize, const std::string& split, const bool& bysentence, const bool& returntokens);
RcppExport SEXP _lingmatch_read_segments(SEXP filesSEXP, SEXP nsegSEXP, SEXP segsizeSEXP, SEXP splitSEXP, SEXP bysentenceSEXP, SEXP returntokensSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector& >::type files(filesSEXP);
    Rcpp::traits::input_parameter< const int& >::type nseg(nsegSEXP);
    Rcpp::traits::input_parameter< const int& >::type segsize(segsizeSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type split(splitSEXP);
    Rcpp::traits::input_parameter< const bool& >::type bysentence(bysentenceSEXP);
    Rcpp::traits::input_parameter< const bool& >::type returntokens(returntokensSEXP);
    rcpp_result_gen = Rcpp::wrap(read_segments(files, nseg, segsize, split, bysentence, returntokens));
    return rcpp_result_gen;
END_RCPP
}
// pattern_search
List pattern_search(const CharacterVector& texts, const CharacterVector& patterns, const int& ncats, const IntegerVector& categories, const NumericVector& weight, const NumericVector& bias, const bool& fixed, const bool& exclusive);
RcppExport SEXP _lingmatch_pattern_search(SEXP textsSEXP, SEXP patternsSEXP, SEXP ncatsSEXP, SEXP categoriesSEXP, SEXP weightSEXP, SEXP biasSEXP, SEXP fixedSEXP, SEXP exclusiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector& >::type texts(textsSEXP);
    Rcpp::traits::input_parameter< const CharacterVector& >::type patterns(patternsSEXP);
    Rcpp::traits::input_parameter< const int& >::type ncats(ncatsSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type categories(categoriesSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type bias(biasSEXP);
    Rcpp::traits::input_parameter< const bool& >::type fixed(fixedSEXP);
    Rcpp::traits::input_parameter< const bool& >::type exclusive(exclusiveSEXP);
    rcpp_result_gen = Rcpp::wrap(pattern_search(texts, patterns, ncats, categories, weight, bias, fixed, exclusive));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_lingmatch_match_terms", (DL_FUNC) &_lingmatch_match_terms, 6},
    {"_lingmatch_vector_similarity", (DL_FUNC) &_lingmatch_vector_similarity, 3},
    {"_lingmatch_calculate_similarities", (DL_FUNC) &_lingmatch_calculate_similarities, 4},
    {"_lingmatch_reformat_embedding", (DL_FUNC) &_lingmatch_reformat_embedding, 7},
    {"_lingmatch_extract_indices", (DL_FUNC) &_lingmatch_extract_indices, 3},
    {"_lingmatch_extract_matches", (DL_FUNC) &_lingmatch_extract_matches, 3},
    {"_lingmatch_read_segments", (DL_FUNC) &_lingmatch_read_segments, 6},
    {"_lingmatch_pattern_search", (DL_FUNC) &_lingmatch_pattern_search, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_lingmatch(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
