// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// BGLPointMass
Rcpp::List BGLPointMass(arma::mat xx, arma::vec y, arma::mat W, unsigned int s, unsigned int q, int maxSteps, arma::vec hatAlpha, arma::vec hatBeta, arma::vec hatInvTauSqStar, arma::mat invSigAlpha0, double hatPiStar, double hatLambdaSqStar, double hatSigmaSq, double aStar, double bStar, double alpha, double gamma, double sh1, double sh0, int progress);
RcppExport SEXP _roben_BGLPointMass(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP sSEXP, SEXP qSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBetaSEXP, SEXP hatInvTauSqStarSEXP, SEXP invSigAlpha0SEXP, SEXP hatPiStarSEXP, SEXP hatLambdaSqStarSEXP, SEXP hatSigmaSqSEXP, SEXP aStarSEXP, SEXP bStarSEXP, SEXP alphaSEXP, SEXP gammaSEXP, SEXP sh1SEXP, SEXP sh0SEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type s(sSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatInvTauSqStar(hatInvTauSqStarSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatPiStar(hatPiStarSEXP);
    Rcpp::traits::input_parameter< double >::type hatLambdaSqStar(hatLambdaSqStarSEXP);
    Rcpp::traits::input_parameter< double >::type hatSigmaSq(hatSigmaSqSEXP);
    Rcpp::traits::input_parameter< double >::type aStar(aStarSEXP);
    Rcpp::traits::input_parameter< double >::type bStar(bStarSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< double >::type sh1(sh1SEXP);
    Rcpp::traits::input_parameter< double >::type sh0(sh0SEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BGLPointMass(xx, y, W, s, q, maxSteps, hatAlpha, hatBeta, hatInvTauSqStar, invSigAlpha0, hatPiStar, hatLambdaSqStar, hatSigmaSq, aStar, bStar, alpha, gamma, sh1, sh0, progress));
    return rcpp_result_gen;
END_RCPP
}
// BGL
Rcpp::List BGL(arma::mat xx, arma::vec y, arma::mat W, unsigned int s, unsigned int q, int maxSteps, arma::vec hatBeta, arma::vec hatAlpha, arma::vec hatInvTauSq, arma::mat invSigAlpha0, double hatLambdaSqStar, double hatSigmaSq, double aStar, double bStar, double alpha, double gamma, int progress);
RcppExport SEXP _roben_BGL(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP sSEXP, SEXP qSEXP, SEXP maxStepsSEXP, SEXP hatBetaSEXP, SEXP hatAlphaSEXP, SEXP hatInvTauSqSEXP, SEXP invSigAlpha0SEXP, SEXP hatLambdaSqStarSEXP, SEXP hatSigmaSqSEXP, SEXP aStarSEXP, SEXP bStarSEXP, SEXP alphaSEXP, SEXP gammaSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type s(sSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatInvTauSq(hatInvTauSqSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatLambdaSqStar(hatLambdaSqStarSEXP);
    Rcpp::traits::input_parameter< double >::type hatSigmaSq(hatSigmaSqSEXP);
    Rcpp::traits::input_parameter< double >::type aStar(aStarSEXP);
    Rcpp::traits::input_parameter< double >::type bStar(bStarSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BGL(xx, y, W, s, q, maxSteps, hatBeta, hatAlpha, hatInvTauSq, invSigAlpha0, hatLambdaSqStar, hatSigmaSq, aStar, bStar, alpha, gamma, progress));
    return rcpp_result_gen;
END_RCPP
}
// BL_SS
Rcpp::List BL_SS(arma::mat xx, arma::vec y, arma::mat W, int maxSteps, arma::vec hatAlpha, arma::vec hatBeta, arma::vec hatInvTauSq, arma::mat invSigAlpha0, double hatPi, double hatLambdaSq, double hatSigmaSq, double aStar, double bStar, double alpha, double gamma, double sh1, double sh0, int progress);
RcppExport SEXP _roben_BL_SS(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBetaSEXP, SEXP hatInvTauSqSEXP, SEXP invSigAlpha0SEXP, SEXP hatPiSEXP, SEXP hatLambdaSqSEXP, SEXP hatSigmaSqSEXP, SEXP aStarSEXP, SEXP bStarSEXP, SEXP alphaSEXP, SEXP gammaSEXP, SEXP sh1SEXP, SEXP sh0SEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatInvTauSq(hatInvTauSqSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatPi(hatPiSEXP);
    Rcpp::traits::input_parameter< double >::type hatLambdaSq(hatLambdaSqSEXP);
    Rcpp::traits::input_parameter< double >::type hatSigmaSq(hatSigmaSqSEXP);
    Rcpp::traits::input_parameter< double >::type aStar(aStarSEXP);
    Rcpp::traits::input_parameter< double >::type bStar(bStarSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< double >::type sh1(sh1SEXP);
    Rcpp::traits::input_parameter< double >::type sh0(sh0SEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BL_SS(xx, y, W, maxSteps, hatAlpha, hatBeta, hatInvTauSq, invSigAlpha0, hatPi, hatLambdaSq, hatSigmaSq, aStar, bStar, alpha, gamma, sh1, sh0, progress));
    return rcpp_result_gen;
END_RCPP
}
// BLasso
Rcpp::List BLasso(arma::mat xx, arma::vec y, arma::mat W, int maxSteps, arma::vec hatBeta, arma::vec hatAlpha, arma::vec hatInvTauSq, arma::mat invSigAlpha0, double hatLambdaSqStar, double hatSigmaSq, double aStar, double bStar, double alpha, double gamma, int progress);
RcppExport SEXP _roben_BLasso(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP maxStepsSEXP, SEXP hatBetaSEXP, SEXP hatAlphaSEXP, SEXP hatInvTauSqSEXP, SEXP invSigAlpha0SEXP, SEXP hatLambdaSqStarSEXP, SEXP hatSigmaSqSEXP, SEXP aStarSEXP, SEXP bStarSEXP, SEXP alphaSEXP, SEXP gammaSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatInvTauSq(hatInvTauSqSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatLambdaSqStar(hatLambdaSqStarSEXP);
    Rcpp::traits::input_parameter< double >::type hatSigmaSq(hatSigmaSqSEXP);
    Rcpp::traits::input_parameter< double >::type aStar(aStarSEXP);
    Rcpp::traits::input_parameter< double >::type bStar(bStarSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BLasso(xx, y, W, maxSteps, hatBeta, hatAlpha, hatInvTauSq, invSigAlpha0, hatLambdaSqStar, hatSigmaSq, aStar, bStar, alpha, gamma, progress));
    return rcpp_result_gen;
END_RCPP
}
// BRGL_SS
Rcpp::List BRGL_SS(arma::mat xx, arma::vec y, arma::mat W, unsigned int s, unsigned int L, int maxSteps, arma::vec hatAlpha, arma::mat hatBeta, double hatTau, arma::vec hatV, arma::vec hatSg, arma::mat invSigAlpha0, double hatPi, double hatEtaSq, double xi1, double xi2, double r, double a, double b, double sh1, double sh0, int progress);
RcppExport SEXP _roben_BRGL_SS(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP sSEXP, SEXP LSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBetaSEXP, SEXP hatTauSEXP, SEXP hatVSEXP, SEXP hatSgSEXP, SEXP invSigAlpha0SEXP, SEXP hatPiSEXP, SEXP hatEtaSqSEXP, SEXP xi1SEXP, SEXP xi2SEXP, SEXP rSEXP, SEXP aSEXP, SEXP bSEXP, SEXP sh1SEXP, SEXP sh0SEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type s(sSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< double >::type hatTau(hatTauSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatV(hatVSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatSg(hatSgSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatPi(hatPiSEXP);
    Rcpp::traits::input_parameter< double >::type hatEtaSq(hatEtaSqSEXP);
    Rcpp::traits::input_parameter< double >::type xi1(xi1SEXP);
    Rcpp::traits::input_parameter< double >::type xi2(xi2SEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type sh1(sh1SEXP);
    Rcpp::traits::input_parameter< double >::type sh0(sh0SEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BRGL_SS(xx, y, W, s, L, maxSteps, hatAlpha, hatBeta, hatTau, hatV, hatSg, invSigAlpha0, hatPi, hatEtaSq, xi1, xi2, r, a, b, sh1, sh0, progress));
    return rcpp_result_gen;
END_RCPP
}
// BRGL
Rcpp::List BRGL(arma::mat xx, arma::vec y, arma::mat W, unsigned int s, unsigned int L, int maxSteps, arma::vec hatAlpha, arma::mat hatBeta, double hatTau, arma::vec hatV, arma::vec hatSg, arma::mat invSigAlpha0, double hatEtaSq, double xi1, double xi2, double r, double a, double b, int progress);
RcppExport SEXP _roben_BRGL(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP sSEXP, SEXP LSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBetaSEXP, SEXP hatTauSEXP, SEXP hatVSEXP, SEXP hatSgSEXP, SEXP invSigAlpha0SEXP, SEXP hatEtaSqSEXP, SEXP xi1SEXP, SEXP xi2SEXP, SEXP rSEXP, SEXP aSEXP, SEXP bSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type s(sSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< double >::type hatTau(hatTauSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatV(hatVSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatSg(hatSgSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatEtaSq(hatEtaSqSEXP);
    Rcpp::traits::input_parameter< double >::type xi1(xi1SEXP);
    Rcpp::traits::input_parameter< double >::type xi2(xi2SEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BRGL(xx, y, W, s, L, maxSteps, hatAlpha, hatBeta, hatTau, hatV, hatSg, invSigAlpha0, hatEtaSq, xi1, xi2, r, a, b, progress));
    return rcpp_result_gen;
END_RCPP
}
// BRL_SS
Rcpp::List BRL_SS(arma::mat xx, arma::vec y, arma::mat W, int maxSteps, arma::vec hatAlpha, arma::vec hatBeta, double hatTau, arma::vec hatV, arma::vec hatSg, arma::mat invSigAlpha0, double hatPi, double hatEtaSq, double xi1, double xi2, double r1, double a, double b, double sh1, double sh0, int progress);
RcppExport SEXP _roben_BRL_SS(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBetaSEXP, SEXP hatTauSEXP, SEXP hatVSEXP, SEXP hatSgSEXP, SEXP invSigAlpha0SEXP, SEXP hatPiSEXP, SEXP hatEtaSqSEXP, SEXP xi1SEXP, SEXP xi2SEXP, SEXP r1SEXP, SEXP aSEXP, SEXP bSEXP, SEXP sh1SEXP, SEXP sh0SEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< double >::type hatTau(hatTauSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatV(hatVSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatSg(hatSgSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatPi(hatPiSEXP);
    Rcpp::traits::input_parameter< double >::type hatEtaSq(hatEtaSqSEXP);
    Rcpp::traits::input_parameter< double >::type xi1(xi1SEXP);
    Rcpp::traits::input_parameter< double >::type xi2(xi2SEXP);
    Rcpp::traits::input_parameter< double >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type sh1(sh1SEXP);
    Rcpp::traits::input_parameter< double >::type sh0(sh0SEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BRL_SS(xx, y, W, maxSteps, hatAlpha, hatBeta, hatTau, hatV, hatSg, invSigAlpha0, hatPi, hatEtaSq, xi1, xi2, r1, a, b, sh1, sh0, progress));
    return rcpp_result_gen;
END_RCPP
}
// BRL
Rcpp::List BRL(arma::mat xx, arma::vec y, arma::mat W, int maxSteps, arma::vec hatAlpha, arma::vec hatBeta, double hatTau, arma::vec hatV, arma::vec hatSg, arma::mat invSigAlpha0, double hatEtaSq, double xi1, double xi2, double r1, double a, double b, int progress);
RcppExport SEXP _roben_BRL(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBetaSEXP, SEXP hatTauSEXP, SEXP hatVSEXP, SEXP hatSgSEXP, SEXP invSigAlpha0SEXP, SEXP hatEtaSqSEXP, SEXP xi1SEXP, SEXP xi2SEXP, SEXP r1SEXP, SEXP aSEXP, SEXP bSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< double >::type hatTau(hatTauSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatV(hatVSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatSg(hatSgSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatEtaSq(hatEtaSqSEXP);
    Rcpp::traits::input_parameter< double >::type xi1(xi1SEXP);
    Rcpp::traits::input_parameter< double >::type xi2(xi2SEXP);
    Rcpp::traits::input_parameter< double >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BRL(xx, y, W, maxSteps, hatAlpha, hatBeta, hatTau, hatV, hatSg, invSigAlpha0, hatEtaSq, xi1, xi2, r1, a, b, progress));
    return rcpp_result_gen;
END_RCPP
}
// BRSGL_SS
Rcpp::List BRSGL_SS(arma::mat xx, arma::vec y, arma::mat W, unsigned int s, unsigned int L, int maxSteps, arma::vec hatAlpha, arma::mat hatBg, double hatTau, arma::vec hatV, arma::mat hatGamma, arma::mat invSigAlpha0, double hatSsq, double hatPi0, double hatPi1, double xi1, double xi2, double hatT, double a, double b, double sh0_1, double sh0_0, double sh1_1, double sh1_0, double cutoff, int progress);
RcppExport SEXP _roben_BRSGL_SS(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP sSEXP, SEXP LSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBgSEXP, SEXP hatTauSEXP, SEXP hatVSEXP, SEXP hatGammaSEXP, SEXP invSigAlpha0SEXP, SEXP hatSsqSEXP, SEXP hatPi0SEXP, SEXP hatPi1SEXP, SEXP xi1SEXP, SEXP xi2SEXP, SEXP hatTSEXP, SEXP aSEXP, SEXP bSEXP, SEXP sh0_1SEXP, SEXP sh0_0SEXP, SEXP sh1_1SEXP, SEXP sh1_0SEXP, SEXP cutoffSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type s(sSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatBg(hatBgSEXP);
    Rcpp::traits::input_parameter< double >::type hatTau(hatTauSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatV(hatVSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatGamma(hatGammaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatSsq(hatSsqSEXP);
    Rcpp::traits::input_parameter< double >::type hatPi0(hatPi0SEXP);
    Rcpp::traits::input_parameter< double >::type hatPi1(hatPi1SEXP);
    Rcpp::traits::input_parameter< double >::type xi1(xi1SEXP);
    Rcpp::traits::input_parameter< double >::type xi2(xi2SEXP);
    Rcpp::traits::input_parameter< double >::type hatT(hatTSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type sh0_1(sh0_1SEXP);
    Rcpp::traits::input_parameter< double >::type sh0_0(sh0_0SEXP);
    Rcpp::traits::input_parameter< double >::type sh1_1(sh1_1SEXP);
    Rcpp::traits::input_parameter< double >::type sh1_0(sh1_0SEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BRSGL_SS(xx, y, W, s, L, maxSteps, hatAlpha, hatBg, hatTau, hatV, hatGamma, invSigAlpha0, hatSsq, hatPi0, hatPi1, xi1, xi2, hatT, a, b, sh0_1, sh0_0, sh1_1, sh1_0, cutoff, progress));
    return rcpp_result_gen;
END_RCPP
}
// BRSGL
Rcpp::List BRSGL(arma::mat xx, arma::vec y, arma::mat W, unsigned int s, unsigned int L, int maxSteps, arma::vec hatAlpha, arma::mat hatBeta, double hatTau, arma::vec hatV, arma::vec hatSg, arma::mat hatGamma, arma::mat invSigAlpha0, double hatEta1Sq, double hatEta2Sq, double xi1, double xi2, double s1, double s2, double r1, double r2, double a, double b, int progress);
RcppExport SEXP _roben_BRSGL(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP sSEXP, SEXP LSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBetaSEXP, SEXP hatTauSEXP, SEXP hatVSEXP, SEXP hatSgSEXP, SEXP hatGammaSEXP, SEXP invSigAlpha0SEXP, SEXP hatEta1SqSEXP, SEXP hatEta2SqSEXP, SEXP xi1SEXP, SEXP xi2SEXP, SEXP s1SEXP, SEXP s2SEXP, SEXP r1SEXP, SEXP r2SEXP, SEXP aSEXP, SEXP bSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type s(sSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< double >::type hatTau(hatTauSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatV(hatVSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatSg(hatSgSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatGamma(hatGammaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatEta1Sq(hatEta1SqSEXP);
    Rcpp::traits::input_parameter< double >::type hatEta2Sq(hatEta2SqSEXP);
    Rcpp::traits::input_parameter< double >::type xi1(xi1SEXP);
    Rcpp::traits::input_parameter< double >::type xi2(xi2SEXP);
    Rcpp::traits::input_parameter< double >::type s1(s1SEXP);
    Rcpp::traits::input_parameter< double >::type s2(s2SEXP);
    Rcpp::traits::input_parameter< double >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< double >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BRSGL(xx, y, W, s, L, maxSteps, hatAlpha, hatBeta, hatTau, hatV, hatSg, hatGamma, invSigAlpha0, hatEta1Sq, hatEta2Sq, xi1, xi2, s1, s2, r1, r2, a, b, progress));
    return rcpp_result_gen;
END_RCPP
}
// BSGL_SS
Rcpp::List BSGL_SS(arma::mat xx, arma::vec y, arma::mat W, unsigned int s, unsigned int L, int maxSteps, arma::vec hatAlpha, arma::mat hatBg, double hatSigmaSq, arma::mat hatGamma, arma::mat invSigAlpha0, double hatSsq, double hatPi0, double hatPi1, double hatT, double sh0_1, double sh0_0, double sh1_1, double sh1_0, double c, double d, double cutoff, int progress);
RcppExport SEXP _roben_BSGL_SS(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP sSEXP, SEXP LSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBgSEXP, SEXP hatSigmaSqSEXP, SEXP hatGammaSEXP, SEXP invSigAlpha0SEXP, SEXP hatSsqSEXP, SEXP hatPi0SEXP, SEXP hatPi1SEXP, SEXP hatTSEXP, SEXP sh0_1SEXP, SEXP sh0_0SEXP, SEXP sh1_1SEXP, SEXP sh1_0SEXP, SEXP cSEXP, SEXP dSEXP, SEXP cutoffSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type s(sSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatBg(hatBgSEXP);
    Rcpp::traits::input_parameter< double >::type hatSigmaSq(hatSigmaSqSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatGamma(hatGammaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatSsq(hatSsqSEXP);
    Rcpp::traits::input_parameter< double >::type hatPi0(hatPi0SEXP);
    Rcpp::traits::input_parameter< double >::type hatPi1(hatPi1SEXP);
    Rcpp::traits::input_parameter< double >::type hatT(hatTSEXP);
    Rcpp::traits::input_parameter< double >::type sh0_1(sh0_1SEXP);
    Rcpp::traits::input_parameter< double >::type sh0_0(sh0_0SEXP);
    Rcpp::traits::input_parameter< double >::type sh1_1(sh1_1SEXP);
    Rcpp::traits::input_parameter< double >::type sh1_0(sh1_0SEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type d(dSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BSGL_SS(xx, y, W, s, L, maxSteps, hatAlpha, hatBg, hatSigmaSq, hatGamma, invSigAlpha0, hatSsq, hatPi0, hatPi1, hatT, sh0_1, sh0_0, sh1_1, sh1_0, c, d, cutoff, progress));
    return rcpp_result_gen;
END_RCPP
}
// BSGL
Rcpp::List BSGL(arma::mat xx, arma::vec y, arma::mat W, unsigned int s, unsigned int L, int maxSteps, arma::vec hatAlpha, arma::mat hatBeta, arma::vec hatInvTauSq, arma::mat hatInvGammaSq, arma::mat invSigAlpha0, double hatLambdaSqT, double hatLambdaSqG, double hatSigmaSq, double s1, double s2, double r1, double r2, double a, double b, int progress);
RcppExport SEXP _roben_BSGL(SEXP xxSEXP, SEXP ySEXP, SEXP WSEXP, SEXP sSEXP, SEXP LSEXP, SEXP maxStepsSEXP, SEXP hatAlphaSEXP, SEXP hatBetaSEXP, SEXP hatInvTauSqSEXP, SEXP hatInvGammaSqSEXP, SEXP invSigAlpha0SEXP, SEXP hatLambdaSqTSEXP, SEXP hatLambdaSqGSEXP, SEXP hatSigmaSqSEXP, SEXP s1SEXP, SEXP s2SEXP, SEXP r1SEXP, SEXP r2SEXP, SEXP aSEXP, SEXP bSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type s(sSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type maxSteps(maxStepsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatAlpha(hatAlphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatBeta(hatBetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hatInvTauSq(hatInvTauSqSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type hatInvGammaSq(hatInvGammaSqSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invSigAlpha0(invSigAlpha0SEXP);
    Rcpp::traits::input_parameter< double >::type hatLambdaSqT(hatLambdaSqTSEXP);
    Rcpp::traits::input_parameter< double >::type hatLambdaSqG(hatLambdaSqGSEXP);
    Rcpp::traits::input_parameter< double >::type hatSigmaSq(hatSigmaSqSEXP);
    Rcpp::traits::input_parameter< double >::type s1(s1SEXP);
    Rcpp::traits::input_parameter< double >::type s2(s2SEXP);
    Rcpp::traits::input_parameter< double >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< double >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(BSGL(xx, y, W, s, L, maxSteps, hatAlpha, hatBeta, hatInvTauSq, hatInvGammaSq, invSigAlpha0, hatLambdaSqT, hatLambdaSqG, hatSigmaSq, s1, s2, r1, r2, a, b, progress));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_roben_BGLPointMass", (DL_FUNC) &_roben_BGLPointMass, 20},
    {"_roben_BGL", (DL_FUNC) &_roben_BGL, 17},
    {"_roben_BL_SS", (DL_FUNC) &_roben_BL_SS, 18},
    {"_roben_BLasso", (DL_FUNC) &_roben_BLasso, 15},
    {"_roben_BRGL_SS", (DL_FUNC) &_roben_BRGL_SS, 22},
    {"_roben_BRGL", (DL_FUNC) &_roben_BRGL, 19},
    {"_roben_BRL_SS", (DL_FUNC) &_roben_BRL_SS, 20},
    {"_roben_BRL", (DL_FUNC) &_roben_BRL, 17},
    {"_roben_BRSGL_SS", (DL_FUNC) &_roben_BRSGL_SS, 26},
    {"_roben_BRSGL", (DL_FUNC) &_roben_BRSGL, 24},
    {"_roben_BSGL_SS", (DL_FUNC) &_roben_BSGL_SS, 23},
    {"_roben_BSGL", (DL_FUNC) &_roben_BSGL, 21},
    {NULL, NULL, 0}
};

RcppExport void R_init_roben(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
