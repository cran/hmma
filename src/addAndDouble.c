#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

SEXP addAndDouble(SEXP x_, SEXP y_) {
  double x = asReal(x_);
  double y = asReal(y_);

  double sum = x + y + y + x;

  return ScalarReal(sum);
}
