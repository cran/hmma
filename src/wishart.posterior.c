#include "include/rcore.h"
#include "include/blas.h"
#include "include/covariance.h"
#include "include/data.frame.h"
#include "include/matrix.h"
#include "include/scores.h"

/* posterior wishart probability for the BGe score. */
double wpost(SEXP x, double iss, double phic) {

int i = 0, n = length(x);
double mu = 0, phi = 0, tau = 0, rho = 0;
double oldtau = 0, oldmu = 0, logk = 0, logscale = 0, mscore = 0;
double res = 0, *xx = REAL(x);

  /* compute the mean and the variance of the data. */
  for (i = 0; i < n; i++)
    mu += xx[i];
  mu /= n;

  for (i = 0; i < n; i++)
    phi += (xx[i] - mu) * (xx[i] - mu);
  phi = phi / (n - 1) * phic ;

  /* set tau and rho. */
  tau = rho = iss;

  for (i = 0; i < n; i++) {

    logscale = log(phi) + log1p(1.0/tau);
    logk = lgammafn(0.5 * (1.0 + rho)) - lgammafn(rho * 0.5);
    logk -= 0.5 * (logscale + log(M_PI));
    mscore = logk - 0.5 * (rho + 1) * log1p( (xx[i] - mu) * (xx[i] - mu) / exp(logscale) );
    res += mscore;

    oldtau = tau;
    oldmu  = mu;

    tau++;
    rho++;
    mu = (oldtau * mu + xx[i]) / tau;
    phi += (xx[i] - mu) * xx[i] + (oldmu - mu) * oldtau * oldmu;

  }/*FOR*/

  return res;

}/*WPOST*/

void build_tau(double **data, covariance tau, int ncol, int nrow,
    double iss, double phi) {

int i = 0, j = 0;
double temp = 0, *mean = NULL;
covariance cov = { 0 };

  /* allocate mean vector and covariance matrix. */
  mean = Calloc1D(ncol, sizeof(double));
  cov = new_covariance(ncol, TRUE);

  /* compute the mean values.  */
  c_meanvec(data, mean, nrow, ncol, 0);

  /* compute the covariance matrix... */
  c_covmat(data, mean, nrow, ncol, cov, 0);
  /* ... multiply it by the phi coefficient... */
  for (i = 0; i < ncol; i++)
    for (j = 0; j < ncol; j++)
      cov.mat[CMC(i, j, ncol)] *= phi;

  /* ... compute the pseudoinverse... */
  c_ginv(cov, cov);

  /* ... and store it in the bottom-right corner of the tau matrix. */
  for (i = 1; i < tau.dim; i++)
    for (j = 1; j < tau.dim; j++)
      tau.mat[CMC(i, j, tau.dim)] = cov.mat[CMC(i - 1, j - 1, ncol)];

  /* fill the top-right and bottom-left corners. */
  for (i = 1; i < ncol + 1; i++) {

    temp = 0;

    for (j = 0; j < ncol; j++)
      temp += mean[j] * cov.mat[CMC(j, i - 1, ncol)];

    tau.mat[CMC(i, 0, tau.dim)] = tau.mat[CMC(0, i, tau.dim)] = -temp;

  }/*FOR*/

  /* fill the top-left corner. */
  for (i = 1; i < tau.dim; i++)
    tau.mat[CMC(0, 0, tau.dim)] += - mean[i - 1] * tau.mat[CMC(i, 0, tau.dim)];

  tau.mat[CMC(0, 0, tau.dim)] += 1 / iss;

  /* perform the final (pseudo)inversion. */
  c_ginv(tau, tau);

  Free1D(mean);
  FreeCOV(cov);

}/*BUILD_TAU*/

double cwpost(SEXP x, SEXP z, double iss, double phic) {

int i = 0, j = 0, k = 0;
int ncol = length(z), num = length(x), tau_ncol = length(z) + 1;
int rho = iss + ncol;
double logscale = 0, logk = 0, xprod = 0, var_x = 0, zi_mu = 0, phi = 0;
double *xx = REAL(x), *workspace = NULL;
double res = 0, **zz = NULL, *zi = NULL, *mu = NULL, *delta_mu = NULL;
double *old_mu = NULL;
covariance tau = { 0 }, inv_tau = { 0 }, old_tau = { 0 };

  /* allocate a workspace vector. */
  workspace = Calloc1D(tau_ncol, sizeof(double));

  /* allocate and initialize the parent configuration. */
  zi = Calloc1D(ncol + 1, sizeof(double));
  zi[0] = 1;

  /* estimate mu and var_x. */
  mu = Calloc1D(tau_ncol, sizeof(double));
  old_mu = Calloc1D(tau_ncol, sizeof(double));
  delta_mu = Calloc1D(tau_ncol, sizeof(double));

  for (i = 0; i < num; i++)
    mu[0] += xx[i];
  mu[0] /= num;

  for (i = 0; i < num; i++)
    var_x += (xx[i] - mu[0]) * (xx[i] - mu[0]);
  var_x /= num - 1;

  /* initialize phi. */
  phi = var_x * phic;

  /* allocate and initialize an array of pointers for the variables. */
  zz = (double **) Calloc1D(ncol, sizeof(double *));
  for (j = 0; j < ncol; j++)
    zz[j] = REAL(VECTOR_ELT(z, j));

  /* allocate and initialize tau. */
  tau = new_covariance(tau_ncol, TRUE);
  old_tau = new_covariance(tau_ncol, FALSE);
  inv_tau = new_covariance(tau_ncol, FALSE);
  build_tau(zz, tau, ncol, num, iss, phic);
  memcpy(old_tau.mat, tau.mat, tau_ncol * tau_ncol * sizeof(double));
  c_ginv(tau, inv_tau);

  /* for each sample... */
  for (i = 0; i < num; i++) {

    /* ... extract the values of the parents ... */
    for (j = 0; j < ncol; j++)
      zi[j + 1] = zz[j][i];

    /* ... compute the Mahalanobis distance of z[i] ... */
    xprod = c_quadratic(zi, &tau_ncol, inv_tau.mat, zi, workspace);

    /* ... compute the scale factor ... */
    logscale = log(phi) + log1p(xprod);
    logk = lgammafn(0.5 * (1 + rho)) - lgammafn(0.5 * rho);
    logk -= 0.5 * (logscale + log(M_PI));

    /* and then the score for the variable. */
    for (j = 0, zi_mu = 0; j < tau_ncol; j++)
      zi_mu += zi[j] * mu[j];

    res += logk - 0.5 * (1 + rho) *
             log1p((xx[i] - zi_mu) * (xx[i] - zi_mu) / exp(logscale));

    /* For the next iteration, update the tau matrix ... */
    memcpy(old_tau.mat, tau.mat, tau_ncol * tau_ncol * sizeof(double));

    for (j = 0; j < tau_ncol; j++)
      for (k = j; k < tau_ncol; k++)
        tau.mat[CMC(j, k, tau_ncol)] = tau.mat[CMC(k, j, tau_ncol)] =
          tau.mat[CMC(j, k, tau_ncol)] + zi[j] * zi[k];

    /* ... its inverse  ... */
    c_finv(tau, inv_tau);

    /* ... update the mu vector ... */
    memcpy(old_mu, mu, tau_ncol * sizeof(double));
    c_rotate(inv_tau.mat, old_tau.mat, mu, &(xx[i]), zi, &tau_ncol, workspace);

    /* ... update rho (ISS + sample size evaluated at the current iteration) ... */
    rho++;

    /* ... and update phi. */
    for (j = 0; j < tau_ncol; j++)
      delta_mu[j] = old_mu[j] - mu[j];
    for (j = 0, zi_mu = 0; j < tau_ncol; j++)
      zi_mu += zi[j] * mu[j];

    phi += (xx[i] - zi_mu) * xx[i] +
             c_quadratic(delta_mu, &tau_ncol, old_tau.mat, old_mu, workspace);

  }/*FOR*/

  Free1D(workspace);
  Free1D(zi);
  Free1D(mu);
  Free1D(old_mu);
  Free1D(delta_mu);
  Free1D(zz);
  FreeCOV(tau);
  FreeCOV(old_tau);
  FreeCOV(inv_tau);

  return res;

}/*CWPOST*/

double wishart_node(SEXP target, SEXP x, SEXP data, SEXP isize, SEXP phi, SEXP prior,
    SEXP beta, int debuglevel) {

double iss = NUM(isize);
int n = length(VECTOR_ELT(data, 0));
char *phi_str = (char *)CHAR(STRING_ELT(phi, 0));
char *t = (char *)CHAR(STRING_ELT(target, 0));
double prob = 0, prior_prob = 0, phi_coef = 0;
SEXP nodes, node_t, parents, parent_vars, data_t;

  /* compute the phi multiplier. */
  if (strcmp(phi_str, "bottcher") == 0)
    phi_coef = (double)(n - 1) / (double)(n) * (iss - 1);
  else
    phi_coef = (double)(n - 1) / (double)n * (iss) / (iss + 1) * (iss - 2);

  /* get the node cached information. */
  nodes = getListElement(x, "nodes");
  node_t = getListElement(nodes, t);
  /* get the parents of the node. */
  parents = getListElement(node_t, "parents");
  /* extract the node's column from the data frame. */
  PROTECT(data_t = c_dataframe_column(data, target, TRUE, FALSE));
  /* compute the prior probability component for the node. */
  prior_prob = graph_prior_prob(prior, target, beta, nodes, debuglevel);

  if (length(parents) == 0) {

    prob = wpost(data_t, iss, phi_coef);

  }/*THEN*/
  else {

    PROTECT(parent_vars = c_dataframe_column(data, parents, FALSE, FALSE));
    /* compute the marginal likelihood. */
    prob = cwpost(data_t, parent_vars, iss, phi_coef);

    UNPROTECT(1);

  }/*ELSE*/

  if (debuglevel > 0) {

    Rprintf("  > (log)prior probability is %lf.\n", prior_prob);
    Rprintf("  > (log)posterior density is %lf.\n", prob);

  }/*THEN*/

  /* add the (log)prior to the marginal (log)likelihood to get the (log)posterior. */
  prob += prior_prob;

  UNPROTECT(1);

  return prob;

}/*WISHART_NODE*/
