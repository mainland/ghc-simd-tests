#include <stdio.h>
#include <math.h>

typedef int blasint;

#include <cblas.h>

double crbf(double nu, double* u, int ul, double* v, int vl)
{
    int n = ul < vl ? ul : vl;
    double temp;
    double nrm2u;
    double nrm2v;

    nrm2u = cblas_dnrm2(n, u, 1);
    nrm2v = cblas_dnrm2(n, v, 1);

    temp = nrm2u*nrm2u -2*cblas_ddot(n, u, 1, v, 1) + nrm2v*nrm2v;
    return exp(-nu*temp);
}
