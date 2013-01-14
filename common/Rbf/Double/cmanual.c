#include <stdio.h>
#include <math.h>

typedef int blasint;

#include <cblas.h>

double crbf(double nu, double* u, int ul, double* v, int vl)
{
    int n = ul < vl ? ul : vl;
    double temp;

    temp = cblas_ddot(n, u, 1, u, 1) -2*cblas_ddot(n, u, 1, v, 1) + cblas_ddot(n, v, 1, v, 1);
    return exp(-nu*temp);
}
