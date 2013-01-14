#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

typedef int blasint;

#include <cblas.h>

double crbf_intermediate(double nu, double* u, int ul, double* y, int yl)
{
    int n = ul < yl ? ul : yl;
    double* x;
    double temp;

    x = malloc(n*sizeof(double));
    assert(x != NULL);
    memcpy(x, u, n*sizeof(double));

    cblas_daxpy(n, -1, y, 1, x, 1);
    temp = cblas_dnrm2(n, x, 1);

    free(x);

    return exp(-nu*temp*temp);
}
