#include <math.h>

#include <iostream>
#include <blitz/array.h>

using namespace blitz;

extern "C" double blitz_rbf(double nu, double* __restrict__ u0, int ul, double* __restrict__ v0, int vl)
{
    int             n = ul < vl ? ul : vl;
    Array<double,1> u(u0, shape(n), neverDeleteData);
    Array<double,1> v(v0, shape(n), neverDeleteData);

    return exp(-nu*dot(u-v, u-v));
}
