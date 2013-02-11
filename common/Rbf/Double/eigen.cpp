#include <math.h>

#include <Eigen/Core>
#include <Eigen/Dense>

using namespace Eigen;

//#if !defined(__GNU__)
#define __builtin_assume_aligned(x,y) x
//#endif

extern "C" double eigen_rbf(double nu, double* __restrict__ u0, int ul, double* __restrict__ v0, int vl)
{
    int           n = ul < vl ? ul : vl;
    Map<VectorXd> u((double*) __builtin_assume_aligned(u0,16),n);
    Map<VectorXd> v((double*) __builtin_assume_aligned(v0,16),n);

    return exp(-nu*(u-v).squaredNorm());
}

extern "C" double eigen_rbf2(double nu, double* __restrict__ u0, int ul, double* __restrict__ v0, int vl)
{
    int           n = ul < vl ? ul : vl;
    Map<VectorXd> u((double*) __builtin_assume_aligned(u0,16),n);
    Map<VectorXd> v((double*) __builtin_assume_aligned(v0,16),n);
    VectorXd      temp = u - v;

    return exp(-nu*(temp.dot(temp)));
}
