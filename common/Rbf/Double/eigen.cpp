#include <math.h>

#include <Eigen/Core>
#include <Eigen/Dense>

using namespace Eigen;

template<class T>
double norm2(T const& x)
{
    return x.dot(x);
}

extern "C" double eigen_rbf(double nu, double* __restrict__ u0, int ul, double* __restrict__ v0, int vl)
{
    int                   n = ul < vl ? ul : vl;
    Map<VectorXd,Aligned> u(u0,n);
    Map<VectorXd,Aligned> v(v0,n);

    return exp(-nu*(u-v).squaredNorm());
}

extern "C" double eigen_rbf_abs(double nu, double* __restrict__ u0, int ul, double* __restrict__ v0, int vl)
{
    int                   n = ul < vl ? ul : vl;
    Map<VectorXd,Aligned> u(u0,n);
    Map<VectorXd,Aligned> v(v0,n);

    return exp(-nu*((u-v).dot(u-v)));
}
