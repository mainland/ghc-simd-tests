#include <math.h>

#define BOOST_UBLAS_SHALLOW_ARRAY_ADAPTOR 1

#include <boost/numeric/ublas/traits.hpp>
#include <boost/numeric/ublas/vector.hpp>

namespace ublas = boost::numeric::ublas;

typedef ublas::shallow_array_adaptor<double>            shallow_adaptor_double;
typedef ublas::vector<double, shallow_adaptor_double>   shallow_vector_double;

extern "C" double boost_rbf(double nu, double* __restrict__ u0, int ul, double* __restrict__ v0, int vl)
{
    int                    n = ul < vl ? ul : vl;
    shallow_adaptor_double u_adapt(n, u0);
    shallow_vector_double  u(n, u_adapt);
    shallow_adaptor_double v_adapt(n, v0);
    shallow_vector_double  v(n, v_adapt);
    ublas::vector<double>  z = u - v;

    return exp(-nu*prec_inner_prod(z, z));
}
