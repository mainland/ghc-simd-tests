#include <xmmintrin.h>

#define VECTOR_SIZE         2

typedef double v2df __attribute__ ((vector_size(sizeof(double)*VECTOR_SIZE)));

union d2v
{
  v2df   v;
  double f[2];
};

double c_vecdsum(double* u, int n)
{
    union d2v d2s = {0.0, 0.0};
    double    s;
    int       i;
    int       m = n & (~VECTOR_SIZE);

    for (i = 0; i < m; i += VECTOR_SIZE)
        d2s.v += *((v2df*) (u + i));

    s = d2s.f[0] + d2s.f[1];

    for (; i < n; ++i)
        s += u[i];

    return s;
}
