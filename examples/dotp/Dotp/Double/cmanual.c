#include <stdio.h>
#include <xmmintrin.h>

#define VECTOR_SIZE         2

typedef double v2sd __attribute__ ((vector_size(sizeof(double)*VECTOR_SIZE)));

union d2v
{
  v2sd   v;
  double d[VECTOR_SIZE];
};

double cdvecdotp(double* u, int ul, double* v, int vl)
{
    int       n = ul < vl ? ul : vl;
    union d2v d2s = {0.0, 0.0};
    double    s;
    int       i;
    int       m = n & (~VECTOR_SIZE);

    for (i = 0; i < m; i += VECTOR_SIZE)
        d2s.v += (*((v2sd*) (u + i))) * (*((v2sd*) (v + i)));

    s = d2s.d[0] + d2s.d[1];

    for (; i < n; ++i)
        s += u[i] * v[i];

    return s;
}
