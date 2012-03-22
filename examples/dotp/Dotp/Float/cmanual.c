#include <stdio.h>
#include <xmmintrin.h>

#define VECTOR_SIZE         4

typedef float v4sf __attribute__ ((vector_size(sizeof(float)*VECTOR_SIZE)));

union f4v
{
  v4sf  v;
  float f[4];
};

float cvecdotp(float* u, int ul, float* v, int vl)
{
    int       n = ul < vl ? ul : vl;
    union f4v f4s = {0.0, 0.0, 0.0, 0.0};
    float     s;
    int       i;
    int       m = n & (~VECTOR_SIZE);

    for (i = 0; i < m; i += 4)
        f4s.v += (*((v4sf*) (u + i))) * (*((v4sf*) (v + i)));

    s = f4s.f[0] + f4s.f[1] + f4s.f[2] + f4s.f[3];

    for (; i < n; ++i)
        s += u[i] * v[i];

    return s;
}
