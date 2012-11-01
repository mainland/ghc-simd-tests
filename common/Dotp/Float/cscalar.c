#include <stdio.h>

float cdotp(float* u, int ul, float* v, int vl)
{
    int    n = ul < vl ? ul : vl;
    float  s = 0.0;
    int    i;

    for (i = 0; i < n; ++i)
        s += u[i] * v[i];

    return s;
}
