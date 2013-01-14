#include <stdio.h>

double cddotp(double* u, int ul, double* v, int vl)
{
    int    n = ul < vl ? ul : vl;
    double s = 0.0;
    int    i;

    for (i = 0; i < n; ++i)
        s += u[i] * v[i];

    return s;
}
