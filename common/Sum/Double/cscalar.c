#include <stdio.h>

double c_dsum(double* u, int n)
{
    double* end = u + n;
    double  s = 0.0;

    while (u < end)
        s += *u++;

    return s;
}
