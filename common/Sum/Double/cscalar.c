#include <stdio.h>

double cdsum(double* u, int n)
{
    double* end = u + n;
    double  s = 0.0;

    while (u < end)
        s += *u++;

    return s;
}
