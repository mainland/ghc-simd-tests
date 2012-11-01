#include <stdio.h>

float csum(float* u, int n)
{
    float* end = u + n;
    float  s = 0.0;

    while (u < end)
        s += *u++;

    return s;
}
